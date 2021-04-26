{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
import Control.Monad
import Data.List
import Data.Ratio ((%))
import System.Directory
import System.Environment
import XMonad
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)
import XMonad.Actions.FloatKeys
import XMonad.Actions.Minimize
import XMonad.Actions.WindowBringer
import XMonad.Layout.BoringWindows
import XMonad.Layout.Decoration
import XMonad.Layout.Spacing
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Tabbed
import XMonad.ManageHook
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run
import XMonad.Util.Types
import XMonad.Hooks.EwmhDesktops
import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- ========================================================================== --
-- Utils

data Prompt_autocomplete = Prompt_autocomplete String

instance XPrompt Prompt_autocomplete where
  showXPrompt (Prompt_autocomplete prompt) = prompt
  commandToComplete _ c = c
  nextCompletion _ = getNextCompletion

home_dir = io $ getEnv "HOME"

-- Fuzzy completion
compl_fun_from_list :: [String] -> String -> IO [String]
compl_fun_from_list lst s =
  return $ fuzzySort s (filter (fuzzyMatch s) lst)

-- Don't match anything if query is empty
compl_no_empty _ "" = return []
compl_no_empty f s = f s

-- The dimentions of the screen a floating window is on
current_screen_size = do
  screen <- W.current <$> gets windowset
  let Rectangle _ _ sw sh = screenRect $ W.screenDetail screen
  return (sw, sh)

-- ========================================================================== --
-- Lock screen

lock_screen = spawn "light-locker-command -l"

-- ========================================================================== --
-- Browser

web_browser = "firefox"

-- ========================================================================== --
-- Border between
-- Put a decoration on the right *or* the bottom, except if it's next to the
-- edge of the screen.
-- Show one border per window, except for the bottom-right most window.

data BorderBetween a = BorderBetween deriving (Show, Read)

instance Eq a => DecorationStyle BorderBetween a where

  shrink BorderBetween (Rectangle dx dy dw dh) (Rectangle x y w h)
    | dw >= dh && dy < y + div (fi h) 2 = -- Top
      let y' = dy + fi dh in
      Rectangle x y' w (fi $ y + fi h - y')
    | dw >= dh = -- Bottom
      Rectangle x y w (fi $ dy - y)
    | dw < dh && dx < x + div (fi w) 2 = -- Left
      let x' = dx + fi dw in
      Rectangle x' y (fi $ x + fi w - x') h
    | dw < dh = -- Right
      Rectangle x y (fi $ dx - x) h

  pureDecoration BorderBetween dw dh (Rectangle sx sy sw sh) stack _ (win, Rectangle x y w h)
    | fi x+w < fi sx+sw = Just $ Rectangle (x + fi (w - dw)) y dw h -- Right
    | fi y+h < fi sy+sh = Just $ Rectangle x (y + fi (h - dh)) w dh -- Bottom
    | otherwise = Nothing

-- ========================================================================== --
-- Password prompt
-- Ask the password manager

data XPrompt_autocomplete = XPrompt_autocomplete String ComplFunction (String -> X ())

-- XPrompt instance suitable for mkXPromptWithModes
-- Example: @mkXPromptWithModes [ XPT (XPrompt_autocomplete prompt compl action) ]@
instance XPrompt XPrompt_autocomplete where
  showXPrompt (XPrompt_autocomplete prompt _ _) = prompt
  commandToComplete _ c = c
  nextCompletion _ = getNextCompletion
  completionFunction (XPrompt_autocomplete _ compl _) = compl
  modeAction (XPrompt_autocomplete _ _ action) _ q = action q

password_prompt prompt_conf = do
  home <- home_dir
  let pass_dir = home ++ "/notes/pass/"
  let pass_script = home ++ "/notes/_pass.sh"
  ps <- io $ getDirectoryContents pass_dir
  let ps' = filter (not . isPrefixOf ".") ps
  let compl = compl_no_empty $ compl_fun_from_list ps'
  let type_password p = safeSpawn pass_script [ "type", "-n", pass_dir ++ p ]
  let copy_password p = safeSpawn pass_script [ "get", "-n", pass_dir ++ p ]
  mkXPromptWithModes [
      (XPT (XPrompt_autocomplete "Type Password: " compl type_password)),
      (XPT (XPrompt_autocomplete "Copy Password: " compl copy_password))
    ] prompt_conf

-- ========================================================================== --
-- Preset prompt
-- Prompt to execute a shell script located in the ~/.presets directory

preset_prompt prompt_conf = do
  home <- home_dir
  let preset_dir = home ++ "/.presets/"
  ws <- io $ getDirectoryContents preset_dir
  let ws' = filter (flip notElem [ ".", ".." ]) ws
  let compl = compl_fun_from_list ws'
  let open w = spawn ("source \"" ++ preset_dir ++ w ++ "\"")
  mkXPrompt (Prompt_autocomplete "Preset: ") prompt_conf compl open

-- ========================================================================== --
-- Window prompt
-- Show the list of windows, sorted by workspace
-- Similar to Xmonad.Prompt.Window

window_title ws w = do
  name <- show <$> getName w
  let tag = W.tag ws
  return ("[" ++ tag ++ "] " ++ name)

window_prompt prompt_conf = do
  wm <- windowMap' window_title
  let compl = compl_fun_from_list (M.keys wm)
  let action = flip whenJust (windows . W.focusWindow) . flip M.lookup wm
  mkXPrompt (Prompt_autocomplete "Windows: ") prompt_conf compl action

-- ========================================================================== --
-- Workspace prompt

workspace_prompt prompt_conf = do
  home <- home_dir
  let workspaces_desc = home ++ "/notes/workspaces/workspaces.nix"
  ws <- io $ runProcessWithInput "workspaces" ["list", "-f", workspaces_desc] []
  let ws' = lines ws
  let compl = compl_fun_from_list ws'
  let open w = safeSpawn "xterm" ["-e", "workspaces", "open", "-f", workspaces_desc, w]
  mkXPrompt (Prompt_autocomplete "Workspace: ") prompt_conf compl open

-- ========================================================================== --
-- Centered layout
-- Improve Layout.Spacing by handling Shink and Expand messages

data CenteredLayout a = CenteredLayout Rational deriving (Show, Read)

instance LayoutModifier CenteredLayout a where
  handleMess (CenteredLayout step) m
    | Just Shrink <- fromMessage m = modify_lr (-step)
    | Just Expand <- fromMessage m = modify_lr step
    | otherwise = return Nothing
    where
      modify_lr step = do
        (sw, _) <- current_screen_size
        let d = truncate (toRational sw * step / 2)
        let m (Border t b r l) = Border t b (r + d) (l + d)
        _ <- sendMessage (ModifyWindowBorder m)
        return Nothing

centered_full sp step =
  ModifiedLayout (CenteredLayout step) $
  spacingRaw False (Border 0 0 0 0) False (Border 0 0 sp sp) True $
  Full

-- ========================================================================== --
-- Scratchpads

scratch_prog key cmd selector manageHook id =
  (key, NS id cmd selector manageHook)

scratch_xterm key cmd manageHook id =
  let xterm_cmd = "xterm -xrm 'xterm*allowTitleOps: false' -T '" ++ id ++ "' -e '" ++ cmd ++ "'" in
  let selector = title =? id in
  scratch_prog key xterm_cmd selector manageHook id

scratchpads = [
    scratch_prog "p" "pavucontrol" (className =? "Pavucontrol") (floating_centered (1/4) (1/8)),
    scratch_xterm "h" "htop" (floating_centered (1/8) (1/8)),
    scratch_xterm "t" "bash" (floating_centered (1/4) 0), -- Quick terminal
    scratch_xterm "b" "bluetoothctl" (floating_centered (1/3) (1/8)),
    scratch_xterm "w" "vim ~/quick_notes" (floating (2/3) (1/6) (1/3 - 1/10) (4/6)),
    scratch_prog "i" "com.spotify.Client" (className =? "spotify") nonFloating,
    scratch_prog "o" "com.slack.Slack" (className =? "slack") nonFloating,
    scratch_xterm "m" "mail_client" nonFloating
  ]
  where
    floating_centered x y = customFloating $ W.RationalRect x y (1 - x*2) (1 - y*2)
    floating x y w h = customFloating $ W.RationalRect x y w h

(scratchpads_actions, scratchpads_manageHooks) = (actions, hooks)
  where
    hooks = namedScratchpadManageHook nss
    (actions, nss) = unzip (zipWith make_id scratchpads [0..])
    make_id scrtch i =
      let id = show i in
      let (key, ns) = scrtch id in
      (("M-a " ++ key, namedScratchpadAction nss id), ns)

-- ========================================================================== --
-- Resize floating windows

-- Resize the focused floating window by step
resize_float_x step =
  withFocused $ \w -> do
    (sw, _) <- current_screen_size
    keysResizeWindow ((truncate (step * toRational sw)), 0) (1%2, 1%2) w

resize_float_y step =
  withFocused $ \w -> do
    (_, sh) <- current_screen_size
    keysResizeWindow (0, truncate (step * toRational sh)) (1%2, 1%2) w

-- Returns [True] if the current window is floating
current_is_floating = do
  wins <- gets windowset
  return $
    case W.peek wins of
      Just w -> M.member w (W.floating wins)
      Nothing -> False

tiled_or_float if_tiled if_float =
  current_is_floating >>= \floating ->
  if floating then if_float else if_tiled

-- ========================================================================== --
-- main

font_name size = "xft:Fira Code:style=Medium:antialias=true:size=" ++ show size

prompt_conf = def {
  font = font_name 12,
  promptBorderWidth = 0,
  height = 22,
  position = CenteredAt 0.5 0.5,
  promptKeymap = foldl (\m (k, a) -> M.insert k a m) emacsLikeXPKeymap [
    ((controlMask, xK_w), killWord Prev),
    ((controlMask, xK_Left), moveWord Prev),
    ((controlMask, xK_Right), moveWord Next)
  ],
  searchPredicate = fuzzyMatch -- This is not used for prompt because not upstreamed, see compl_fun_from_list
  -- , sorter = fuzzySort
}

border_width = 2
active_color = "#e33e14"
inactive_color = "#000000"
urgent_color = "#dc322f"

border_conf =
  def {
    fontName = font_name 0,
    activeColor = active_color,
    inactiveColor = inactive_color,
    urgentColor = urgent_color,
    activeBorderColor = active_color,
    inactiveBorderColor = inactive_color,
    urgentBorderColor = urgent_color,
    activeTextColor = active_color,
    inactiveTextColor = inactive_color,
    urgentTextColor = urgent_color,
    decoWidth = border_width,
    decoHeight = border_width
  }

resize_step = 3%100

layout =
    noBorders $ border_between $ minimize $ boringWindows $
      (tiled_layout ||| centered_layout)
  where
    tiled_layout = ResizableTall 1 resize_step (1/2) []
    centered_layout = centered_full 600 resize_step
    border_between = decoration shrinkText border_conf BorderBetween

copy_rect = W.RationalRect (2%3 - 1%20) (2%3 - 1%20) (1%3) (1%3)

manageHooks =
  [ manageHook def
  , liftX current_is_floating --> hasBorder True -- Borders around floating windows
  , title =? "Slack | mini panel" --> doIgnore -- Hide slack's minipanel
  , scratchpads_manageHooks
  ]

main =
  xmonad $ ewmh def
  {
    focusFollowsMouse = False,
    borderWidth = border_width,
    focusedBorderColor = active_color,
    normalBorderColor = inactive_color,
    layoutHook = layout,
    manageHook = composeAll manageHooks,
    handleEventHook = handleEventHook def <+> fullscreenEventHook,
    terminal = "xterm"
  }
  `additionalKeysP` ([

    -- BoringWindows
    ("M-S-<Tab>", focusUp),
    ("M-<Tab>", focusDown),
    ("M-m", markBoring),
    ("M-S-m", clearBoring),

    -- Minimize/restore
    ("M-d", withFocused minimizeWindow),
    ("M-S-d", withLastMinimized maximizeWindowAndFocus),

    -- Float and copy to every workspaces
    ("M-i", windows $ \ws ->
        case W.peek ws of
          Just w -> copyToAll (W.float w copy_rect ws)
          Nothing -> ws),
    ("M-S-i", killAllOtherCopies),

    -- Shrink/expand vertically
    ("M-h", tiled_or_float (sendMessage Shrink) (resize_float_x resize_step)),
    ("M-l", tiled_or_float (sendMessage Expand) (resize_float_x (-resize_step))),
    ("M-S-l", tiled_or_float (sendMessage MirrorShrink) (resize_float_y resize_step)),
    ("M-S-h", tiled_or_float (sendMessage MirrorExpand) (resize_float_y (-resize_step))),

    -- Control

    -- Lock screen
    ("M-z", lock_screen),

    -- Web browser
    ("M-S-<Backspace>", safeSpawnProg web_browser),

    -- Shell, window, preset prompts
    ("M-p", shellPrompt prompt_conf),
    ("M-S-p", window_prompt prompt_conf),
    ("M-o", workspace_prompt prompt_conf),
    ("M-S-o", preset_prompt prompt_conf),

    -- Password prompt
    ("M-;", password_prompt (prompt_conf { changeModeKey = xK_semicolon })),

    -- Dunst shortcuts
    ("M-r", safeSpawn "dunstctl" ["close"]),
    ("M-S-r", safeSpawn "dunstctl" ["close-all"]),
    ("M-S-`", safeSpawn "dunstctl" ["history-pop"]),
    ("M-S-:", safeSpawn "dunstctl" ["action"]),

    -- Indicators
    ("M-`", safeSpawn "indicators.sh" []),

    -- Small features

    -- Screenshot drag/whole screen
    ("M-s", safeSpawn "screenshot.sh" ["interactive"]),
    ("M-S-s", safeSpawn "screenshot.sh" ["screen"]),

    -- Volume keys
    ("<XF86AudioLowerVolume>", safeSpawn "volume.sh" ["dec"]),
    ("<XF86AudioRaiseVolume>", safeSpawn "volume.sh" ["inc"]),
    -- Media keys
    ("<XF86AudioMute>", safeSpawn "volume.sh" ["toggle"]),
    ("<XF86AudioPlay>", safeSpawn "bash" ["play-pause.sh"]),
    ("<XF86AudioPrev>", safeSpawn "playerctl" ["previous"]),
    ("<XF86AudioNext>", safeSpawn "playerctl" ["next"]),

    -- Backlight keys
    ("<XF86MonBrightnessUp>", safeSpawn "brightness.sh" ["5"]),
    ("<XF86MonBrightnessDown>", safeSpawn "brightness.sh" ["-5"])

    -- Autorandr
    , ("M-a r", safeSpawn "autorandr" ["--change"])
  ] ++ scratchpads_actions) -- Can't use `additionalKeysP` again because that would shadow the previous M-a submap
  `removeKeysP` [
    "M-S-q"
  ]
