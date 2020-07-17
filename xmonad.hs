{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
import Control.Monad
import Data.List
import System.Directory
import System.Environment
import XMonad
import XMonad.Actions.Minimize
import XMonad.Actions.SpawnOn
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowBringer
import XMonad.Layout.BoringWindows
import XMonad.Layout.Decoration
import XMonad.Layout.Spacing
import XMonad.Layout.Minimize
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Tabbed
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
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

  pureDecoration BorderBetween dw dh (Rectangle _ _ sw sh) _ _ (_, Rectangle x y w h)
    | fi x+w < sw = Just $ Rectangle (x + fi (w - dw)) y dw h -- Right
    | fi y+h < sh = Just $ Rectangle x (y + fi (h - dh)) w dh -- Bottom
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
-- Prompt to open a workspace (in ~/notes/workspaces/) using workspaces.sh

workspace_prompt prompt_conf = do
  home <- home_dir
  let workspaces_dir = home ++ "/notes/workspaces/"
  let workspaces_sh = home ++ "/notes/setup/tools/workspaces.sh"
  ws <- io $ getDirectoryContents workspaces_dir
  let ws' = filter (flip notElem [ ".", ".." ]) ws
  let compl = compl_fun_from_list ws'
  let open w =
        let w' = workspaces_dir ++ w in
        safeSpawn workspaces_sh [ "open", "-s", w' ]
  mkXPrompt (Prompt_autocomplete "Workspace: ") prompt_conf compl open

-- ========================================================================== --
-- Centered layout
-- Improve Layout.Spacing by handling Shink and Expand messages

data CenteredLayout a = CenteredLayout Integer deriving (Show, Read)

instance LayoutModifier CenteredLayout a where
  handleMess (CenteredLayout step) m
    | Just Shrink <- fromMessage m = modify_lr (-step)
    | Just Expand <- fromMessage m = modify_lr step
    | otherwise = return Nothing
    where
      modify_lr d = do
        let m (Border t b r l) = Border t b (r + d) (l + d)
        _ <- sendMessage (ModifyWindowBorder m)
        return Nothing

centered_full sp step =
  ModifiedLayout (CenteredLayout step) $
  spacingRaw False (Border 0 0 0 0) False (Border 0 0 sp sp) True $
  Full

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

-- Decoration with invisible text and no border
set_colors_no_text active inactive urgent =
  def {
    fontName = font_name 0,
    activeColor = active,
    inactiveColor = inactive,
    urgentColor = urgent,
    activeBorderColor = active,
    inactiveBorderColor = inactive,
    urgentBorderColor = urgent,
    activeTextColor = active,
    inactiveTextColor = inactive,
    urgentTextColor = urgent
  }

tabbed_conf =
  (set_colors_no_text "#859900" "#eeeada" "#dc322f") {
    decoHeight = 2
  }

border_conf =
  (set_colors_no_text "#2878c8" "#000000" "#dc322f") {
    decoWidth = 1,
    decoHeight = 1
  }

layout =
    border_between $ add_tabs $ minimize $ boringWindows $
      (tiled_layout ||| centered_layout)
  where
    tiled_layout = ResizableTall 1 (5/100) (1/2) []
    centered_layout = centered_full 600 20
    add_tabs = addTabs shrinkText tabbed_conf . subLayout [] Simplest
    border_between = decoration shrinkText border_conf BorderBetween

main =
  xmonad $ ewmh def
  {
    focusFollowsMouse = False,
    borderWidth = 0,
    logHook = updatePointer (0.5, 0.5) (0, 0),
    layoutHook = layout,
    manageHook = manageSpawn <+> manageHook def,
    handleEventHook = handleEventHook def <+> fullscreenEventHook,
    terminal = "xterm"
  } `additionalKeysP`
  [

    -- Windows management

    -- SubTabbed merge/unmerge
    ("M-u", withFocused (\w -> focusDown >> sendMessage (mergeDir id w))),
    ("M-S-u", withFocused (sendMessage . UnMerge)),
    -- SubTabbed next/prev
    ("M-j", onGroup W.focusUp'),
    ("M-k", onGroup W.focusDown'),

    -- BoringWindows
    ("M-S-<Tab>", focusUp),
    ("M-<Tab>", focusDown),
    ("M-m", markBoring),
    ("M-S-m", clearBoring),

    -- Minimize/restore
    ("M-d", withFocused minimizeWindow),
    ("M-S-d", withLastMinimized maximizeWindowAndFocus),

    -- Shrink/expand vertically
    ("M-S-l", sendMessage MirrorShrink),
    ("M-S-h", sendMessage MirrorExpand),

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

    -- Indicators
    ("M-`", safeSpawn "indicators.sh" []),

    -- Small features

    -- Screenshot drag/whole screen
    ("M-s", safeSpawn "screenshot.sh" ["interactive"]),
    ("M-S-s", safeSpawn "screenshot.sh" ["screen"]),

    -- Volume keys
    ("<XF86AudioLowerVolume>", safeSpawn "volume.sh" ["-5%"]),
    ("<XF86AudioRaiseVolume>", safeSpawn "volume.sh" ["+5%"]),
    -- Media keys
    ("<XF86AudioMute>", safeSpawn "volume.sh" ["toggle"]),
    ("<XF86AudioPlay>", spawnOn "9" "playerctl play-pause || spotify.sh"),
    ("<XF86AudioPrev>", safeSpawn "playerctl" ["previous"]),
    ("<XF86AudioNext>", safeSpawn "playerctl" ["next"]),

    -- Backlight keys
    ("<XF86MonBrightnessUp>", safeSpawn "brightness.sh" ["5"]),
    ("<XF86MonBrightnessDown>", safeSpawn "brightness.sh" ["-5"])

  ] `removeKeysP`
  [
    "M-S-q"
  ]
