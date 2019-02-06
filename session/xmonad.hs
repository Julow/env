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
import XMonad.Layout.Minimize
import XMonad.Layout.ResizableTile
import XMonad.Hooks.ManageHelpers
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import qualified Data.Map as M


-- ========================================================================== --
-- Wallpaper

wallpaper = "~/.xmonad/bg.jpg"

init_wallpaper = spawn ("display -window root " ++ wallpaper)

-- ========================================================================== --
-- Lock screen

screen_timeout = 500
screen_timeout_locked = 20

set_dpms timeout = "xset dpms 0 0 " ++ show timeout

init_lock_screen = spawn (set_dpms screen_timeout)

init_xmodmap = "xmodmap -display :0 ~/.xmodmap"

lock_screen = spawn (
		(set_dpms screen_timeout_locked) ++ ";"
		++ "slock;"
		++ (set_dpms screen_timeout) ++ ";"
		++ init_xmodmap
	)

-- ========================================================================== --
-- Audio

current_sink () = do
	outp <- runProcessWithInput "pactl" ["list", "short", "sinks"] ""
	let running = head $ filter (isSuffixOf "RUNNING") $ lines outp
	return $ head (words running)

pactl cmd arg = do
	sink <- current_sink ()
	safeSpawn "pactl" [cmd, sink, arg]

volume_up = pactl "set-sink-volume" "+5%"
volume_down = pactl "set-sink-volume" "-5%"
volume_toggle = pactl "set-sink-mute" "toggle"

audio_prev = safeSpawn "playerctl" ["previous"]
audio_next = safeSpawn "playerctl" ["next"]

audio_toggle = spawnOn "9" "playerctl play-pause || spotify.sh"

-- ========================================================================== --
-- Browser

web_browser = "firefox"

-- ========================================================================== --
-- Screenshot

take_screenshot = safeSpawn "screenshot.sh" ["screen"]
take_screenshot_interactive = safeSpawn "screenshot.sh" ["interactive"]

-- ========================================================================== --
-- Brightness

change_brightness inc = safeSpawn "brightness.sh" [ show inc ]

-- ========================================================================== --
-- Preset prompt
-- Prompt to execute a shell script located in the ~/.presets directory

data Preset = Preset

instance XPrompt Preset where
	showXPrompt Preset = "Preset: "
	commandToComplete _ c = c
	nextCompletion _ = getNextCompletion

preset_prompt prompt_conf = do
	home <- io $ getEnv "HOME"
	let preset_dir = home ++ "/.presets/"
	ws <- io $ getDirectoryContents preset_dir
	let ws' = filter (flip notElem [ ".", ".." ]) ws
	let open w = spawn ("source \"" ++ preset_dir ++ w ++ "\"")
	mkXPrompt Preset prompt_conf (mkComplFunFromList' ws') open

-- ========================================================================== --
-- Window prompt
-- Show the list of windows, sorted by workspace
-- Similar to Xmonad.Prompt.Window

data WindowPrompt = WindowPrompt

instance XPrompt WindowPrompt where
	showXPrompt WindowPrompt = "Windows: "
	commandToComplete _ c = c
	nextCompletion _ = getNextCompletion

window_title ws w = do
	name <- show <$> getName w
	let tag = W.tag ws
	return ("[" ++ tag ++ "] " ++ name)

window_prompt prompt_conf = do
	wm <- windowMap' window_title
	let compl = mkComplFunFromList' (M.keys wm)
	let action = flip whenJust (windows . W.focusWindow) . flip M.lookup wm
	mkXPrompt WindowPrompt prompt_conf compl action

-- ========================================================================== --
-- Floating status window

status_window_name = "floating-status-window"

status_window_hook =
	appName =? status_window_name --> doCenterFloat

spawn_status_window =
	safeSpawn "xterm" ["-name", status_window_name, "-geom", "32x3", "-e", "status.sh"]

-- ========================================================================== --
-- main

prompt_conf = def
	{
		font = "xft:",
		promptBorderWidth = 0,
		height = 22,
		position = CenteredAt 0.5 0.5,
		promptKeymap = foldl (\m (k, a) -> M.insert k a m) emacsLikeXPKeymap [
			((controlMask, xK_w), killWord Prev),
			((controlMask, xK_Left), moveWord Prev),
			((controlMask, xK_Right), moveWord Next)
		]
	}

on_start = do
	init_lock_screen
	init_wallpaper
	lock_screen

tiled_layout =
	minimize (boringWindows (ResizableTall 1 (5/100) (1/2) []))

main =
	xmonad $ def
	{
		focusFollowsMouse = False,
		borderWidth = 0,
		startupHook = on_start,
		logHook = updatePointer (0.99, 0.001) (0, 0),
		layoutHook = tiled_layout ||| Full,
		manageHook = manageSpawn <+> status_window_hook <+> manageHook def,
		terminal = "xterm tmux"
	} `additionalKeysP`
	[

		("M-S-l",					sendMessage MirrorShrink),
		("M-S-h",					sendMessage MirrorExpand),

		-- BoringWindows
		("M-S-<Tab>",				focusUp),
		("M-<Tab>",					focusDown),
		("M-m",						markBoring),
		("M-S-m",					clearBoring),

		("M-d",						withFocused minimizeWindow),
		("M-S-d",					withLastMinimized maximizeWindowAndFocus),

		("<XF86AudioLowerVolume>",	volume_down),
		("<XF86AudioRaiseVolume>",	volume_up),
		("<XF86AudioMute>",			volume_toggle),
		("<XF86AudioPlay>",			audio_toggle),
		("<XF86AudioPrev>",			audio_prev),
		("<XF86AudioNext>",			audio_next),

		("<XF86MonBrightnessUp>",	change_brightness 10),
		("<XF86MonBrightnessDown>",	change_brightness (-10)),

		("M-z",						lock_screen),

		("M-S-<Backspace>",			safeSpawnProg web_browser),

		("M-p",						shellPrompt prompt_conf),
		("M-S-p",					window_prompt prompt_conf),
		("M-S-o",					preset_prompt prompt_conf),

		("M-o",						spawn_status_window),

		("M-S-s",					take_screenshot),
		("M-s",						take_screenshot_interactive)

	] `removeKeysP`
	[
		"M-S-q"
	]
