import System.Directory
import System.Environment
import Control.Monad
import Data.List
import XMonad
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Layout.BoringWindows
import XMonad.Layout.Minimize
import XMonad.Layout.ResizableTile
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Minimize
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.Run

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

audio_toggle = spawnOn "9"
	"playerctl play-pause || (spotify & sleep 3. && playerctl play)"

-- ========================================================================== --
-- Browser

web_browser = "firefox"

-- ========================================================================== --
-- Screenshot

take_screenshot = safeSpawn "screenshot.sh" ["screen"]
take_screenshot_interactive = safeSpawn "screenshot.sh" ["interactive"]

-- ========================================================================== --
-- main

shell_conf = def
	{
		font = "xft:",
		promptBorderWidth = 0,
		height = 22,
		position = CenteredAt 0.5 0.5
	}

on_start = do
	init_lock_screen
	init_wallpaper
	lock_screen

tiled_layout =
	minimize (boringWindows (ResizableTall 1 (5/100) (1/2) []))

-- Preset prompt
-- Prompt to execute a shell script located in the ~/.presets directory

data Preset = Preset

instance XPrompt Preset where
	showXPrompt Preset = "Preset: "

preset_prompt = do
	home <- io $ getEnv "HOME"
	let preset_dir = home ++ "/.presets/"
	ws <- io $ getDirectoryContents preset_dir
	let ws' = filter (flip notElem [ ".", ".." ]) ws
	let open w = spawn ("source \"" ++ preset_dir ++ w ++ "\"")
	mkXPrompt Preset shell_conf (mkComplFunFromList' ws') open

main =
	xmonad $ def
	{
		focusFollowsMouse = False,
		borderWidth = 0,
		startupHook = on_start,
		logHook = updatePointer (0.99, 0.001) (0, 0),
		layoutHook = tiled_layout ||| Full,
		manageHook = manageSpawn <+> manageHook def,
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

		("M-z",						lock_screen),

		("M-S-<Backspace>",			safeSpawnProg web_browser),

		("M-p",						shellPrompt shell_conf),
		("M-S-p",					windowPrompt shell_conf Goto allWindows),
		("M-S-o",					preset_prompt),

		("M-S-s",					take_screenshot),
		("M-s",						take_screenshot_interactive)

	] `removeKeysP`
	[
		"M-S-q"
	]
