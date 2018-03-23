import Control.Monad
import XMonad
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Layout.ResizableTile
import XMonad.Actions.UpdatePointer
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run

-- ========================================================================== --
-- Wallpaper

wallpaper = "~/.xmonad/bg.jpg"

init_wallpaper = spawn ("display -window root " ++ wallpaper)

-- ========================================================================== --
-- Lock screen

screen_timeout = 500
screen_timeout_locked = 20
wallpaper_locked = "~/.xmonad/bg_locked.png"

set_dpms timeout = "xset dpms 0 0 " ++ show timeout

init_lock_screen = spawn (set_dpms screen_timeout)

init_xmodmap = "xmodmap -display :0 ~/.xmodmap"

lock_screen = spawn (
		(set_dpms screen_timeout_locked) ++ ";"
		++ "slock -i " ++ wallpaper_locked ++ ";"
		++ (set_dpms screen_timeout) ++ ";"
		++ init_xmodmap
	)

-- ========================================================================== --
-- Volume

current_sink = "$(pactl list short | grep RUNNING | cut -f1 | head -n1)"

volume_up = spawn ("pactl set-sink-volume " ++ current_sink ++ " +10%")
volume_down = spawn ("pactl set-sink-volume " ++ current_sink ++ " -10%")
volume_toggle = spawn ("pactl set-sink-mute " ++ current_sink ++ " toggle")

-- ========================================================================== --
-- Browser

web_browser = "/opt/firefox/firefox"

-- ========================================================================== --
-- Screenshot

take_screenshot = spawn "screenshot.sh screen"
take_screenshot_interactive = spawn "screenshot.sh interactive"

-- ========================================================================== --
-- main


on_start = do
	init_lock_screen
	init_wallpaper
	spawn "pulseaudio --start &"
	lock_screen

main =
	xmonad $ defaultConfig
	{
		focusFollowsMouse = False,
		borderWidth = 0,
		startupHook = on_start,
		logHook = updatePointer (0.99, 0.001) (0, 0),
		layoutHook =
			let tiled = ResizableTall 1 (5/100) (1/2) [] in
			tiled ||| Full,
		terminal = "x-terminal-emulator tmux"
	} `additionalKeysP`
	[

		("M-S-l",					sendMessage MirrorShrink),
		("M-S-h",					sendMessage MirrorExpand),

		("<XF86AudioLowerVolume>",	volume_down),
		("<XF86AudioRaiseVolume>",	volume_up),
		("<XF86AudioMute>",			volume_toggle),

		("M-z",						lock_screen),

		("M-S-<Backspace>",			safeSpawnProg web_browser),

		("M-p",						shellPrompt def),

		("M-S-s",					take_screenshot),
		("M-s",						take_screenshot_interactive)

	]
