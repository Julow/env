import XMonad
import XMonad.Layout.ResizableTile
import XMonad.Actions.UpdatePointer
import XMonad.Util.EZConfig (additionalKeysP)

-- ========================================================================== --
-- Lock screen

init_lock_screen = spawn "xset dpms 0 0 60"
lock_screen = spawn "xset dpms 0 0 2; ( slock ; xset dpms 0 0 60 ) &"

-- ========================================================================== --
-- Volume

current_sink = "$(pactl list short | grep RUNNING | cut -f1 | head -n1)"

volume_up = spawn ("pactl set-sink-volume " ++ current_sink ++ " +10%")
volume_down = spawn ("pactl set-sink-volume " ++ current_sink ++ " -10%")
volume_toggle = spawn ("pactl set-sink-mute " ++ current_sink ++ " toggle")

-- ========================================================================== --
-- Chrome

spawn_chrome = spawn "google-chrome-stable"

-- ========================================================================== --
-- main

on_start :: X()
on_start = do
	init_lock_screen

main =
	xmonad $ defaultConfig
	{
		focusFollowsMouse = False,
		borderWidth = 0,
		startupHook = on_start,
		logHook = updatePointer (0.99, 0.001) (0, 0),
		layoutHook =
			let tiled = ResizableTall 1 (3/100) (1/2) [] in
			tiled ||| Full ||| Mirror tiled,
		terminal = "x-terminal-emulator"
	} `additionalKeysP`
	[

		("M-S-l",					sendMessage MirrorShrink),
		("M-S-h",					sendMessage MirrorExpand),

		("<XF86AudioLowerVolume>",	volume_down),
		("<XF86AudioRaiseVolume>",	volume_up),
		("<XF86AudioMute>",			volume_toggle),

		("M-z",						lock_screen),

		("M-S-<Backspace>",			spawn_chrome)

	]
