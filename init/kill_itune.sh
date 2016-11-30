# kill itune

if type launchctl >/dev/null
then
	cat << "KILL_ITUNE_END"
launchctl unload -w /System/Library/LaunchAgents/com.apple.rcd.plist 2>/dev/null
launchctl unload -w /System/Library/LaunchDaemons/com.apple.apsd.plist 2>/dev/null
KILL_ITUNE_END
fi
