# kill itune

cat << "KILL_ITUNE_END"
launchctl unload -w /System/Library/LaunchAgents/com.apple.rcd.plist
launchctl unload -w /System/Library/LaunchDaemons/com.apple.apsd.plist
KILL_ITUNE_END
