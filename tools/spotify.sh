# Spawn spotify, wait for it to load and play a song

# Spawn spotify
spotify &
S=0
# Try playing a song with playerctl, every 0.5s
# If nothing is playing after 50 iterations (25s), give up
until sleep 0.5; [[ $(playerctl status) = "Playing" || $S -gt 50 ]]
do
	playerctl play
	S=$[S+1]
done 2>/dev/null
