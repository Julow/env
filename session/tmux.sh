# .tmux.conf

if ! which tmux >/dev/null; then
	exit 100
fi

cat > "$HOME/.tmux.conf" <<"EOF"
# Change prefix to C-Space
set -g prefix C-Space
unbind C-b

# Vi bindings
setw -g mode-keys vi

# Copy and paste use x clipboard
bind -T copy-mode-vi enter send-keys -X copy-pipe-and-cancel "xclip -selection clipboard"
bind "]" run "xclip -o -sel clipboard | tmux load-buffer -; tmux paste-buffer"

# Mouse mode
set -g mouse on
# Ending mouse selection does not close selection mode
unbind -n -Tcopy-mode-vi MouseDragEnd1Pane

# Disable status line
set -g status off

# Escape key timeout
set -g escape-time 0

# New window in same directory
bind c neww -c "#{pane_current_path}"

# Prev/next window
bind -T root "C-[" previous-window
bind -T root "C-]" next-window
EOF
