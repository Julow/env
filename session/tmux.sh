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

# Escape key timeout
set -g escape-time 0

# New window/pane in same directory
# Also, bind ' and " for horizontal and vertical split
bind c neww -c "#{pane_current_path}"
bind "'" splitw -h -c "#{pane_current_path}"
bind '"' splitw -v -c "#{pane_current_path}"

# Start window indexes at 1
set -g base-index 1

# Monitor bell and don't make any sound
set -g monitor-bell on
set -g bell-action other
set -g visual-bell off

# Status bar
set -g status-bg black
set -g status-fg white
set-window-option -g window-status-current-style bold
TITLE="#{?pane_title,#T,#W}"
set -g window-status-format "#I-$TITLE"
set -g window-status-current-format "#I+$TITLE"
set -g status-right ""

# Alt+left/right to change window
bind -n M-left select-window -p
bind -n M-right select-window -n

# Alt+Shift+left/right to move the current window
bind -n M-S-left swap-window -t -1
bind -n M-S-right swap-window -t +1

# Alt+up/down to change pane
bind -n M-up select-pane -t :.+
bind -n M-down select-pane -t :.-

# Alt+Shift+up/down to move the current pane
bind -n M-S-up swap-pane -t :.+ \; select-pane -t :.+
bind -n M-S-down swap-pane -t :.- \; select-pane -t :.-
EOF
