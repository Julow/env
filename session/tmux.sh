# .tmux.conf

cat > "$HOME/.tmux.conf" <<"EOF"
set -g prefix C-Space
unbind C-b

setw -g mode-keys vi

bind -T copy-mode-vi enter send-keys -X copy-pipe-and-cancel "xclip -selection clipboard"
bind "]" run "xclip -o -sel clipboard | tmux load-buffer -; tmux paste-buffer"
EOF
