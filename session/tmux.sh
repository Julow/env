# .tmux.conf

cat > "$HOME/.tmux.conf" <<"EOF"

set -g prefix C-Space
unbind C-b
setw -g mode-keys vi

EOF
