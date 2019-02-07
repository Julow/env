# .tmux.conf

if ! which tmux >/dev/null; then
	exit 100
fi

cp "$SESSION_DIR/tmux.conf" "$HOME/.tmux.conf"
