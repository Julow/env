# .tmux.conf

if ! which tmux >/dev/null; then
	exit 100
fi

{
	echo "TOOLS=$ENV_PATH/tools"
	cat "$SESSION_DIR/tmux.conf"
} > "$HOME/.tmux.conf"
