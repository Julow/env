# htoprc

if ! which htop &>/dev/null; then
  exit 100
fi

ln -sf "$SESSION_DIR/htoprc" "$HOME/.config/htop/htoprc"
