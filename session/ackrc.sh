# .ackrc

if ! which ack >/dev/null; then
	exit 100
fi

cat <<EOF > "$HOME/.ackrc"
--ignore-directory=is:_opam
--ignore-directory=is:duniverse
EOF
