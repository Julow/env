# fzf

cat <<"EOF"
export FZF_DEFAULT_COMMAND="ack -f"
FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
EOF

cat "$ENV_PATH"/bash/external/fzf/{completion,key-bindings}.bash
