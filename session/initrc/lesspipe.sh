# Enable lesspipe if present

if which lesspipe &>/dev/null; then
	echo 'eval "$(SHELL=/bin/sh lesspipe)"'
fi
