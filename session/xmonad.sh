# Xmonad

if ! which xmonad >/dev/null; then
	exit 100
fi

# xmonad config

XMONAD_DIR="$HOME/.xmonad"
XMONAD_CONF="$XMONAD_DIR/xmonad.hs"

mkdir -p "$XMONAD_DIR"
cp "$SESSION_DIR/xmonad.hs" "$XMONAD_CONF"

xmonad --recompile

# xinitrc

cat > $HOME/.xinitrc << "END"
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
	for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

xset -b

xmodmap ~/.xmodmap
xcape -e Hyper_L=space

pulseaudio -k -D

exec xmonad
END

cat > $HOME/.xreloadrc << "END"
xmodmap ~/.xmodmap
xcape -e Hyper_L=space

xrandr --output eDP-1 --auto --preferred \
	--output DP-1 --auto --left-of eDP-1
END
