eval $(gpg-agent --daemon) &
xrdb -merge ~/.Xresources


# Source scripts in /etc/X11/xinit/xinitrc.d/
if [ -d /etc/X11/xinit/xinitrc.d ]; then
for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
unset f
fi

exec i3
