xrdb -merge ~/.Xresources

# Source scripts in /etc/X11/xinit/xinitrc.d/
if [ -d /etc/X11/xinit/xinitrc.d ]; then
for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
unset f
fi

# GPG agent
gnupginf="$HOME/.gnupg/gpg-agent.env"
if ( pgrep -u "${USER}" gpg-agent ); then
eval `cat $gnupginf`
    eval `cut -d= -f1 $gnupginf | xargs echo export`
else
eval `gpg-agent --daemon --write-env-file $gnupginf`
fi


systemctl --user set-environment \
    ALTERNATE_EDITOR='emacs -nw' \
    EDITOR='emacsclient -nw -c -a ""' \
    VISUAL='emacsclient -nw -c -a ""' \
    GIT_EDITOR='emacsclient -nw -c -a ""' \
    $(cat $gnupginf)

systemctl --user start xorg.target
while true; do sleep 1h; done
