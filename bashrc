# .bashrc

export ALTERNATE_EDITOR='emacs -nw' 
export EDITOR='emacsclient -c -nw -a ""' 
export VISUAL='emacsclient -nw -c -a ""'
export GIT_EDITOR='emacsclient -nw -c -a ""'

alias e=$EDITOR

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# User specific aliases and functions
