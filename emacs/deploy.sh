#!/usr/bin/env sh
[ ! -h ~/.emacs.el ] && ln -s `pwd`/emacs.el ~/.emacs.el
[ ! -d ~/.config/emacs ] && mkdir -p ~/.config/emacs
find `pwd`/config -type f -exec ln -s {} ~/.config/emacs/ 2> /dev/null \;

