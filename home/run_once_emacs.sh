#!/usr/bin/env bash

# Doom emacs
if [ ! -d ~/.emacs.d ]; then
    git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
    mkdir ~/.emacs.d/auto-save-list
else
    git -C ~/.emacs.d pull
fi
~/.emacs.d/bin/doom sync -u
popd
