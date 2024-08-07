#!/usr/bin/env bash

# Doom emacs
if [ ! -d ~/.config/emacs ]; then
    git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
fi
~/.config/emacs/bin/doom sync -u
