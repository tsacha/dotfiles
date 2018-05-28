#!/bin/bash
if [ $(readlink -f ~/.config/termite/config) == "/home/sacha/Git/dotfiles/termite/config.light" ]; then
    ln -s -f ~/Git/dotfiles/termite/config.dark ~/.config/termite/config
elif [ $(readlink -f ~/.config/termite/config) == "/home/sacha/Git/dotfiles/termite/config.dark" ]; then
    ln -s -f ~/Git/dotfiles/termite/config.light ~/.config/termite/config
fi;
pkill -USR1 termite
