#!/bin/bash
if [ $(readlink -f ~/.config/termite/config) == "/home/sacha/Git/dotfiles/termite/config.light" ]; then
    ln -s -f ~/Git/dotfiles/termite/config.dark ~/.config/termite/config
    ln -s -f ~/Git/dotfiles/shell/dark.zsh ~/Git/dotfiles/shell/current.zsh
elif [ $(readlink -f ~/.config/termite/config) == "/home/sacha/Git/dotfiles/termite/config.dark" ]; then
    ln -s -f ~/Git/dotfiles/termite/config.light ~/.config/termite/config
    ln -s -f ~/Git/dotfiles/shell/light.zsh ~/Git/dotfiles/shell/current.zsh
fi;
pkill -USR1 termite
