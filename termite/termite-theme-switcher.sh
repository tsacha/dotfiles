#!/bin/bash
if [ $(gsettings get org.gnome.desktop.interface gtk-theme) == "'Adwaita'" ]; then
    gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita-dark'
    gsettings set org.gnome.desktop.interface icon-theme 'Adwaita-dark'
    ln -s -f ~/Git/dotfiles/termite/config.dark ~/.config/termite/config
    ln -s -f ~/Git/dotfiles/shell/dark.zsh ~/Git/dotfiles/shell/current.zsh
elif [ $(gsettings get org.gnome.desktop.interface gtk-theme) == "'Adwaita-dark'" ]; then
    gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita'
    gsettings set org.gnome.desktop.interface icon-theme 'Adwaita'
    ln -s -f ~/Git/dotfiles/termite/config.light ~/.config/termite/config
    ln -s -f ~/Git/dotfiles/shell/light.zsh ~/Git/dotfiles/shell/current.zsh
fi;
pkill -USR1 termite
