#!/bin/bash
if [ $(gsettings get org.gnome.desktop.interface gtk-theme) == "'Adwaita'" ]; then
    gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita-dark'
    gsettings set org.gnome.desktop.interface icon-theme 'Adwaita-dark'
    sed -Ei --follow-symlinks 's/colors: \*\w+/colors: *dark/g' ~/.config/alacritty/alacritty.yml
    ln -s -f ~/Git/dotfiles/shell/dark.zsh ~/Git/dotfiles/shell/current.zsh
elif [ $(gsettings get org.gnome.desktop.interface gtk-theme) == "'Adwaita-dark'" ]; then
    gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita'
    gsettings set org.gnome.desktop.interface icon-theme 'Adwaita'
    sed -Ei --follow-symlinks 's/colors: \*\w+/colors: *light/g' ~/.config/alacritty/alacritty.yml
    ln -s -f ~/Git/dotfiles/shell/light.zsh ~/Git/dotfiles/shell/current.zsh
fi;
