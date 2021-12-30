#!/bin/bash
cm="chezmoi -S ~/Git/dotfiles -c ~/Git/dotfiles/chezmoi.toml --persistent-state ~/.config/chezmoi/chezmoistate.boltdb"
if [ $(gsettings get org.gnome.desktop.interface gtk-theme) == "'Adwaita'" ]; then
    gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita-dark'
    gsettings set org.gnome.desktop.interface icon-theme 'Adwaita-dark'
    $cm apply --force ~/.config/alacritty/alacritty.yml ~/.config/rofi/config.rasi
elif [ $(gsettings get org.gnome.desktop.interface gtk-theme) == "'Adwaita-dark'" ]; then
    gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita'
    gsettings set org.gnome.desktop.interface icon-theme 'Adwaita'
    $cm apply --force ~/.config/alacritty/alacritty.yml ~/.config/rofi/config.rasi
fi;
