#!/bin/bash
cm="chezmoi -S ~/Git/dotfiles -c ~/Git/dotfiles/chezmoi.toml --persistent-state ~/.config/chezmoi/chezmoistate.boltdb"
if [ "$(uname)" != "Darwin" ]; then
if [[ "$XDG_CURRENT_DESKTOP" == *"KDE"* ]]; then
    current_theme=$(kreadconfig6 --file kdeglobals --group KDE --key LookAndFeelPackage)
    if [ "$current_theme" == "org.kde.breezedark.desktop" ]; then
        plasma-apply-lookandfeel -a org.kde.breeze.desktop
    else
        plasma-apply-lookandfeel -a org.kde.breezedark.desktop
    fi
else
if [ $(gsettings get org.gnome.desktop.interface color-scheme) == "'prefer-light'" ]; then
    gsettings set org.gnome.desktop.interface color-scheme \'prefer-dark\'
    gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita-dark'
    awww query && awww img $HOME/.wallpapers/dark.jpg
elif [ $(gsettings get org.gnome.desktop.interface color-scheme) == "'prefer-dark'" ]; then
    gsettings set org.gnome.desktop.interface color-scheme \'prefer-light\'
    gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita'
    awww query && awww img $HOME/.wallpapers/light.png
elif [ $(gsettings get org.gnome.desktop.interface color-scheme) == "'default'" ]; then
    gsettings set org.gnome.desktop.interface color-scheme \'prefer-light\'
    gsettings set org.gnome.desktop.interface gtk-theme 'Adwaita'
    awww query && awww img $HOME/.wallpapers/light.png
fi;
$cm apply --force \
    ~/.config/sway/config \
    ~/.config/niri/config.kdl \
    ~/.config/k9s/skins/rosepine.yaml
swaymsg reload
fi
else
osascript -e 'tell app "System Events" to tell appearance preferences to set dark mode to not dark mode'
$cm apply --force \
    ~/.config/k9s/config.yaml \
    ~/.config/k9s/skins/rosepine.yaml \
    ~/.config/zellij/config.kdl
fi
