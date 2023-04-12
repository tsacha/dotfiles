#!/bin/bash
chezmoi -S ~/Git/dotfiles -c ~/Git/dotfiles/chezmoi.toml --persistent-state ~/.config/chezmoi/chezmoistate.boltdb apply


flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
flatpak install -y flathub com.spotify.Client
flatpak install -y flathub com.visualstudio.code
sudo ln -s /var/lib/flatpak/exports/bin/com.visualstudio.code /usr/local/bin/vscode
sudo ln -s /var/lib/flatpak/exports/bin/com.spotify.Client /usr/local/bin/spotify

cd $HOME
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si
