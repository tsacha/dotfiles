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


# azure-cli-bin 2.55.0-2
# flux-bin 2.2.2-1
# gomplate-bin 3.11.5-1
# google-cloud-cli 458.0.0-1
# grpcurl-bin 1.8.9-1
# hurl-bin 4.1.0-2
# ijq 0.4.1-1
# keeper-password-manager 16.10.11-1
# mqtt-explorer 0.3.5-13
# noto-color-emoji-fontconfig 1.0.0-1
# rpi-imager-bin 1.8.4-1
# ventoy-bin 1.0.96-1
# visual-studio-code-bin 1.85.1-1
# winbox 3.40-2
# yay 12.2.0-1
