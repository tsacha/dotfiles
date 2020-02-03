#!/bin/bash
flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
flatpak install -y flathub com.spotify.Client
sudo ln -s /var/lib/flatpak/exports/bin/com.spotify.Client /usr/local/bin/spotify
flatpak install -y flathub org.telegram.desktop
sudo ln -s /var/lib/flatpak/exports/bin/org.telegram.desktop /usr/local/bin/telegram
flatpak install -y flathub com.visualstudio.code
sudo ln -s /var/lib/flatpak/exports/bin/com.visualstudio.code /usr/local/bin/vscode

cd $HOME
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si

yay -S --noconfirm --noedit rofi-pass
