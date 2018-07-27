#!/bin/bash
# Aurman installation part : 
# darim92 - https://github.com/darim92/aurman-install/blob/master/aurman_install.sh
# make tmp dir
mkdir -p aurman_install
cd aurman_install

echo "installing dependencies..."

# install dependencies
sudo pacman -Sy binutils make gcc fakeroot pkg-config --noconfirm --needed
sudo pacman -S git python python-regex python-requests pyalpm --noconfirm --needed

echo "installing expac-git..."

# install expac-git
curl -o PKGBUILD https://aur.archlinux.org/cgit/aur.git/plain/PKGBUILD?h=expac-git
makepkg PKGBUILD --skippgpcheck --install --noconfirm --needed

echo "installing aurman..."

# install aurman
curl -o PKGBUILD https://aur.archlinux.org/cgit/aur.git/plain/PKGBUILD?h=aurman
makepkg PKGBUILD --skippgpcheck --install --noconfirm --needed

# clean up
cd ..
rm -rf aurman_install
echo "-------------"
echo "enjoy aurman!"
echo "-------------"

# install pamac
read -p "install pamac? [y/n]" answer
if [[ $answer = y ]] ; then
  aurman -S pamac-aur --noconfirm
  echo "pamac is ready..."
  echo "-------------"
fi

echo "end of script..."

flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo
flatpak install -y flathub com.spotify.Client
sudo ln -s /var/lib/flatpak/exports/bin/com.spotify.Client /usr/local/bin/spotify
flatpak install -y flathub org.telegram.desktop
sudo ln -s /var/lib/flatpak/exports/bin/org.telegram.desktop /usr/local/bin/telegram
flatpak install -y flathub com.visualstudio.code
sudo ln -s /var/lib/flatpak/exports/bin/com.visualstudio.code /usr/local/bin/vscode

aurman -S --noconfirm --noedit rofi-pass
