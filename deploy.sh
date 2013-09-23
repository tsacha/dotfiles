#!/usr/bin/env sh

# Emacs
cp emacs/emacs.el ~/.emacs.el
mkdir -p ~/.config/emacs
mkdir -p ~/.emacs.d/{auto-save-list,cache,elpa,url}
cp -Rf emacs/config/* ~/.config/emacs

# Shells
cp bashrc ~/.bashrc
cp zshrc ~/.zshrc
cp zshrc.pre ~/.zshrc.pre
cp zshrc.local ~/.zshrc.local

# i3
cp -Rf i3/ ~/.i3
cp i3/i3status.conf ~/.i3status.conf

# urxvt
cp Xresources ~/.Xresources

# xorg
cp xinitrc ~/.xinitrc
