#!/usr/bin/env sh

# Emacs deployment
cp emacs/emacs.el ~/.emacs.el
mkdir -p ~/.config/emacs
mkdir -p ~/.emacs.d/{auto-save-list,cache,elpa,url}
cp -Rf emacs/config/* ~/.config/emacs

# Shells deployment
cp bashrc ~/.bashrc
