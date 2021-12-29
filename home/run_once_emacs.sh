#!/usr/bin/env bash

# Doom emacs
if [ ! -d ~/.emacs.d ]; then
    git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d
    mkdir ~/.emacs.d/auto-save-list
    pushd ~/.emacs.d
else
    pushd ~/.emacs.d
    git pull
fi
./bin/doom sync -u
popd
