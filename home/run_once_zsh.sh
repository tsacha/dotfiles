#!/usr/bin/env bash

# Oh my zsh
if [ ! -d ~/.oh-my-zsh ]; then
    git clone --depth 1 https://github.com/robbyrussell/oh-my-zsh.git ~/.oh-my-zsh
else
    pushd ~/.oh-my-zsh
    git pull
    popd
fi
