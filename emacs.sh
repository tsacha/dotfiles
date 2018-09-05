#!/bin/bash
if [ ! -d ~/Git/emacs ]; then
    git clone git@github.com:tsacha/.emacs.d.git ~/Git/emacs
else
    pushd ~/Git/emacs
    git pull
    popd
fi
if [ ! -L ~/.emacs.d ]; then
    rm -Rf ~/.emacs.d/
    ln -s ~/Git/emacs ~/.emacs.d
fi

if [ ! -d ~/.emacs.d/elisp ]; then
    mkdir ~/.emacs.d/elisp
fi

if [ ! -d ~/.emacs.d/elisp/use-package ]; then
    git clone https://github.com/jwiegley/use-package ~/.emacs.d/elisp/use-package
else
    pushd ~/.emacs.d/elisp/use-package
    git pull
    popd
fi

pushd ~/.emacs.d
emacs --batch -l org sacha.org -f org-babel-tangle or emacs --batch -l org --eval '(org-babel-tangle-file "sacha.org")'
emacs --batch -l org sacha.org -f org-babel-tangle or emacs --batch -l org --eval '(package-initialize)'

popd

if [ ! -L ~/.emacs.d/init.el ]; then
    ln -s ~/.emacs.d/sacha.el ~/.emacs.d/init.el
fi
