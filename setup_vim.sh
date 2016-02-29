#!/bin/bash

git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
apt-get install haskell-platform
apt-get install suckless-tools
apt-get install xscreensaver
apt-get install x11-xserver-utils
apt-get install libXrandr-dev
apt-get install libxpm-dev
cabal update
cabal install xmonad
cabal install xmobar

