#!/usr/bin/bash

set -e

DOTFILES_DIR="$HOME/.dotfiles"

cd "$HOME"

mkdir -p "$HOME/.vim/autoload"

curl -LSso "$HOME/.vim/autoload/pathogen.vim" https://tpo.pe/pathogen.vim

cd .vim

git submodule update --init
