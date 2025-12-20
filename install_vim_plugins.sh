#!/usr/bin/env bash

set -e

mkdir -p "$HOME/.vim/autoload" && curl -LSso "$HOME/.vim/autoload/pathogen.vim" https://tpo.pe/pathogen.vim

cd $HOME/.vim && git submodule update --init
