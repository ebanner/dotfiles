# zshrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000000 # Number of lines to save in $HISTFILE
SAVEHIST=10000000 # Number of lines of history to load up at invocation
setopt autocd extendedglob nomatch notify append_history share_history
unsetopt beep
bindkey -v
# End of lines configured by zsh-newuser-install
# The following lines were added by compinstall
zstyle :compinstall filename '/home/edward/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall

# For autocomletion with an arrow-key driven interface
zstyle ':completion:*' menu select

# Quick and easy way to set up colored prompt
autoload -U promptinit
promptinit

# Aliases
alias grep=ack

# Set prompt
prompt bart

# cd's up to the directory into your current path
function up {
    [[ $# -eq 1 ]] && builtin cd $(awk -v dir=$1 'BEGIN { FS=dir } { print $1 }' <<< $PWD)$1
}

# vi-mode quick <Esc>-/ fix (credit to marshaul)
vi-search-fix() {
    zle vi-cmd-mode
    zle .vi-history-search-backward
}

autoload vi-search-fix
zle -N vi-search-fix
bindkey -M viins '\e/' vi-search-fix
