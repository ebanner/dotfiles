# zshrc

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias q=exit
alias ls='ls --color'
alias grep='grep --color'
alias ack=ack-grep
# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=10000000 # Number of lines to save in $HISTFILE
SAVEHIST=10000000 # Number of lines of history to load up at invocation
REPORTTIME=10
setopt autocd completeinword extendedglob extendedhistory interactivecomments nomatch notify append_history sharehistory
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

# Tab completion should be case-insensitive
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'

# Better completion for killall
zstyle ':completion:*:killall:*' command 'ps -u $USER -o cmd'

# Quick and easy way to set up colored prompt
autoload -U promptinit
promptinit

# Set prompt
prompt bart

# cd's up to the directory into your current path
function up {
    [[ $# -eq 1 ]] && builtin cd $(awk -v dir=$1 'BEGIN { FS=dir } { print $1 }' <<< $PWD)$1
}

function ffind {
    [[ $# -eq 1 ]] && find . -type f -name $1
}

function dfind {
    [[ $# -eq 1 ]] && find . -type d -name $1
}

set -o vi
[[ $EMACS = t ]] && unsetopt zle
