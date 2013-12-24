#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias clear='echo Use Ctrl-L instead you mangy jackrabbit!'
alias sudo='sudo '
alias poweroff='systemctl poweroff'
alias halt='systemctl poweroff'
alias wli='wicd-curses'

# Defines colors and sets the PS1 variable
function set_prompt_style {
    local WHITE='\[\e[0m\]'
    local CYAN='\[\e[0;36m\]'
    local BOLD_GREEN='\[\e[1;32m\]'
    local GREEN='\[\e[0;92m\]'
    local RED='\[\e[0;91m\]'
    local BOLD_RED='\[\e[1;91m\]'
    local YELLOW='\e[0;33m'
    local HOST_COLOR=$CYAN
    local PATH_COLOR=$BOLD_GREEN
    local SYMBOL='$'
    local SYMBOL_COLOR=$GREEN

    if [ `whoami` == 'root' ] ; then
        HOST_COLOR=$RED
        PATH_COLOR=$BOLD_RED
        SYMBOL='#'
        SYMBOL_COLOR=$RED
    fi

    export PS1="[${HOST_COLOR}\h$WHITE][${PATH_COLOR}${PWD}$WHITE] ${SYMBOL_COLOR}
${SYMBOL}$WHITE "
}

# Emulates ksh-style two argument form of the cd command
function cd {
    # Use the builtin cd in the normal case
    if [[ "$#" != "2" ]] ; then 
        builtin cd $*
    else 
        builtin cd ${PWD/$1/$2}
    fi
}

# cd's up to the directory into your current path
function up {
    [[ $# -eq 1 ]] && builtin cd $(awk -v dir=$1 'BEGIN { FS=dir } { print $1 }' <<< $PWD)$1
}

# Set the prompt
set_prompt_style

complete -c -f man  # tab completion
set -o vi # Set vi-like editing mode of commands
