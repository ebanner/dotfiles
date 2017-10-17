#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Aliases
alias grep='grep --color=auto'
alias clear='echo Use Ctrl-L instead you mangy jackrabbit!'
alias sudo='sudo '
alias poweroff='systemctl poweroff'

# Defines colors and sets the PS1 variable
function set_prompt_style {
    local white='\[\e[0m\]'
    local cyan='\[\e[0;36m\]'
    local bold_green='\[\e[1;32m\]'
    local green='\[\e[0;92m\]'
    local red='\[\e[0;91m\]'
    local bold_red='\[\e[1;91m\]'
    local yellow='\e[0;33m'
    local host_color=$cyan
    local path_color=$bold_green
    local symbol_color=$green

    if [ `whoami` == 'root' ] ; then
        host_color=$red
        path_color=$bold_red
        symbol_color=$red
    fi

    export PS1="${host_color}\u@\h:${white} ${path_color}\w${white} ${yellow}\$(_jobs)${white}
 ${symbol_color}\$$white "
}

function _jobs {
    num_jobs=$(jobs -l | wc -l)
    [[ num_jobs -gt 0 ]] && echo " +${num_jobs}"
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

set -o vi
