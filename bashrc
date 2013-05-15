#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'
#alias pacman='pacman-color'
alias sshme='ssh bannerem195@lab.cs.potsdam.edu'
alias wli='wicd-curses'
alias play='mplayer -playlist '
alias sftpme='sftp bannerem195@lab.cs.potsdam.edu'
alias sudo='sudo '
alias poweroff='systemctl poweroff'
alias halt='systemctl poweroff'
alias clear='echo No.'

# defines colors and sets the PS1 variable
function set_prompt_style {
    local WHITE='\[\e[0m\]'
    local CYAN='\[\e[0;36m\]'    
    local BOLD_GREEN='\[\e[1;32m\]'    
    local GREEN='\[\e[0;92m\]'
    local RED='\[\e[0;91m\]'
	local BOLD_RED='\[\e[1;91m\]'

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

    export PS1="[${HOST_COLOR}\h$WHITE][${PATH_COLOR}\$(short_dir)$WHITE] ${SYMBOL_COLOR}${SYMBOL}$WHITE "
}

# returns tildified version of pwd
function short_dir {
    echo $PWD | sed "s#$HOME#~#" | awk -F'/' '{ if (NF>3) print $1 "/.../" $(NF-1) "/" $(NF); else print $0 }'
}


# call function set_prompt_style to set the prompt
set_prompt_style

# make reciever think i'm using xterm instead of urxvt for rendering
export TERM='xterm'

# vim
export EDITOR="vim"

# use syntax highlighting while viewing in less
export LESS='-R'

# allows tab completion for sudo and man
complete -c -f sudo
complete -c -f man

# set vi-like editing mode of commands
set -o vi
