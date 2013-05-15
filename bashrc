#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Aliases
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias sshme='ssh bannerem195@lab.cs.potsdam.edu'
alias wli='wicd-curses'
alias sftpme='sftp bannerem195@lab.cs.potsdam.edu'
alias sudo='sudo '
alias poweroff='systemctl poweroff'
alias halt='systemctl poweroff'
alias clear='echo No.'

# Defines colors and sets the PS1 variable
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

# Returns tildified version of pwd
function short_dir {
    echo $PWD | sed "s#$HOME#~#" | awk -F'/' '{ if (NF>3) print $1 "/.../" $(NF-1) "/" $(NF); else print $0 }'
}

set_prompt_style

export TERM='xterm'
export EDITOR="vim"
complete -c -f sudo # tab completion
complete -c -f man  # tab completion
set -o vi # set vi-like editing mode of commands
