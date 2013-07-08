# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
setopt appendhistory autocd extendedglob nomatch notify
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
alias ls='ls --color=auto -F'
alias grep='grep --color=auto'
alias sshme='ssh bannerem195@lab.cs.potsdam.edu'
alias wli='wicd-curses'
alias sftpme='sftp bannerem195@lab.cs.potsdam.edu'
alias poweroff='systemctl poweroff'
alias clear='echo No.'
alias ch='cd'

# Set prompt
prompt bart
