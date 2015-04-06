# Set Enviroment variables for the zsh shell

# Get the aliases and functions
[ -f ~/.zshrc ] && . ~/.zshrc

export EDITOR=vim
export TERM=xterm

xmodmap -e "pointer = 3 2 1"
setxkbmap -variant colemak us

[[ -f ~/.Xmodmap.emacs ]] && xmodmap ~/.Xmodmap.emacs
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
