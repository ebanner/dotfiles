# Set Enviroment variables for the zsh shell

# Get the aliases and functions
[ -f ~/.zshrc ] && . ~/.zshrc

xmodmap -e "pointer = 3 2 1"

[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap
