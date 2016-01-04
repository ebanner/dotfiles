# Set Enviroment variables for the zsh shell

# Get the aliases and functions
[ -f ~/.zshrc ] && . ~/.zshrc

[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

[[ $HOST -eq 'masenko' ]] && source ~/.zprofile.masenko
