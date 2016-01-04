# Set Enviroment variables for the zsh shell

# Get the aliases and functions
[ -f ~/.zshrc ] && . ~/.zshrc

[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

[ -f ~/.zprofile.mac ] && . ~/.zprofile.mac

[[ $HOST -eq 'masenko' ]] && source ~/.zprofile.masenko
