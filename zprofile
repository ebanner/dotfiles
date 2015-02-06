# Set Enviroment variables for the zsh shell

# Get the aliases and functions
[ -f ~/.zshrc ] && . ~/.zshrc

export EDITOR=vim
export TERM=xterm

# Setting PATH for Python 3.4
# The orginal version is saved in .zprofile.pysave
PATH="/Library/Frameworks/Python.framework/Versions/3.4/bin:${PATH}"
export PATH
