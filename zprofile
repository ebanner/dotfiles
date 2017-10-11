# Set Enviroment variables for the zsh shell

# Get the aliases and functions
[ -f ~/.zshrc ] && . ~/.zshrc

[ -f ~/.zprofile.mac ] && . ~/.zprofile.mac

# added by Anaconda3 4.4.0 installer
export PATH="/Users/ebanner/.anaconda/bin:$PATH"

# google cloud platform tools
source /Users/ebanner/Downloads/google-cloud-sdk/completion.zsh.inc
source /Users/ebanner/Downloads/google-cloud-sdk/path.zsh.inc

export GOOGLE_APPLICATION_CREDENTIALS=/Users/ebanner/.gcp/mosss-d654d19941ca.json
