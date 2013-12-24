#!/bin/bash
#
# This script backs up dotfiles that are in the way and restores dotfiles to
# where they need to be.

dotfiles=dotfiles            # Dotfiles directory
old_dir=.dotfiles.old        # Old dotfiles backup directory
files='bashrc vimrc Xresources dwm-statusbar.sh gitconfig inputrc xinitrc fehbg Xmodmap Xmodmap.dwm zshrc tmux.conf'

# Create dotfiles.old in homedir
echo "Creating $old_dir for backup of any existing dotfiles in $HOME"
mkdir -p $HOME/$old_dir
echo "...done"

# Move any existing dotfiles in homedir to dotfiles.old directory, then create
# symlinks from the homedir to any files in the ~/dotfiles directory specified
# in $files
echo "Moving any existing dotfiles from $HOME to $old_dir..."
for file in $files; do
  [[ -e $HOME/.$file ]] && mv $HOME/.$file $HOME/$old_dir
done

echo "Creating symlinks..."
for file in $files; do
  [[ ! -h $HOME/.$file ]] && ln -s $HOME/$dotfiles/$file $HOME/.$file
done
