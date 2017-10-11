import os
import subprocess
import tempfile
import logging

HOME = os.environ['HOME']
DOTFILES_DIR = '{}/.dotfiles/'.format(HOME)


def cd(dir):
    try:
        os.chdir(dir)
    except OSError:
        print('Could not cd into {0}. Exiting.'.format(dir))
        exit(1)

def read_dotfiles(dotfiles_path):
    with open(dotfiles_path) as f:
        dotfiles = [line.strip() for line in f.readlines()]
    return dotfiles


if __name__ == '__main__':
    """Installs dotfiles to home directory

    Dotfiles already present are backed up then all dotfiles are symlinked from
    dotfiles/ into the user's home directory. Pathogen and other vim plugins are
    also installed.

    """
    cd(HOME)
    dotfiles = read_dotfiles(DOTFILES_DIR+'dotfiles')
    tmpdir = tempfile.mkdtemp()
    for dotfile in dotfiles:
        try:
            logging.info('Moving {} to {}'.format(dotfile, tmpdir))
            os.rename('.'+dotfile, tmpdir+dotfile)
            os.symlink(DOTFILES_DIR+dotfile, '.'+dotfile)
        except FileNotFoundError:
            pass

    # vim
    os.makedirs(HOME+'/.vim/autoload')
    dest, url = HOME+'/.vim/autoload/pathogen.vim', 'https://tpo.pe/pathogen.vim'
    subprocess.call(['curl', '-LSso', dest, url])
    cd('.vim')
    subprocess.call(['git', 'submodule', 'update', '--init']) # plugins
