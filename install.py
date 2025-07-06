import os
import subprocess
import tempfile
import logging
logging.basicConfig(level=logging.INFO)

HOME = os.environ['HOME']
logging.info('Home dir ={}'.format(HOME))
DOTFILES_DIR = '{}/.dotfiles/'.format(HOME)
logging.info(DOTFILES_DIR)


def cd(dir):
    try:
        os.chdir(dir)
    except OSError:
        print('Could not cd into {0}. Exiting.'.format(dir))
        exit(1)


if __name__ == '__main__':
    """Install pathogen and other vim plugins."""
    cd(HOME)

    try:
        os.makedirs(HOME+'/.vim/autoload')
    except:
        pass

    dest, url = HOME+'/.vim/autoload/pathogen.vim', 'https://tpo.pe/pathogen.vim'
    subprocess.call(['curl', '-LSso', dest, url])
    cd('.vim')
    subprocess.call(['git', 'submodule', 'update', '--init']) # plugins
