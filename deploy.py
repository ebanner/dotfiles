import os
import subprocess

DOTFILES_DIR = 'dotfiles/'
BACKUP_DIR = '.' + 'dotfiles.backup/'
DOTFILES = 'dotfiles'

HOME = os.environ['HOME']

PATHOGEN_URL = 'https://raw.github.com/tpope/vim-pathogen/master/autoload/pathogen.vim'

DOTFILE_SEEN = False


def cd(dir):
    try:
        os.chdir(dir)
    except OSError:
        print('Could not cd into {0}. Exiting.'.format(dir))
        exit(1)

if __name__ == '__main__':
    """Installs dotfiles to user's home directory

    Dotfiles already present are backed up then all dotfiles are symlinked from
    dotfiles/ into the user's home directory. Pathogen and other vim plugins are
    installed if necessary.

    """
    cd(HOME)

    with open(DOTFILES_DIR + DOTFILES) as f:
        dotfiles = [ line.strip() for line in f.readlines() ]

    if not dotfiles:
        print('Could not read {0}. Exiting.'.format(DOTFILES))
        exit(2)

    for dotfile in dotfiles:
        if os.path.isfile('.' + dotfile):
            # Backup dotfile

            if not DOTFILE_SEEN:
                print('Existing dotfile(s) detected. Performing backup...')
                DOTFILE_SEEN = True

                if not os.path.exists(BACKUP_DIR):
                    os.makedirs(BACKUP_DIR)

            os.rename('.' + dotfile, BACKUP_DIR + dotfile)

        try:
            os.symlink(DOTFILES_DIR + dotfile, '.' + dotfile)
        except OSError:
            os.remove('.' + dotfile)
            os.symlink(DOTFILES_DIR + dotfile, '.' + dotfile)

    cd(DOTFILES_DIR + 'vim')

    if not os.path.isdir('autoload'):
        # Install pathogen
        os.makedirs('autoload')
        subprocess.call(['curl', '-Sso', 'autoload/pathogen.vim', PATHOGEN_URL])

    with open('plugins.repos') as f:
        repos = [ line.strip() for line in f.readlines() ]

    if not os.path.isdir('bundle'):
        os.makedirs('bundle')

    cd('bundle')

    for repo in repos:
        plugin = repo.split('/')[-1].replace('.git', '')
        if not os.path.isdir(plugin):
            # Install plugin
            print('Installing {0}'.format(plugin))
            subprocess.call(['git', 'clone', repo], stderr=open(os.devnull, 'wb'))
