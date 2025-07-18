Dotfiles
========

Configuration files for various utilities. Managed with
[GNU Stow](https://www.gnu.org/software/stow/).

## Installation

```bash
$ stow --dotfiles --target=$HOME .
$ python3 install.py # install vim plugins
```

## Other commands

```bash
$ stow --dotfiles --target=$HOME --simulate --verbose=3 . # debugging
$ stow --delete --dotfiles --target=$HOME . # unstow
```

## Keyboard layout

Probably the most unorthodox configuration I perform is modifying my keyboard layout (configuration in [`/karabiner`](karabiner)).

<img width="835" alt="keyboard-layout" src="https://github.com/ebanner/dotfiles/assets/2068912/a8f42dbc-5b95-433f-8c43-0bf6c54068d5">
*Image generated by [keyboard-layout-editor.com](http://keyboard-layout-editor.com).*

This layout is optimized to decrease the amount of typing done by the pinkies. It takes all the most frequently pressed keys by the pinkies (i.e. `Control`, `Alt`, `Enter`, and `Tab`) and puts them in the top row. This allows these keys to be pressed by the ring, middle, and index fingers.

I made this layout because I was getting pinky fatigue when using jupyter notebooks (hitting `Shift-Enter` a lot) and also emacs (hitting `Control` and `Alt`) a lot. I have been using this variants of this layout for over a year now and find it very enjoyable.

