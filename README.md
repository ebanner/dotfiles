Dotfiles
========

Configuration files for various utilities

A lot of the config files are for old programs that I do not use anymore. The programs I actively still use are the following.

- Emacs (`spacemacs`)
- Karabiner (`private.xml`)
- zsh
  - `zprofile`
  - `zshrc`

## Keyboard layout

Probably the most unorthodox configuration I perform is modifying my keyboard layout (configuration in [`/karabiner`](karabiner)).

![Keyboard layout](../assets/keyboard-layout.png?raw=true)
*Image generated by [keyboard-layout-editor.com](http://keyboard-layout-editor.com).*

This layout is optimized to decrease the amount of typing done by the pinkies. It takes all the most frequently pressed keys by the pinkies (i.e. `Control`, `Alt`, `Enter`, and `Tab`) and puts them in the top row. This allows these keys to be pressed by the ring, middle, and index fingers.

I made this layout because I was getting pinky fatigue when using jupyter notebooks (hitting `Shift-Enter` a lot) and also emacs (hitting `Control` and `Alt`) a lot. I have been using this variants of this layout for over a year now and find it very enjoyable.

## Notes

- Use Option as Meta key in OSX Terminal
- Put `karabiner.json` in `~/.config/karabiner/`
