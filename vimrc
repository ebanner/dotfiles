" Vim configurations

execute pathogen#infect()
call pathogen#helptags()
syntax on
filetype plugin indent on


" Basic Settings not included in sensible.vim ------------------------------ {{{

set nocompatible
set ignorecase
set smartcase
set mouse=a
set expandtab
set hlsearch
set smartindent
set textwidth=80
set expandtab
set tabstop=4
set softtabstop=4
set shiftwidth=4
set wildmenu
set wildmode=full
set listchars=tab:▸\ ,eol:¬
set hidden
set background=dark
set pastetoggle=<F8>
set formatoptions+=r
set visualbell

if 703 <# v:version
    set formatoptions+=j
endif

if exists('+relativenumber')
    set relativenumber
else
    set number
endif

if has('persistent_undo')
    set undofile
endif

" }}}


" Mappings ----------------------------------------------------------------- {{{

" Source vimrc on the fly
nnoremap <Leader>sv :source $MYVIMRC<CR>

" Save changes to a read-only file without permissions
nnoremap <Leader>w :write !sudo tee % > /dev/null<CR>

" Use Virtural Replace mode instead of Replace mode
nnoremap R gR
nnoremap r gr

" Clear highlighting from screen
nnoremap <silent> <C-l> :nohlsearch<CR><C-l>

" }}}


" Abbreviations ------------------------------------------------------------ {{{

iabbrev sysout System.out.println

" }}}


" Autogroups --------------------------------------------------------------- {{{

if has("autocmd")
  " Have Vim jump to the last position when reopening a file
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | execute "normal! g'\"" | endif

  " Fold groups of related code in vimrc by special markers
  augroup vimrc
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
  augroup END

endif

" }}}


" Vim Tips ----------------------------------------------------------------- {{{
"
" Miscellaneous:
"  Insert-normal mode (<C-o>)
"  Jump to the file name under the cursor (gf)
"  Jump from the use of a variable to its local declaration (gd)
"
" Windows & Tabs:
"  Divide the window horizontally (<C-w>s) and vertically (<C-w>v)
"  Use :lcd when using tabs
"
" Registers:
"  X11 clipboard ("+) and primary ("*) registers
"
" Macros:
"  Use uppercase version of register to append a macro to it (qQ for qq)
"
" Subsititution:
"  Substitute the contents of a register by using the \= item
"    e.g. %s//\=@a/g
"  Substitute the contents of a register by using the \= item
"  Repeat the last subsitution over the whole file (g&)
"  Repeat the last substiution command (:&&)
"  Repeat last substitution (&)
"
" Spelling:
"  Generate a list of suggested spellings (z=)
"  Add current word to spell file (zg)
"  Remove current word from spell file (zw)
"  Undo whatever spelling action has been taken against the word under the cursor (zug)
"  Source spell files that don't consist of English words
"    e.g. :setlocal spellfile+=~/Foo/Bar/foobar.utf-8.add
"    e.g.g. Hit Nzg where N is the Nth spell file you want to add the word to
"
" }}}
