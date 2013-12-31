" Vim configurations

execute pathogen#infect()
call pathogen#helptags()
syntax on
filetype plugin indent on


" Basic Settings ----------------------------------------------------------- {{{

set nocompatible        " Do not accomodate vi
set showcmd             " Show (partial) command in status line.
set showmatch           " Show matching brackets.
set smartcase           " Do smart case matching
set ignorecase          " Case insensitive searching
set incsearch           " Incremental search
set mouse=a             " Enable mouse usage (all modes)
set expandtab           " Insert appropriate number of spaces to fill a tab.
set smarttab            
set number              " Show line numbers.
set hlsearch            " When there is a previous search pattern, highlight all
set autoindent smartindent          
set expandtab           " Maximum width of text that is being inserted.
set textwidth=80        " Maximum width of text that is being inserted.
set tabstop=4           " Number of columns a Tab character accounts for
set expandtab           " Allows spaces to be used for tab characters
set softtabstop=4       " How many whitespace characters are removed on a BS
set shiftwidth=4        " Amount of whitespace for the indentation command
set formatoptions=q,r,t,c
set ruler               " Show the line and column number of the cursor position.
set wildmenu            " Set zsh-like autocomlete menu behavior
set wildmode=full
set listchars=tab:>\ ,eol:. " Use the same symbols as TextMate for tabstops and EOLs
set hidden              " Silence vim when changing between buffers with unsaved changes
set nrformats=          " Treat all numerals as decimal, regardless of whether
                        " they are padded with zeros
set history=200         " Set ex history to 200
set wildcharm=<C-Z>
set nocscopeverbose     " Don't alert us when another cscope database already exists
set ruler               " Display column number at the bottom of the buffer
set makeprg=build       " Set the make program to build
set background=light

" }}}


" Leaders ------------------------------------------------------------------ {{{

let mapleader = ","
let maplocalleader = ","

" }}}


" Mappings ----------------------------------------------------------------- {{{

" Move Gvim tabs with alt + left|right
nnoremap <silent> <A-Left> :execute 'silent! tabmove ' . (tabpagenr()-2)<CR>
nnoremap <silent> <A-Right> :execute 'silent! tabmove ' . tabpagenr()<CR>

" Switch between Gvim tabs easily
noremap <C-Left> <Esc>:tabprev<CR>
noremap <C-Right> <Esc>:tabnext<CR>
noremap <C-n> <Esc>:tabnew

" F7 to toggle NERDTree
nnoremap <silent> <F7> :NERDTreeToggle<CR>

" F8 to toggle paste mode
noremap <F8> :set paste!<CR>

" F9 to toggle Taglist
nnoremap <silent> <F9> :TlistToggle<CR>

" Source and edit vimrc on the fly
nnoremap <leader>ev :vsplit $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>

" Y yanks until the end of the current line
nnoremap Y y$

" }}}


" Abbreviations ------------------------------------------------------------ {{{

iabbrev sysout System.out.println

" }}}


" Functions ---------------------------------------------------------------- {{{

" Have Vim jump to the last position when reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

" Set tabstop, softtabstop and shiftwidth to the same value
command! -nargs=* Stab call Stab()
function! Stab()
  let l:tabstop = 1 * input('set tabstop = softtabstop = shiftwidth = ')
  if l:tabstop > 0
    let &l:sts = l:tabstop
    let &l:ts = l:tabstop
    let &l:sw = l:tabstop
  endif
  call SummarizeTabs()
endfunction

" Helper function for Stab
function! SummarizeTabs()
  try
    echohl ModeMsg
    echon 'tabstop='.&l:ts
    echon ' shiftwidth='.&l:sw
    echon ' softtabstop='.&l:sts
    if &l:et
      echon ' expandtab'
    else
      echon ' noexpandtab'
    endif
  finally
    echohl None
  endtry
endfunction

" }}}


" Autogroups --------------------------------------------------------------- {{{

" Fold groups of related code in vimrc by special markers
augroup vimrc
    autocmd!
    autocmd FileType vim setlocal foldmethod=marker
augroup END

" }}}


" Taglist ------------------------------------------------------------------ {{{

let Tlist_Auto_Open = 1
let Tlist_Exit_OnlyWindow = 1
let Tlist_Use_Right_Window = 1
let Tlist_Enable_Fold_Column = 0
let Tlist_Highlight_Tag_On_BufEnter = 1
let Tlist_File_Fold_Auto_Close = 1
highlight MyTagListTagName guifg=red ctermfg=red

" }}}


" NERDTree ----------------------------------------------------------------- {{{

let g:NERDTreeDirArrows=0

" }}}


" Misc --------------------------------------------------------------------- {{{

" Have vim display a friendly ASCII cat whenever we start vim
echo ">^.^<"

" }}}
