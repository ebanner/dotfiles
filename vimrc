" Vim configurations

filetype plugin indent on
syntax on

set showcmd             " Show (partial) command in status line.
set showmatch           " Show matching brackets.
set ignorecase          " Do case insensitive matching
set smartcase           " Do smart case matching
set incsearch           " Incremental search
set mouse=a             " Enable mouse usage (all modes)
set tabstop=2           " Number of spaces that a <Tab> in the file counts for.
set shiftwidth=2        " Number of spaces to use for each step of (auto)indent.
set expandtab           " Insert appropriate number of spaces to fill a tab.
set smarttab            
set number              " Show line numbers.
set hlsearch            " When there is a previous search pattern, highlight all
set softtabstop=2
set autoindent          
set textwidth=80        " Maximum width of text that is being inserted.
set formatoptions=q,r,t,c
set ruler               " Show the line and column number of the cursor position.
set wildchar=<Tab>      " Set zsh-like autocomlete menu behavior
set wildmenu
set wildmode=full
set listchars=tab:▸\ ,eol:¬ " Use the same symbols as TextMate for tabstops and EOLs
set hidden              " Silence vim when changing between buffers with unsaved changes
set nrformats=          " Treat all numerals as decimal, regardless of whether
                        " they are padded with zeros
set history=200         " Set ex history to 200
set wildcharm=<C-Z>
nnoremap <F10> :b <C-Z>


" Have Vim jump to the last position when reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

" Move tabs with alt + left|right
nnoremap <silent> <A-Left> :execute 'silent! tabmove ' . (tabpagenr()-2)<CR>
nnoremap <silent> <A-Right> :execute 'silent! tabmove ' . tabpagenr()<CR>

" Switch between gvim tabs
map <C-Left> <Esc>:tabprev<CR>
map <C-Right> <Esc>:tabnext<CR>
map <C-n> <Esc>:tabnew

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

" Have <C-p> and <C-n> behave like up and down respectively in command mode
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

" Make a nicer navigation block than the one currently there
set langmap=jnJNnlNLljLJ
