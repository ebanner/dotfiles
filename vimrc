" Vim configurations

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

" Use the same symbols as TextMate for tabstops and EOLs
set listchars=tab:▸\ ,eol:¬

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

filetype plugin indent on
syntax on

" Swap navigation keys
set langmap=jkJKKJkjlhLhHLhl
