" All system-wide defaults are set in $VIMRUNTIME/debian.vim (usually just
" /usr/share/vim/vimcurrent/debian.vim) and sourced by the call to :runtime
" you can find below.  If you wish to change any of those settings, you should
" do it in this file (/etc/vim/vimrc), since debian.vim will be overwritten
" everytime an upgrade of the vim packages is performed.  It is recommended to
" make changes after sourcing debian.vim since it alters the value of the
" 'compatible' option.

" This line should not be removed as it ensures that various options are
" properly set to work with the Vim-related packages available in Debian.
runtime! debian.vim

" Enables syntax highlighting by default
if has("syntax")
  syntax on
endif

" Have Vim jump to the last position when reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

" Uncomment the following to have Vim load indentation rules and plugins
" according to the detected filetype.
if has("autocmd")
  filetype plugin indent on
endif

" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
set showcmd		" Show (partial) command in status line.
set showmatch		" Show matching brackets.
set ignorecase		" Do case insensitive matching
set smartcase		" Do smart case matching
set incsearch		" Incremental search
"set autowrite		" Automatically save before commands like :next and :make
"set hidden             " Hide buffers when they are abandoned
set mouse=a		    " Enable mouse usage (all modes)

set tabstop=4           " Number of spaces that a <Tab> in the file counts for.
set shiftwidth=4        " Number of spaces to use for each step of (auto)indent.
set expandtab           " Use the appropriate number of spaces to insert a <Tab>.
                        " Spaces are used in indents with the '>' and '<' commands
                        " and when 'autoindent' is on. To insert a real tab when
                        " 'expandtab' is on, use CTRL-V <Tab>.
set smarttab            " When on, a <Tab> in front of a line inserts blanks
                        " according to 'shiftwidth'. 'tabstop' is used in other
                        " places. A <BS> will delete a 'shiftwidth' worth of space
                        " at the start of the line.
set number              " Show line numbers.
set hlsearch            " When there is a previous search pattern, highlight all
set softtabstop=4
                        " its matches.
set autoindent          " Copy indent from current line when starting a new line
                        " (typing <CR> in Insert mode or when using the "o" or "O"
                        " command).
set textwidth=80        " Maximum width of text that is being inserted. A longer
                        " line will be broken after white space to get this width.
set formatoptions=q,r,t,c " This is a sequence of letters which describes how
                        " automatic formatting is to be done.
                        "
                        " letter    meaning when present in 'formatoptions'
                        " ------    ---------------------------------------
                        " c         Auto-wrap comments using textwidth, inserting
                        "           the current comment leader automatically.
                        " q         Allow formatting of comments with "gq".
                        " r         Automatically insert the current comment leader
                        "           after hitting <Enter> in Insert mode. 
                        " t         Auto-wrap text using textwidth (does not apply
                        "           to comments)
set ruler               " Show the line and column number of the cursor position,
                        " separated by a comma.

colorscheme peachpuff

" Source a global configuration file if available
if filereadable("/etc/vim/vimrc.local")
  source /etc/vim/vimrc.local
endif

" Move tabs with alt + left|right
nnoremap <silent> <A-Left> :execute 'silent! tabmove ' . (tabpagenr()-2)<CR>
nnoremap <silent> <A-Right> :execute 'silent! tabmove ' . tabpagenr()<CR>

" Switch between gvim tabs
map <C-Left> <Esc>:tabprev<CR>
map <C-Right> <Esc>:tabnext<CR>
map <C-n> <Esc>:tabnew

" Shortcut to rapidly toggle `set list`
nmap <leader>l :set list!<CR>

" Use the same symbols as TextMate for tabstops and EOLs
set listchars=tab:▸\ ,eol:¬

" Set tabstop, softtabstop and shiftwidth to the same value
" Invoke this command with :Stab
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

" Easier changing focus on multiple open windows.
map <C-h> <C-w>h
map <C-j> <C-w>j
map <C-k> <C-w>k
map <C-l> <C-w>l
