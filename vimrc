execute pathogen#infect()
call pathogen#helptags()

" Basic Settings not included in sensible.vim
set nocompatible
set smartcase
set hlsearch
set ignorecase
set hidden
set mouse=
set expandtab
set textwidth=80
set expandtab
set softtabstop=4
set shiftwidth=4
set wildmenu
set wildmode=full
set listchars=tab:▸\ ,trail:¬
set background=dark
set pastetoggle=<F8>
set formatoptions+=r
set infercase
set linebreak
set modelines=1
set number
set shortmess-=S
" set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
set splitbelow
set diffopt+=vertical
set noequalalways
set nowrap
set ts=4

" vim-slime
let g:slime_target = "tmux"
let g:slime_python_ipython = 1
let g:slime_default_config = {"socket_name": "default", "target_pane": "1"}
let g:slime_dont_ask_default = 1
xmap <TAB> <Plug>SlimeRegionSend
nmap <TAB> <Plug>SlimeLineSend
nmap 1 <Plug>SlimeParagraphSend
nmap 3 <Plug>SlimeParagraphSend
nnoremap <C-(> <C-I>
let g:vim_isort_map = '!'
let g:miniBufExplBRSplit = 0

if 703 <# v:version
    set formatoptions+=j
endif

if has('persistent_undo')
    set undofile
    set undodir=/tmp
endif

" Easier traveral of command history than pressing <Up> and <Down>
cnoremap <C-n> <Down>
cnoremap <C-p> <Up>

" All is NOT lost if you begin editing a file for which you don't have write access!
nnoremap <Leader>w :write !sudo tee % > /dev/null<CR>

" Use Virtural Replace mode instead of Replace mode
nnoremap R gR
nnoremap r gr

" Clear highlighting from screen
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR><C-l>

" Previous Buffer mapping
nnoremap <Leader><Leader> <C-^>

" Y yanks until the end of the current line. Just like C and D.
nnoremap Y y$

" Visually select a line character-wise
nnoremap <Leader>s v^o$h

" Have gm do something more useful
nnoremap gm :call SetCursorHalfway()<CR>

function! SetCursorHalfway()
    let halfway = len(getline(line('.'))) / 2
    execute 'normal' . halfway . '|'
endfunction

if has("autocmd")
  " Have Vim jump to the last position when reopening a file
  autocmd BufReadPost * if 1 < line("'\"") && line("'\"") <= line("$") | execute "normal! g'\"" | endif
endif

" Vim Tips
"
" Miscellaneous:
"  Insert-normal mode (<C-o>)
"  :runtime syntax/colortest.vim for a demonstration of color combinations
"  Convert vim to html (see 2html.vim)
"  Use :patchmode when editing config files (:set patchmode=.orig)
"  :write >> file to append to file
"  :savas command "Save As"
"  Use 0 as a range with :read to insert result at the top of a file
"  guu, gUU, g~~, and g?? (rot13) act on an entire line
"  !{motion}{command} filters the current text captured by {motion} through {command}
"  write !{command} writes text in the buffer to a command
"  :browse oldfiles to open previously open buffers
"  Views (probably the most useful application is folding)
"  Use 'key' for mischievous hijinks (:X for added security)
"  View binary files (vim -b, :set display=uhex, %!xxd [-r])
"  View compressed files (*.gz, *.bz2)
"  'thesaurus'
"  :set virtualedit=all to edit tables
"  zi toggles folding
"  g-/g+ to go back/forward in time
"  :undolist to get a snapshot of the undo tree
"  :earlier/:later to restore the active buffer to an earlier point in time
"
" Subsititution & Searching: {{{2
"  Substitute the contents of a register by using the \= item
"    e.g. %s//\=@a/g
"  Repeat the last subsitution over the whole file (g&)
"  Repeat the last substiution command (:&& or normal mode &)
"  Use patterns in ranges (:?^Chapter?,/^Chapter/ s=grey=gray=g)
"  Use marks in ranges (:'t,'b s/foo/bar/)
"  \_s matches whitespace & newlines (/foo\_sbar/ matches foo separated by any
"    number of spaces including newlines then bar)
"
" Spelling:
"  Remove current word from spell file (zw)
"  Undo whatever spelling action has been taken against the word under the cursor (zug)
"  Source spell files that don't consist of English words for specialized purposes
"    e.g. :setlocal spellfile+=~/Foo/Bar/foobar.utf-8.add
"    e.g.g. Hit Nzg where N is the Nth spell file you want to add the word to
"
" Inserting Text:
"   <C-u> to delete everything to the left of the cursor
"   <C-e> and <C-y> insert text directly overhead of below

" Vimscript:
"   @{register} and &{option} are expressions themselves (i.e. they can be used
"       in any place a number or string can be used)
"   :next $VIMRUNTIME/compiler/*.vim to check out compiler plugins
