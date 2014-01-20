" vim: foldmethod=marker :

execute pathogen#infect()
call pathogen#helptags()

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
set listchars=tab:▸\ ,trail:¬
set hidden
set background=dark
set pastetoggle=<F8>
set formatoptions+=r
set nowrapscan

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

" Save changes to a read-only file without permissions
nnoremap <Leader>w :write !sudo tee % > /dev/null<CR>

" Use Virtural Replace mode instead of Replace mode
nnoremap R gR
nnoremap r gr

" Clear highlighting from screen
nnoremap <silent> <C-l> :nohlsearch<CR><C-l>

" Don't use Ex mode, use Q for formatting
nnoremap Q gq

" Y yanks until the end of the current line. Just like C and D.
nnoremap Y y$

" }}}
" Abbreviations ------------------------------------------------------------ {{{

iabbrev sysout System.out.println

" }}}
" Autogroups --------------------------------------------------------------- {{{

if has("autocmd")
  " Have Vim jump to the last position when reopening a file
  autocmd BufReadPost * if 1 < line("'\"") && line("'\"") <= line("$") | execute "normal! g'\"" | endif
endif

" }}}
" Vim Tips ----------------------------------------------------------------- {{{
"
" Miscellaneous:
"  Insert-normal mode (<C-o>)
"  Jump to the file name under the cursor (gf)
"  Jump from the use of a variable to its local declaration (gd)
"  % moves forward to find a useful character
"  Prepend a count to % to move to that percentage of the file
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
"  :mksession & :source for session goodness
"  Views (probably the most useful application is folding)
"  Don't ever use modelines. For anything.
"
"
" Windows & Tabs:
"  Divide the window horizontally (<C-w>s) and vertically (<C-w>v)
"  Use :lcd when using tabs
"
" Registers:
"  X11 clipboard ("+) and primary ("*) registers
"  Append to a register by using the uppercase version of the letter ("Ay) --
"    * useful in global commands to collect all the lines that match some pattern
"
" Macros:
"  Use uppercase version of register to append a macro to it (qQ for qq)
"
" Subsititution:
"  Substitute the contents of a register by using the \= item
"    e.g. %s//\=@a/g
"  Repeat the last subsitution over the whole file (g&)
"  Repeat the last substiution command (:&& or normal mode &)
"  Use a different delimiter than / for searches (:s+one/two+one or two+)
"  Use patterns in ranges (:?^Chapter?,/^Chapter/ s=grey=gray=g)
"  Use marks in ranges (:'t,'b s/foo/bar/)
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
