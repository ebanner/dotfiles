" vim: foldmethod=marker: foldclose=all:

execute pathogen#infect()
call pathogen#helptags()

" Basic Settings not included in sensible.vim -- {{{1

set nocompatible
set ignorecase
set smartcase
set mouse=a
set expandtab
set hlsearch
set textwidth=80
set expandtab
set softtabstop=4
set shiftwidth=4
set wildmenu
set wildmode=full
set listchars=tab:▸\ ,trail:¬
set hidden
set background=dark
set pastetoggle=<F8>
set formatoptions+=r
set infercase
set linebreak

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


" Mappings -- {{{1

" Easier traveral of command history with <C-n> and <C-p>
cnoremap <C-n> <Down>
cnoremap <C-p> <Up>

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

" Visually select a character character-wise
nnoremap <Leader>s v^o$h

" Toggle folding of current fold with <Space>
nnoremap <Space> za


" Functions -- {{{1

" Keep this to implement going to center of line
function! Line_len()
    return len(getline(line('.')))
endfunction


" Abbreviations -- {{{1

inoreabbrev sysout System.out.println()<Left>
inoreabbrev #d #define
inoreabbrev #i #include ""<Left>


" Autogroups -- {{{1

if has("autocmd")
  " Have Vim jump to the last position when reopening a file
  autocmd BufReadPost * if 1 < line("'\"") && line("'\"") <= line("$") | execute "normal! g'\"" | endif
endif


" Vim Tips -- {{{1
"
" Miscellaneous: {{{2
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
"  Use 'key' for mischievous hijinks (:X for added security)
"  View binary files (vim -b, :set display=uhex, %!xxd [-r])
"  View compressed files (*.gz, *.bz2)
"  Look into using 'thesaurus'
"  :set virtualedit=all to edit tables
"  zi toggles folding
"
"
" Windows & Tabs:: {{{2
"  Divide the window horizontally (<C-w>s) and vertically (<C-w>v)
"  Use :lcd when using tabs
"
" Registers:: {{{2
"  X11 clipboard ("+) and primary ("*) registers
"  Append to a register by using the uppercase version of the letter ("Ay) --
"    * useful in global commands to collect all the lines that match some pattern
"
" Macros:: {{{2
"  Use uppercase version of register to append a macro to it (qQ for qq)
"
" Subsititution & Searching:
"  Substitute the contents of a register by using the \= item
"    e.g. %s//\=@a/g
"  Repeat the last subsitution over the whole file (g&)
"  Repeat the last substiution command (:&& or normal mode &)
"  Use a different delimiter than / for searches (:s+one/two+one or two+)
"  Use patterns in ranges (:?^Chapter?,/^Chapter/ s=grey=gray=g)
"  Use marks in ranges (:'t,'b s/foo/bar/)
"  \_s matches whitespace & newlines (/foo\_sbar/ matches foo separated by any
"    number of spaces including newlines then bar)
"
" Spelling:: {{{2
"  Generate a list of suggested spellings (z=)
"  Add current word to spell file (zg)
"  Remove current word from spell file (zw)
"  Undo whatever spelling action has been taken against the word under the cursor (zug)
"  Source spell files that don't consist of English words
"    e.g. :setlocal spellfile+=~/Foo/Bar/foobar.utf-8.add
"    e.g.g. Hit Nzg where N is the Nth spell file you want to add the word to
"
" Inserting Text:: {{{2
"   <C-u> to delete everything to the left of the cursor
"   <C-e> and <C-y> insert text directly overhead of below
"
