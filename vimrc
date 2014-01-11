" Vim configurations

execute pathogen#infect()
call pathogen#helptags()
syntax on
filetype plugin indent on


" Basic Settings ----------------------------------------------------------- {{{

set nocompatible        " Do not accomodate vi
set ignorecase          " Ignore case as long as all characters are lowercase
set smartcase           " Respect case once an uppercase letter enters the search
set incsearch           " Highlight search as it is formed
set mouse=a             " Enable mouse usage for resizing widows
set expandtab           " Insert appropriate number of spaces to fill a tab.
set number              " Show line numbers.
set hlsearch            " Highlight active search string
set autoindent smartindent
set textwidth=80
set tabstop=4           " Width of a tab character
set softtabstop=4       " Number of whitespace characters are removed on <BS>
set shiftwidth=4        " Number of whitespace used in indentation (> and <) commands
set expandtab           " Use spaces in place of tab characters
set formatoptions=q,r,t,c
set ruler               " Show the line and column number of the cursor position.
set wildmenu            " zsh-like auto-comlete menu behavior
set wildmode=full
set listchars=tab:>\ ,eol:.
set hidden              " Silence vim when changing between buffers with unsaved changes
set nrformats=          " Treat all numerals as decimal, regardless of whether
                        " they are padded with zeros
set history=200         " Set ex history to 200
set nocscopeverbose     " Don't alert us when another cscope database already exists
set background=dark
set pastetoggle=<F8>

" }}}


" Mappings ----------------------------------------------------------------- {{{

" F9 to toggle Taglist
nnoremap <silent> <F9> :TlistToggle<CR>

" Source vimrc on the fly
nnoremap <Leader>sv :source $MYVIMRC<CR>

" Save changes to a read-only file without permissions
nnoremap <Leader>w :write !sudo tee % > /dev/null<CR>

" Use Virtural Replace mode instead of Replace mode
nnoremap R gR
nnoremap r gr

" Clear highlighting from screen
nnoremap <silent> <C-l> :nohlsearch<CR><C-l>

" Make & use the same flags as the last subsitute command
nnoremap & :&&<CR>
xnoremap & :&&<CR>

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


" Taglist ------------------------------------------------------------------ {{{

let Tlist_Auto_Open = 1
let Tlist_Exit_OnlyWindow = 1
let Tlist_USE_RIGHT_Window = 1
let Tlist_Enable_Fold_Column = 0
let Tlist_Highlight_Tag_On_BufEnter = 1
let Tlist_File_Fold_Auto_Close = 1
highlight MyTagListTagName guifg=red ctermfg=red

" }}}


" Misc --------------------------------------------------------------------- {{{

" Have vim display a friendly ASCII cat whenever we start vim
echo ">^.^<"

" }}}


" Vim Tips ----------------------------------------------------------------- {{{
"
" Many tips here taken from ``Practical Vim - Edit Text at the Speed of Thought" by Drew Neil
"
" Insert-normal mode (<C-o>)
" Repeat last substitution (&)
" Look up the man page for the word under the cursor (K)
" Paste the contents of a register while in Insert Mode (<C-r>{register} or <C-r><C-p>{register})
"   e.g. The Expression Register (<C-r>=)
" Insert character by its character code in Insert Mode
"   e.g. <C-v>{123} -- Insert character by decimal code
"   e.g. <C-v>u{1234} -- Insert character by hex code
"   e.g. <C-v>{nondigit} -- Insert nondigit literally
"   e.g. <C-k>{char1}{char2} -- Insert nondigit literally
" Find out the code for the character under the cursor (ga)
" Reselect the last visual selection (gv)
" Go to other end of highlighted text while in Visual Mode (o)
" Inside tag text object (it)
" `.' represents the current line in an Ex command range
"   e.g. :.,$ print
" Patterns can be used in Ex ranges
"   e.g. :/<html>/,/<\/html>/ print
" Use relative Ex addressing in ranges to execute a command
"   e.g. :.,.+3 print
" Use :copy (:t) to copy a distant line
"   e.g. :copy . (:t .) to duplicate the current line
" Use :move (:m) to move a line under Visual selection
"   e.g. :'<'> move $ (:'<'> m $)
" Use :normal to apply a normal command to a range of contiguous lines
"   e.g. :'<'> normal A; (append a semicolon to the end of each line in Visual mode)
" Get the word (<C-r><C-w>) or WORD (<C-r><C-a>) under cursor
" Filter lines through an external program ([range] !{cmd} or use !{motion})
" Execute {cmd} in the shell with [range] lines as standard input ([range] write !{cmd})
"
" Use ** to recurse down directories when specifying the arglist
"   e.g. args **/*.c **/*.h
" Divide the window horizontally (<C-w>s) and vertically (<C-w>v)
" Close the active window (:close or <C-w>c) and keep only the active window (:only or <C-w>o)
" Use tabs if I need to edit something totally unrelated (view them as workspaces)
"   NOTE: Use :lcd to change the directory of the new tab
" Easily edit a file in the same directory as the present window (:edit " %:h<Tab>)
" Configure path for easier file searching with :find
"   e.g. set path+=src/**
" Use netrw for built-in file exploring (:Explore or :Sexplore or :Vexplore)
"   e.g. Use <C-^> to get back to the last buffer
"   e.g. Create new files (%)
"   e.g. Use netrw to edit files over scp
" 
" Prefix movement keys (jk^0$) with `g' to act on display lines instead of real lines
" Move backward to end of previous word (ge)
"   e.g. ea (append at the end of a word) and gea (append at end of previous word)
" Target low-frequency characters when using f{char} and F{char}
" Text object mnemonic `i' (inside) and `a' (around)
"   e.g. ab (AROUND pair of parenthesis) iB (INSIDE a pair of Braces)
" Bounded Text Objects
"   e.g. Use around (a) with `d' and inside (i) with `c' (rule of thumb)
"   e.g. Sentences (das) -- delete a sentence
" Use `{mark} instead of '{mark} to jump back to the row AND column position
" Builtin marks
"   e.g. `. jumps to the location of the last change
"   e.g. `` or <Ctrl-o> jumps to the location of last jump
" Use surround.vim for surrounding text with different characters
" Use vim-matchit to jump from between `if' and `else' clauses
" Jump to the file name under the cursor (gf)
" Use the changelist to jump to recent modifications (g; or `.)
" Enter into Insert mode at marker `^ (gi)
" Use global markers to snap back to a position in another file
"   e.g. mM before traversing the quick list populated by :vimgrep
" Black hole register ("_), Yank register ("0)
" X11 clipboard ("+) and primary ("*) registers
"
" Mute beep (:visualbell)
" A macro aborts when a beep is encountered
"   e.g. Play macros back with a high count
" Know the difference between different kinds of macros
"   e.g. Series - Go to the next line (j) and repeat with count ([num]@{reg})
"       e.g.g. Have the last command of a macro be :next and then issue [num]@{reg}
"   e.g. Parallel - Stay on current line, select target lines with Visual Line mode, and execute :normal @{reg}
"       e.g.g. :argdo normal @{reg}
" Use uppercase version of register to append a macro to it (qQ for qq)
" Edit macros by pasting its contents (:put {reg}) into the file, editing it, and then yanking it back into the register
"
" Force case-sensitivity (\C) or case-insensitivity (\c) in a search string
" Very-magic (\v) switch assumes all non-alphanumeric (and `_') characters take on special meaning
" Use literal switch (\V) for verbatim searches
" Character classes (:help /character-classes)
"   e.g. Hex character class (\x)
" In very magic searches, `<' and `>' are word delimeters
"   e.g. `/\v<the>' matches `the' but not `these'
" Lookaround expressions (\zs and \ze) allow you to match only certain parts of a pattern
" When searching for URLs, search backward so you don't have to escape `/' (only `?' and `\')
" Use search offsets
"   e.g. /foo/e or //e to append put the cursor at the end of the most recent search
" Convert search term to uppper case with search offsets
"   e.g. gU//e<CR>
" Search for text being highlighted in Visual mode with * and #
" Print the number of matches for a search pattern with the `n' flag
" Substitute the contents of a register by using the \= item
"   e.g. %s//\=@a/g
" Substitute the contents of a register by using the \= item
" Repeat the last subsitution over the whole file (g&)
" Repeat the last substiution command (:&&)
" Use global command to delete every line in a file that matches a pattern
"   e.g. :global//delete
"
" Check out ctags
" Check out :make and :grep
" Check out Ack.vim
" Press <C-n><C-p> to deselect entry from auto-complete menu
" Investigate <C-x><C-]> support for cscope
" Omni completion may be useful from time to time (<C-x><C-o>)
" Generate a list of suggested spellings (z=)
" Add current word to spell file (zg)
" Remove current word from spell file (zw)
" Undo whatever spelling action has been taken against the word under the cursor (zug)
" Source spell files that don't consist of English words
"   e.g. :setlocal spellfile+=~/Foo/Bar/foobar.utf-8.add
"   e.g.g. Hit Nzg where N is the Nth spell file you want to add the word to
" Scan back from the cursor for the first misspelled word and pop up a list of
" suggestions (<C-x>s) -- must be in Insert Mode
" Jump from the use of a variable to its local declaration (gd)
"
" }}}
