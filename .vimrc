" ~/.vimrc

"""
""" Preamble
"""
call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

"""
""" Settings
"""

" Basics
set nocompatible                " Use Vim settings instead of vi settings.
set autoindent                  " Copy indent from current line for new line.
set autoread                    " Automatically re-read files changed (but not deleted) on disk.
set backspace=indent,eol,start  " Backspace over everything in indent mode.
set browsedir=buffer            " Browse from the directory of the related buffer.
set cindent                     " Recommended setting for C-style indentation.
set copyindent                  " Copy the previous indentation on autoindenting.
set cpoptions+=J                " A sentence has to be followed by two spaces.
set encoding=utf-8              " Set character encoding.
set fillchars=diff:\            " Character to use for deleted lines in diff output.
set hidden                      " Allow hiding buffers with unsaved changes.
set history=1000                " Remember this many command lines.
set nojoinspaces                " Don't get fancy with the spaces when joining lines.
set lazyredraw                  " Don't redraw the screen unnecessarily.
set nolist                      " Turn 'list' off by default.
set listchars=tab:▸-            " Start and body of tabs.
set listchars+=trail:·          " Trailing spaces.
set listchars+=extends:>        " Last column when line extends off right.
set listchars+=precedes:<       " First column when line extends off left.
set listchars+=eol:¬            " End of line.
set matchpairs=(:),[:],{:},<:>  " Character pairs for use with %, 'showmatch'
set matchtime=3                 " In tenths of seconds.
set nomodeline                  " Ignore modelines.
set ruler                       " Always show the cursor position.
set scrolloff=3                 " Context lines at top and bottom of display.
set shortmess+=I                " Don't show the Vim welcome screen.
set showbreak=↪                 " String to put at start of lines that have been wrapped.
set showcmd                     " Display incomplete commands.
set showmatch                   " Jump to matching characters.
set showmode                    " Show Insert, Replace or Visual on the last line.
set nosmartindent               " 'smartindent' breaks right-shifting of # lines.
set notimeout                   " Do not time out.
set nottimeout                  " ...
set ttyfast                     " Assume fast terminal connection.
set virtualedit=block           " Allow virtual editing when in Visual Block mode.
set visualbell                  " Use visual bell.
set wildignore+=*.pyc,.hg,.git  " Ignored when completion matching.
set wildmenu                    " Use menu for completions.
set wildmode=list:longest       " List all matches and complete till longest common string.

" Searching
set gdefault                    " All matches in a line are substituted by default.
set ignorecase                  " Ignore case for pattern matches (\C overrides).
set incsearch                   " Do incremental searching.
set smartcase                   " Override 'ignorecase' if pattern contains uppercase.
set nowrapscan                  " Don't allow searches to wrap around EOF.

if has("win32")
    set grepprg=grep\ -n        " Windows findstr.exe just isn't good enough.
endif

" Line numbering
set number                      " Display line numbers.
set numberwidth=4               " Minimum number of columns to show for line numbers.
set norelativenumber            " Don't use relative line numbers.

" Tabs, spaces, wrapping
set display=lastline            " Display as much of a window's last line as possible.
set expandtab                   " Insert spaces for <Tab> press; use spaces to indent.
set formatoptions=q             " Allow formatting of comments with 'gq'.
set formatoptions+=n            " Recognize numbered lists.
set formatoptions+=1            " Don't break a line after a one-letter word.
set linebreak                   " Wrap at 'breakat' char vs display edge if 'wrap' is on.
set shiftround                  " Round indent to a multiple of 'shiftwidth'.
set shiftwidth=4                " Number of spaces to use for indent and unindent.
set sidescroll=1                " Number of chars to scroll when scrolling sideways.
set sidescrolloff=5             " Context columns at left and right.
set smarttab                    " Tab respects 'shiftwidth', 'tabstop', 'softtabstop'.
set softtabstop=8               " Edit as if tabs are 8 characters wide.
set tabstop=8                   " Set the visible width of tabs.
set textwidth=0                 " Don't auto-wrap lines except for specific filetypes.
set whichwrap+=<,>,[,]          " Allow left/right arrows to move across lines.
set nowrap                      " Don't wrap the display of long lines.

if executable("par")
    set formatprg=par\ -w78     " When available, use 'par' as the formatting program for 'gq'.
endif

" Folds
set foldcolumn=3                " Number of columns to show at left for folds.
set foldenable                  " Enable folding.
set foldnestmax=3               " Only allow 3 levels of folding.
set foldlevelstart=99           " Start with all folds open.

" Status line
set laststatus=2                " Always show a status line.
set statusline=%F\ %m%r%y%w%h%=B:%n\ L:%l/%L[%p%%]\ C:%v

" Windowing
set splitbelow                  " Split new horizontal windows under current window.
set splitright                  " Split new vertical windows right of current window.
set winminheight=0              " Allow windows to shrink to status line.
set winminwidth=0               " Allow windows to shrink to vertical separator.

" Backups, swap, etc.
set backupdir=~/.vim/tmp/backup
set directory=~/.vim/tmp/swap
set undodir=~/.vim/tmp/undo

if !isdirectory(expand(&backupdir))
    call mkdir(expand(&backupdir), "p")
endif

if !isdirectory(expand(&directory))
    call mkdir(expand(&directory), "p")
endif

if !isdirectory(expand(&undodir))
    call mkdir(expand(&undodir), "p")
endif

set backup
set backupskip=/tmp/*,/private/tmp/*

" Colors
set background=dark
set nocursorcolumn              " Don't highlight the current screen column.
set nocursorline                " Don't highlight the current screen line.

colorscheme wombat256mod

" Miscellaneous
if has("mouse")
    set mouse=a                 " Enable mouse support if it's available.
endif

" Switch on syntax highlighting when the terminal has colors or when running
" in the GUI.  Also switch on highlighting the latest search pattern.
if &t_Co > 2 || has("gui_running")
    syntax on
    set hlsearch
endif

""
""" Autocommands
"""
if has("autocmd") && !exists("autocommands_loaded")

    " Set a flag to indicate that autocommands have already been loaded,
    " so we only do this once. I use this flag instead of just blindly
    " running `autocmd!` (which removes all autocommands from the
    " current group) because `autocmd!` breaks the syntax highlighting /
    " syntax menu expansion logic.
    "
    let autocommands_loaded = 1

    " Enable filetype detection, so language-dependent plugins, indentation
    " files, syntax highlighting, etc., are loaded for specific filetypes.
    filetype plugin indent on

    " When editing a file, always jump to the last known cursor
    " position. Don't do it when the position is invalid or when inside
    " an event handler (happens when dropping a file on gvim).
    au BufReadPost *
        \   if line("'\"") > 0 && line("'\"") <= line("$") |
        \       exe "normal g`\"" |
        \   endif

    " Resize Vim windows to equal heights and widths when Vim itself is resized.
    au VimResized * wincmd =

    " Turn off browse dialog filters.
    au FileType * let b:browsefilter = ''

    " Close Ack/Quickfix windows.
    au FileType qf map <leader>q :cclose<CR>

    " Close help windows with <leader>q.
    au FileType HELP map <leader>q :q<CR>

endif

"""
""" Leaders
"""
let mapleader = "\\"
let maplocalleader = ","

" Just one space.
nmap <leader><space> :call JustOneSpace()<cr>

" Clear highlighting of last search pattern.
nmap <leader>\ :noh<cr>

" Untabify/tabify.
nmap <leader><tab> :retab!<cr>
nmap <leader><s-tab> :set noexpandtab<cr>:retab!<cr>:set expandtab<cr>

" Delete trailing whitespace.
nmap <leader><bs> :call Preserve("%s/\\s\\+$//e")<cr>

" NERDTree
map <Leader>d :NERDTreeToggle<cr>

" Edit another file in the same directory as the current file
" uses expression to extract path from current file's path.
map <Leader>e :e <c-r>=expand("%:p:h") . '/'<cr>
map <Leader>s :split <c-r>=expand("%:p:h") . '/'<cr>
map <Leader>v :vnew <c-r>=expand("%:p:h") . '/'<cr>

" Execute selection as Vimscript.
vnoremap <leader>E y:@"<cr>
nnoremap <leader>E yy:@"<cr>

" Buffet
map <leader>B :Bufferlist<cr>

" Ctrl-P
map <leader>f :CtrlP<cr>

" Toggle invisible characters.
map <leader>i :set list!<cr>

" Rename current file.
nmap <leader>r :call RenameFile()<cr>

" Easier cut/copy/paste to/from the system clipboard.
noremap <leader>x "*x
noremap <leader>y "*y
nnoremap <leader>Y :%y "*<cr>
noremap <leader>p :set paste<cr>"*p<cr>:set nopaste<cr>
noremap <leader>P :set paste<cr>"*P<cr>:set nopaste<cr>

"""
""" Other key mappings
"""

" Move by display lines.
noremap j gj
noremap k gk

" Nuke the help key, instead toggle fullscreen mode.
noremap <F1> :set invfullscreen<CR>

" Easy window navigation.
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

" Y behaves as you'd expect.
nnoremap Y y$

" Backsapce in visual mode deletes selection.
vnoremap <BS> d

" Default searches to 'very magic', more like the world outside Vim.
nnoremap / /\v
vnoremap / /\v
nnoremap ? ?\v
vnoremap ? ?\v

" Tab/shift-tab to indent/outdent in visual mode.
vnoremap <Tab> >gv
vnoremap <S-Tab> <gv

" Keep selection when indenting/outdenting.
vnoremap > >gv
vnoremap < <gv

" Center the display line after searches. (This makes it *much* easier to see
" the matched line.)
"
" More info: http://www.vim.org/tips/tip.php?tip_id=528
"
nnoremap n nzz
nnoremap N Nzz
nnoremap * *zz
nnoremap # #zz
nnoremap g* g*zz
nnoremap g# g#zz

" Keep the cursor in place while joining lines.
nnoremap J mzJ`z

" In command mode, type %% to insert the path of the currently edited file, as a shortcut for %:h<tab>.
cmap %% <C-R>=expand("%:h") . "/" <CR>

" A little Emacs heresy.
inoremap <c-a> <esc>I
inoremap <c-e> <esc>A

" Center line on previous/next fix.
map - :cprev<CR> zz
map + :cnext<CR> zz

" Center line in previous/next file.
map g- :cpfile<CR> zz
map g+ :cnfile<CR> zz

" Disable Ex mode
map Q <Nop>

" Disable man page lookups
map K <Nop>

"""
""" Filetype, indent, syntax and plugin-specific configuration
"""

" Autoclose (off by default)
let g:autoclose_on=0

" BufExplorer
let g:bufExplorerDefaultMappings=1

" Ctrl-P
let g:ctrlp_by_filename=1
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\.git$\|\.hg$\|\.svn$',
  \ 'file': '\.exe$\|\.obj$\|\.dll$\|\.so$\|\.o$\|\.hi$\|\.a$',
  \ }

" Haskell
let g:haskell_indent_if=2
let g:haskell_indent_case=2

"""
""" Custom commands
"""

" Replace consecutive spaces with just one space.
function JustOneSpace()
  " Get the current contents of the current line
  let current_line = getline(".")

  " Get the current cursor position
  let cursor_position = getpos(".")

  " Generate a match using the column number of the current cursor position
  let matchre = '\s*\%' . cursor_position[2] . 'c\s*'
  let pos = match(current_line, matchre) + 2

  " Modify the line by replacing with one space
  let modified_line = substitute(current_line, matchre, " ", "")

  " Modify the cursor position to handle the change in string length
  let cursor_position[2] = pos

  " Set the line in the window
  call setline(".", modified_line)

  " Reset the cursor position
  call setpos(".", cursor_position)
endfunction

" Set tabstop, softtabstop and shiftwidth to the same value.
command! -nargs=* Stab call Stab()
function! Stab()
  let l:tabstop = 1 * input('set tabstop = softtabstop = shiftwidth = ')
  if l:tabstop > 0
    let &l:sts = l:tabstop
    let &l:ts = l:tabstop
    let &l:sw = l:tabstop
  endif
endfunction

" Preserve search history and cursor position across a command execution.
function! Preserve(command)
  " Preparation: save last search, and cursor position.
  let _s=@/
  let l = line(".")
  let c = col(".")
  " Do the business:
  execute a:command
  " Clean up: restore previous search history, and cursor position
  let @/=_s
  call cursor(l, c)
endfunction

" Rename current file.
function! RenameFile()
  let old_name = expand('%')
  let new_name = input('New file name: ', expand('%'), 'file')
  if new_name != '' && new_name != old_name
    exec ':saveas ' . new_name
    exec ':silent !rm ' . old_name
    redraw!
  endif
endfunction
