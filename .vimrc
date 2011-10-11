" ~/.vimrc

"""
""" Preamble
"""
filetype off
call pathogen#runtime_append_all_bundles()

"""
""" Settings
"""

" Basics
set nocompatible                " Use Vim settings instead of vi settings.
set autochdir                   " Change the cwd when opening a file, switching buffers, etc.
set autoindent                  " Copy indent from current line for new line.
set autoread                    " Automatically re-read files changed (but not deleted) on disk.
set backspace=indent,eol,start  " Backspace over everything in indent mode.
set browsedir=buffer            " Browse from the directory of the related buffer.
set cpoptions+=J                " A sentence has to be followed by two spaces.
set encoding=utf-8              " Set character encoding.
set fillchars=diff:\            " Character to use for deleted lines in diff output.
set hidden                      " Keep changed buffers without requiring saves.
set history=1000                " Remember this many command lines.
set nojoinspaces                " Don't get fancy with the spaces when joining lines.
set lazyredraw                  " Don't redraw the screen unnecessarily.
set nolist                      " Turn 'list' off by default.
set listchars=tab:▸-            " Start and body of tabs.
set listchars+=trail:.          " Trailing spaces.
set listchars+=extends:>        " Last column when line extends off right.
set listchars+=precedes:<       " First column when line extends off left.
set listchars+=eol:¬            " End of line.
set matchpairs=(:),[:],{:},<:>  " Character pairs for use with %, 'showmatch'
set matchtime=3                 " In milliseconds.
set nomodeline                  " Ignore modelines.
set ruler                       " Always show the cursor position.
set scrolloff=3                 " Context lines at top and bottom of display.
set shell=/bin/bash
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
set hlsearch                    " Highlight latest search pattern.
set ignorecase                  " Ignore case for pattern matches (\C overrides).
set incsearch                   " Do incremental searching.
set smartcase                   " Override 'ignorecase' if pattern contains uppercase.
set nowrapscan                  " Don't allow searches to wrap around EOF.

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
set formatoptions-=or           " Don't repeat the current comment leader on new line.
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

" Folds
set foldcolumn=3                " Number of columns to show at left for folds.
set foldnestmax=3               " Only allow 3 levels of folding.
set foldlevelstart=99           " Start with all folds open.

" Status line
set laststatus=2                " Always show a status line.
set statusline=%F%m%r%h%w
set statusline+=\ %#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
set statusline+=%=(%{&ff}/%Y)

" Windowing
set splitbelow                  " Split new horizontal windows under current window.
set splitright                  " Split new vertical windows right of current window.
set winminheight=0              " Allow windows to shrink to status line.
set winminwidth=0               " Allow windows to shrink to vertical separator.

" Backups, swap, etc.
set backup
set backupdir=~/.vim/tmp/backup/
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.vim/tmp/swap/
set undodir=~/.vim/tmp/undo/

" Colors
set background=dark
set nocursorcolumn              " Don't highlight the current screen column.
set nocursorline                " Highlight the current screen line.
colorscheme wombat

" Miscellaneous
if has("win32")
    set grepprg=internal        " Windows findstr.exe just isn't good enough.
endif

if has('mouse')
    set mouse=a                 " Enable mouse support if it's available.
endif

"" Switch on syntax highlighting when the terminal has colors, or when running
" in the GUI. Set the do_syntax_sel_menu flag to tell $VIMRUNTIME/menu.vim
" to expand the syntax menu.
"
" Note: This happens before the 'Autocommands' section below to give the syntax
" command a chance to trigger loading the menus (vs. letting the filetype
" command do it). If do_syntax_sel_menu isn't set beforehand, the syntax menu
" won't get populated.
"
if &t_Co > 2 || has("gui_running")
    let do_syntax_sel_menu=1
    syntax on
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
    "
    " Note: See $HOME/.vim/ftplugin and $HOME/.vim/after/ftplugin for
    " most local filetype autocommands and customizations.
    "
    filetype plugin indent on

    " When editing a file, always jump to the last known cursor
    " position. Don't do it when the position is invalid or when inside
    " an event handler (happens when dropping a file on gvim).
    "
    au BufReadPost *
        \   if line("'\"") > 0 && line("'\"") <= line("$") |
        \       exe "normal g`\"" |
        \   endif

    " Resize Vim windows to equal heights and widths when Vim itself is resized.
    au VimResized * wincmd =

    " Turn off browse dialog filters.
    au FileType * let b:browsefilter = ''

    "
    " Filetype specific autocommands
    "

    " HTML
    au BufNewFile,BufRead *.html setlocal foldmethod=manual

    " Use Shift-Return to turn this:
    "     <tag>|</tag>
    "
    " into this:
    "     <tag>
    "         |
    "     </tag>
    au BufNewFile,BufRead *.html inoremap <buffer> <s-cr> <cr><esc>kA<cr>
    au BufNewFile,BufRead *.html nnoremap <buffer> <s-cr> vit<esc>a<cr><esc>vito<esc>i<cr><esc>

endif

"""
""" Key mappings
"""

" Set 'selection', 'selectmode', 'mousemodel' and 'keymodel' to make
" both keyboard- and mouse-based highlighting behave more like Windows
" and OS X. (These are the same settings you get with `:behave mswin`.)
"
" Note: 'selectmode', 'keymodel', and 'selection' are also set within
" map_movement_keys.vim, since they're critical to the behavior of those
" mappings (although they should be set to the same values there as here.)
"
" Note: Under MacVim, `:let macvim_hig_shift_movement = 1` will cause MacVim
" to set selectmode and keymodel. See `:help macvim-shift-movement` for
" details.
" 
"set selectmode=mouse,key
"set keymodel=startsel,stopsel
"set selection=exclusive
"set mousemodel=popup

" Leaders
let mapleader = "\\"
let maplocalleader = ","

" Move by display lines.
noremap j gj
noremap k gk

" Nuke the help key, instead toggle fullscreen mode.
noremap  <F1> :set invfullscreen<CR>

" Easy buffer navigation.
noremap <C-h> <C-w>h
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l

" Don't move the cursor after pasting.
noremap p p`[
noremap P P`[

" Y behaves as you'd expect.
nnoremap Y y$

" Delete characters into the blackhole buffer.
nnoremap x "_x
nnoremap X "_X

" Backsapce in Visual mode deletes selection.
vnoremap <BS> d

" Turn off search highlighting.
map <leader><space> :noh<cr>

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

" A little Emacs heresy.
inoremap <c-a> <esc>I
inoremap <c-e> <esc>A

"""
""" Filetype, indent, syntax and plugin-specific configuration
"""

" Autoclose
let g:autoclose_on=0            " Turn off autoclose by default.

" Haskell
let g:haskell_indent_if=2
let g:haskell_indent_case=2

" Syntastic
let g:syntastic_enable_signs=1
