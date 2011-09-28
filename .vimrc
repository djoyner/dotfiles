" ~/.vimrc
"
"""
""" Preamble
"""
filetype off
call pathogen#runtime_append_all_bundles()
filetype plugin indent on
set nocompatible                " Use Vim settings instead of vi settings.

"""
""" Settings
"""

" Basics
set autoindent                  " Copy indent from current line for new line.
set backspace=indent,eol,start
set cpoptions+=J
set cursorline
set encoding=utf-8
set fillchars=diff:\ 
set hidden                      " Keep changed buffers without requiring saves.
set history=1000                " Remember this many command lines.
set lazyredraw
set list
set listchars=tab:▸\ ,eol:¬
set matchtime=3
set modelines=0
set ruler                       " Always show the cursur position.
set scrolloff=3                 " Context lines at top and bottom of display.
set shell=/bin/bash
set shortmess+=I                " Don't show the Vim welcome screen.
set showbreak=↪
set showcmd                     " Display incomplete commands.
set showmode
set sidescroll=1                " Number of chars to scroll when scrolling sideways.
set sidescrolloff=5             " Context columns at left and right.
set splitbelow                  " Split new horizontal windows under current window.
set splitright                  " Split new vertical windows right of current window.
set notimeout
set nottimeout
set ttyfast
set visualbell
set wildignore+=*.pyc,.hg,.git
set wildmenu
set wildmode=list:longest

set nosmartindent               " 'smartindent' breaks right-shifting of # lines.

" Searching
set hlsearch                    " Highlight latest search pattern.
set incsearch                   " Do incremental searching.

" Line numbering
set number                      " Display line numbers.
set numberwidth=4               " Minimum number of columns to show for line numbers.
set norelativenumber            " Don't use relative line numbers.

" Tabs, spaces, wrapping
set display=lastline            " Display as much of a window's last line as possible.
set expandtab
set formatoptions=qrn1
set linebreak                   " Wrap at 'breakat' char vs display edge if 'wrap' is on.
set shiftwidth=4
set tabstop=8
set textwidth=85
set nowrap                      " Don't wrap the display of long lines.

" Colors
syntax on
set background=dark
colorscheme wombat

" Status line
set laststatus=2                " Always show a status line.
set statusline=%F%m%r%h%w
set statusline+=\ %#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
set statusline+=%=(%{&ff}/%Y)

" Backups, swap, etc.
set backup
set backupdir=~/.vim/tmp/backup/
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.vim/tmp/swap/
set undodir=~/.vim/tmp/undo/
"
" Resize splits when the window is resized
au VimResized * exe "normal! \<c-w>="
