"""
""" Load Plugins
"""
""" NB: git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
"""

" Preamble
set nocompatible                " Use Vim settings instead of vi settings.
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" Vundle (required)
Plugin 'VundleVim/Vundle.vim'

" VIM enhancements
Plugin 'ciaranm/securemodelines'
Plugin 'editorconfig/editorconfig-vim'
Plugin 'airblade/vim-rooter'
Plugin 'tpope/vim-commentary'
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'
Plugin 'LucHermitte/lh-vim-lib'
Plugin 'LucHermitte/lh-brackets'
Plugin 'voldikss/vim-floaterm'
Plugin 'farmergreg/vim-lastplace'
Plugin 'sickill/vim-pasta'

" GUI enhancements
Plugin 'chriskempson/base16-vim'
Plugin 'itchyny/lightline.vim'
Plugin 'machakann/vim-highlightedyank'
Plugin 'andymass/vim-matchup'
Plugin 'tpope/vim-fugitive'

" Semantic language support
Plugin 'neovim/nvim-lspconfig'
Plugin 'hrsh7th/cmp-nvim-lsp'
Plugin 'hrsh7th/cmp-buffer'
Plugin 'hrsh7th/cmp-path'
Plugin 'hrsh7th/cmp-cmdline'
Plugin 'hrsh7th/nvim-cmp'
Plugin 'nvim-lua/lsp_extensions.nvim'
Plugin 'ray-x/lsp_signature.nvim'
Plugin 'AndrewRadev/splitjoin.vim'

" Only because nvim-cmp requires snippets
Plugin 'hrsh7th/cmp-vsnip'
Plugin 'hrsh7th/vim-vsnip'

" Syntactic language support
let g:polyglot_disabled = ['rust']
Plugin 'sheerun/vim-polyglot'
Plugin 'simrat39/rust-tools.nvim'

" rust-tools dependencies
Plugin 'nvim-lua/plenary.nvim'
Plugin 'nvim-telescope/telescope.nvim'
Plugin 'nvim-telescope/telescope-ui-select.nvim'

" Postamble
call vundle#end()
filetype plugin indent on

"""
""" Editor Settings
"""

" Basics
set autoindent                  " Copy indent from current line for new line.
set autoread                    " Automatically re-read files changed (but not deleted) on disk.
set backspace=indent,eol,start  " Backspace over everything in indent mode.
set browsedir=buffer            " Browse from the directory of the related buffer.
set cindent                     " Recommended setting for C-style indentation.
set copyindent                  " Copy the previous indentation on autoindenting.
set encoding=utf-8              " Set character encoding.
set fillchars=diff:\            " Character to use for deleted lines in diff output.
set hidden                      " Allow hiding buffers with unsaved changes.
set inccommand=nosplit          " Preview incremental substitution results.
set nojoinspaces                " Don't get fancy with the spaces when joining lines.
set laststatus=2                " Always show a status line.
set lazyredraw                  " Don't redraw the screen unnecessarily.
set nolist                      " Turn 'list' off by default.
set listchars=tab:▸-            " Start and body of tabs.
set listchars+=nbsp:¬           " Non-breakable spaces.
set listchars+=trail:·          " Trailing spaces.
set listchars+=extends:»        " Last column when line extends off right.
set listchars+=precedes:«       " First column when line extends off left.
set listchars+=eol:¬            " End of line.
set matchpairs=(:),{:},[:],<:>  " Character pairs for use with %, 'showmatch'
set matchtime=3                 " In tenths of seconds.
set mouse=a                     " Enable mouse support (all modes).
set scrolloff=8                 " Context lines at top and bottom of display.
set shortmess+=c                " Don't show ins-completion-menu messages.
set shortmess+=I                " Don't show the Vim welcome screen.
set showcmd                     " Display incomplete commands.
set showmatch                   " When a bracket is inserted, briefly jump to the matching one.
set noshowmode                  " Don't show Insert, Replace or Visual on the last line.
set signcolumn=yes:2            " Always draw sign column.
set nosmartindent               " 'smartindent' breaks right-shifting of # lines.
set notimeout                   " Do not time out waiting for key code sequences.
set nottimeout                  " Likewise in the TUI.
set virtualedit=block           " Allow virtual editing when in Visual Block mode.
set visualbell                  " Use visual bell.
set wildignore+=*.pyc,.hg,.git  " Ignored when completion matching.
set wildmenu                    " Use menu for completions.
set wildmode=list:longest       " List all matches and complete till longest common string.

" Searching
set ignorecase                  " Ignore case for pattern matches (\C overrides).
set incsearch                   " Do incremental searching.
set smartcase                   " Override 'ignorecase' if pattern contains uppercase.

if executable("rg")
    set grepprg=rg\ --no-heading\ --vimgrep
    set grepformat=%f:%l:%c:%m
endif

" Line numbering
set number                      " Display line numbers.
set numberwidth=4               " Minimum number of columns to show for line numbers.
set norelativenumber            " Don't use relative line numbers.

" Tabs, spaces, wrapping
set display=lastline            " Display as much of a window's last line as possible.
set expandtab                   " Insert spaces for <Tab> press; use spaces to indent.
set formatoptions=tc            " Wrap text and comments using textwidth.
set formatoptions+=r            " Continue comments when pressing ENTER in insert mode.
set formatoptions+=q            " Allow formatting of comments with 'gq'.
set formatoptions+=n            " Recognize numbered lists.
set formatoptions+=b            " Auto-wrap in insert mode, and do not wrap old long lines.
set formatoptions+=1            " Don't break a line after a one-letter word.
set linebreak                   " Wrap at 'breakat' char vs display edge if 'wrap' is on.
set shiftround                  " Round indent to a multiple of 'shiftwidth'.
set shiftwidth=2                " Number of spaces to use for indent and unindent.
set showbreak=↪                 " String to put at start of lines that have been wrapped.
set sidescroll=1                " Number of chars to scroll when scrolling sideways.
set sidescrolloff=8             " Context columns at left and right.
set smarttab                    " Tab respects 'shiftwidth', 'tabstop', 'softtabstop'.
set softtabstop=8               " Edit as if tabs are 8 characters wide.
set tabstop=8                   " Set the visible width of tabs.
set textwidth=0                 " Don't auto-wrap lines except for specific filetypes.
set whichwrap+=<,>,[,]          " Allow left/right arrows to move across lines.
set nowrap                      " Don't wrap the display of long lines.

if executable("par")
    set formatprg=par\ -w78
endif

" Completion
" menu: Use a popup to show completions
" menuone: Popup even when there's only one match
" noinsert: Do not insert text until a selection is made
" noselect: Do not select, force user to select one from the menu
set completeopt=menu,menuone,noinsert,noselect
set cmdheight=2                 " Better display for messages
set updatetime=300              " You will have bad experience for diagnostic messages when it's default 4000.

" Diffing
set diffopt+=iwhite             " No whitespace in vimdiff.
set diffopt+=algorithm:patience " Make diffing better: https://vimways.org/2018/the-power-of-diff/
set diffopt+=indent-heuristic

" Folds
set foldcolumn=3                " Number of columns to show at left for folds.
set foldenable                  " Enable folding.
set foldnestmax=3               " Only allow 3 levels of folding.
set foldlevelstart=99           " Start with all folds open.

" Windowing
set splitbelow                  " Split new horizontal windows under current window.
set splitright                  " Split new vertical windows right of current window.
set winminheight=0              " Allow windows to shrink to status line.
set winminwidth=0               " Allow windows to shrink to vertical separator.

" Printing
set printfont=:h10
set printencoding=utf-8
set printoptions=paper:letter

" Backups, swap, etc.
set backup
set backupdir=~/.vim/tmp/backup
set backupskip=/tmp/*,/private/tmp/*
set directory=~/.vim/tmp/swap
set undodir=~/.vim/tmp/undo
set undofile

if !isdirectory(expand(&backupdir))
    call mkdir(expand(&backupdir), "p")
endif

if !isdirectory(expand(&directory))
    call mkdir(expand(&directory), "p")
endif

if !isdirectory(expand(&undodir))
    call mkdir(expand(&undodir), "p")
endif

" Colors
if !has('gui_running')
  set t_Co=256
endif

set background=dark             " Terminal is always dark.
set nocursorcolumn              " Don't highlight the current screen column.
set nocursorline                " Don't highlight the current screen line.
set guicursor=n-v-c:block-Cursor/lCursor-blinkon0,i-ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor
set hlsearch                    " Highlight the latest search pattern.
set termguicolors               " Enable 24-bit RGB color.

colorscheme base16-tomorrow-night
syntax on

" Don't ever color the terminal background
hi Normal ctermbg=NONE

" Make comments more prominent -- they are important.
"call Base16hi("Comment", g:base16_gui09, "", g:base16_cterm09, "", "", "")

" Make it clearly visible which argument we're at.
call Base16hi("LspSignatureActiveParameter", g:base16_gui05, g:base16_gui03, g:base16_cterm05, g:base16_cterm03, "bold", "")

"""
""" LSP Settings
"""
lua << END

  -- Setup nvim-cmp.
  local has_words_before = function()
    local line, col = unpack(vim.api.nvim_win_get_cursor(0))
    return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match('%s') == nil
  end

  local cmp = require'cmp'

  cmp.setup({
    snippet = {
      expand = function(args)
        vim.fn["vsnip#anonymous"](args.body)
      end,
    },

    mapping = {
      ['<CR>'] = cmp.mapping.confirm {
        behavior = cmp.ConfirmBehavior.Insert,
        select = true,
      },

      ['<Tab>'] = function(fallback)
        if not cmp.select_next_item() then
          if vim.bo.buftype == 'prompt' and has_words_before() then
            cmp.complete()
          else
            fallback()
          end
        end
      end,

      ['<S-Tab>'] = function(fallback)
        if not cmp.select_prev_item() then
          if vim.bo.buftype == 'prompt' and has_words_before() then
            cmp.complete()
          else
            fallback()
          end
        end
      end,

      ['<Up>'] = cmp.mapping(cmp.mapping.select_prev_item(), { 'i', 'c' }),
      ['<Down>'] = cmp.mapping(cmp.mapping.select_next_item(), { 'i', 'c' }),
    },

    sources = cmp.config.sources({
      { name = 'nvim_lsp' },
    }, {
      { name = 'path' },
    }),
  })

  -- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline('/', {
    sources = {
      { name = 'buffer' }
    }
  })

  -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
  cmp.setup.cmdline(':', {
    sources = cmp.config.sources({
      { name = 'path' },
    }, {
      { name = 'cmdline' },
    }),
  })

  -- Setup lspconfig
  local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())

  local on_attach = function(client, bufnr)
    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    -- Enable completion triggered by <c-x><c-o>
    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Mappings.
    local opts = { noremap=true, silent=true }

    -- See `:help vim.lsp.*` for documentation on any of the below functions
    buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
    buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
    buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
    buf_set_keymap('n', '<localleader>a', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
    buf_set_keymap('n', '<localleader>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
    buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
    buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
    buf_set_keymap('n', '<localleader>k', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    buf_set_keymap('n', '<localleader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
    buf_set_keymap('n', '<localleader>r', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
    buf_set_keymap('n', '<localleader>t', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)

    -- Get signatures (and _only_ signatures) when in argument lists.
    require "lsp_signature".on_attach({
      doc_lines = 0,
      handler_opts = {
        border = "none"
      },
    })
  end

  local lspconfig = require'lspconfig'

  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
      virtual_text = true,
      signs = true,
      update_in_insert = true,
    }
  )

  -- Setup telescope
  -- NB: brew install fd
  require('telescope').setup({
    pickers = {
      find_files = {
        find_command = { "fd", "--type", "file", "--follow", "--strip-cwd-prefix" }
      }
    },
    extensions = {
      ["ui-select"] = {
        require("telescope.themes").get_dropdown {
            -- even more opts
        }
      }
    },
  })

  require("telescope").load_extension("ui-select")

  -- Setup rust-tools
  require('rust-tools').setup({
    tools = {
      inlay_hints = {
        only_current_line = true,
      },
    },
    server = {
      on_attach = on_attach,
      settings = {
        ["rust-analyzer"] = {
          assist = {
            importGranularity = "module",
            importPrefix = "by_self",
          },
          cargo = {
            loadOutDirsFromCheck = true,
          },
          completion = {
            postfix = {
              enable = false,
            },
          },
          procMacro = {
            enable = true,
          },
        },
      },
    },
  })

  --vim.lsp.set_log_level("debug")

END

"""
""" Plugin Settings
"""

" editorconfig
let g:EditorConfig_exclude_patterns = ['fugitive://.*']

" floaterm
let g:floaterm_keymap_toggle = '<F1>'
let g:floaterm_keymap_next = '<F2>'
let g:floaterm_keymap_prev = '<F3>'
let g:floaterm_keymap_new = '<F4>'

let g:floaterm_gitcommit='floaterm'
let g:floaterm_autoinsert=1
let g:floaterm_width=0.8
let g:floaterm_height=0.8
let g:floaterm_wintitle=0
let g:floaterm_autoclose=1

" lh-brackets
let g:cb_disable_default = { '[': 'nv', '{': 'nv' }
let g:marker_define_jump_mappings = 0
let g:usemarks = 0

" lightline
let g:lightline = {
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ],
      \             [ 'readonly', 'filename', 'gitbranch', 'modified' ] ],
      \   'right': [ [ 'lineinfo' ],
      \              [ 'percent' ],
      \              [ 'fileencoding', 'filetype' ] ],
      \ },
      \ 'component_function': {
      \   'filename': 'LightlineFilename',
      \   'gitbranch': 'FugitiveHead'
      \ },
      \ }

" pasta
let g:pasta_disabled_filetypes = ['fugitive']

" rust
let g:rustfmt_autosave = 1
let g:rustfmt_emit_files = 1
let g:rustfmt_fail_silently = 0

" securemodelines
let g:secure_modelines_allowed_items = [
                \ "textwidth",   "tw",
                \ "softtabstop", "sts",
                \ "tabstop",     "ts",
                \ "shiftwidth",  "sw",
                \ "expandtab",   "et",   "noexpandtab", "noet",
                \ "filetype",    "ft",
                \ "foldmethod",  "fdm",
                \ "readonly",    "ro",   "noreadonly", "noro",
                \ "rightleft",   "rl",   "norightleft", "norl",
                \ "colorcolumn"
                \ ]

" vim-markdown
let g:vim_markdown_new_list_item_indent = 0
let g:vim_markdown_auto_insert_bullets = 0
let g:vim_markdown_frontmatter = 1

" vim-rooter
let g:rooter_patterns = ['.git', 'package.json', '!node_modules']

"""
""" Autocommands
"""

" Auto-format on save
au BufWritePre *.rs lua vim.lsp.buf.formatting_sync(nil, 1000)

" Resize Vim windows to equal heights and widths when Vim itself is resized.
au VimResized * wincmd =

augroup FloatermCustomisations
    autocmd!
    autocmd ColorScheme * highlight FloatermBorder guibg=none
augroup END

"""
""" Leaders
"""
let mapleader = "\<Space>"
let maplocalleader = ","

nmap <leader>b <cmd>lua require('telescope.builtin').buffers()<cr> 
nmap <leader>f <cmd>lua require('telescope.builtin').find_files({cwd=vim.fn.FindRootDirectory()})<cr>
nmap <leader>r <cmd>lua require('rust-tools.runnables').runnables()<cr>
nmap <leader>s <cmd>lua require('telescope.builtin').live_grep({previewer=false})<cr>
nmap <leader>g <cmd>Git<cr>
nmap <leader>w <cmd>w<cr>

" <leader><leader> toggles between buffers.
nnoremap <leader><leader> <c-^>

" Delete all buffers.
nmap <leader>Q :bufdo bdelete<cr>

" Edit another file in the same directory as the current file.
nmap <leader>e :e <c-r>=expand("%:p:h") . '/'<cr>

" Rename current file.
nmap <leader>R <cmd>call RenameFile()<cr>

" Toggle invisible characters.
nmap <leader>, <cmd>set list!<cr>

" Clear highlighting of last search pattern.
nmap <leader>/ <cmd>noh<cr>

" Just one space.
nmap <leader>\ <cmd>call JustOneSpace()<cr>

" Untabify/tabify.
nmap <leader><tab> <cmd>retab!<cr>
nmap <leader><s-tab> <cmd>set noexpandtab<cr>:retab!<cr>:set expandtab<cr>

" Delete trailing whitespace.
nmap <leader><bs> <cmd>call Preserve("%s/\\s\\+$//e")<cr>

" Execute selection as Vimscript.
vnoremap <leader>E y:@"<cr>
nnoremap <leader>E yy:@"<cr>

" Easier cut/copy/paste to/from the system clipboard.
noremap <leader>x "*x
noremap <leader>y "*y
nnoremap <leader>Y :%y "*<cr>
noremap <leader>p :set paste<cr>"*p<cr>:set nopaste<cr>
noremap <leader>P :set paste<cr>"*P<cr>:set nopaste<cr>

" Make it easier to work with init.vim.
nmap <leader>ve :edit ~/.config/nvim/init.vim<cr>
nmap <leader>vr :source ~/.config/nvim/init.vim<cr>

"""
""" Other key mappings
"""

" Disable arrow keys in command mode.
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>

" Move by display lines.
noremap <silent> j gj
noremap <silent> k gk

" Easy window navigation.
noremap <c-h> <c-w>h
noremap <c-j> <c-w>j
noremap <c-k> <c-w>k
noremap <c-l> <c-w>l

" Y behaves as you'd expect.
nnoremap Y y$

" Backsapce in visual mode deletes selection.
vnoremap <bs> d

" Default searches to 'very magic', more like the world outside Vim.
nnoremap / /\v
vnoremap / /\v
nnoremap ? ?\v
vnoremap ? ?\v

" Tab/shift-tab to indent/outdent in visual mode.
vnoremap <tab> >gv
vnoremap <s-tab> <gv

" Keep selection when indenting/outdenting.
vnoremap > >gv
vnoremap < <gv

" Maintain the cursor position when yanking a visual selection.
" http://ddrscott.github.io/blog/2016/yank-without-jank/
vnoremap y myy`y
vnoremap Y myy$`y

" Center the display line after searches and jumps.
nnoremap <silent> n nzz
nnoremap <silent> N Nzz
nnoremap <silent> * *zz
nnoremap <silent> # #zz
nnoremap <silent> g* g*zz
nnoremap <silent> g# g#zz
nnoremap <silent> { {zz
nnoremap <silent> } }zz

" In command mode, type %% to insert the path of the currently edited file, as a shortcut for %:h<tab>.
cmap %% <C-R>=expand("%:h") . "/" <CR>

" Center line on previous/next fix.
map - :cprev<cr> zz
map + :cnext<cr> zz

" Center line in previous/next file.
map g- :cpfile<cr> zz
map g+ :cnfile<cr> zz

" Disable man page lookups
map K <nop>

" Disable ex mode
map Q <nop>

" Wean off Emacs...
map <c-x> <nop>

"""
""" Custom Commands and Functions
"""

function! LightlineFilename()
  return expand('%:t') !=# '' ? @% : '[No Name]'
endfunction

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
