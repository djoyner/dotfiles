" ~/.gvimrc

"
" GUI-specific Settings
"
set cursorline                  " Highlight the current screen line.
set fillchars+=vert:│           " Use a line-drawing char for pretty vertical splits.
set guicursor+=a:blinkon0       " Turn off cursor blink in all modes.
set guicursor+=v:ver35          " Keep the cursor from obscuring visual selections.
set guioptions=egm              " Remove all the GUI cruft.
set guitablabel=%t\ %m          " Tab labels show filename and modified flag.
set guitabtooltip=%F            " Tab tooltips show the full pathname.
set showtabline=2               " Always show the tab line.
set tabpagemax=100              " Allow many more files to be opened in tabs.

" Platform-specific settings
if has("win32")
    set guifont=Consolas:h10 antialias linespace=1
    
    " Maximize the Win32 GUI window
    if has("autocmd")
        au GUIEnter * simalt ~x
    endif
endif

if has("gui_macvim")
    set guifont=Consolas:h14 antialias linespace=1

    " Make the Mac Vim window as tall as possible.
    set lines=999 columns=140

    " Maximize both horizontally and vertically when entering fullscreen mode.
    set fuoptions=maxvert,maxhorz
endif
