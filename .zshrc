# Prelude to handle Tramp
# ref: https://www.emacswiki.org/emacs/TrampMode (Troubleshooting)
if [[ "$TERM" == "dumb" ]]
then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    if whence -w precmd >/dev/null; then
        unfunction precmd
    fi
    if whence -w preexec >/dev/null; then
        unfunction preexec
    fi
    unset zle_bracketed_paste
    PS1='$ '
    return
fi

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="${HOME}/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
# ZSH_THEME="robbyrussell"
ZSH_THEME="essembeh"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# Caution: this setting can cause issues with multiline prompts (zsh 5.7.1 and newer seem to work)
# See https://github.com/ohmyzsh/ohmyzsh/issues/5765
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    docker
    ripgrep
)

source $ZSH/oh-my-zsh.sh

# User configuration

## Prepend miscellaneous directories to PATH
[ -d ~/.local/bin ] && PATH=~/.local/bin:$PATH
[ -d ~/.toolbox/bin ] && PATH=~/.toolbox/bin:$PATH
[ -d ~/bin ] && PATH=~/bin:$PATH
[ -d ~/go/bin ] && PATH=~/go/bin:$PATH

## Bootstrap Nix
[ -e ~/.nix-profile/etc/profile.d/nix.sh ] && . ~/.nix-profile/etc/profile.d/nix.sh

## Bootstrap Cargo
[ -d ~/.cargo/bin ] && PATH=~/.cargo/bin:$PATH

## Bootstrap NVM
if [ -d ~/.nvm ]; then
    export NVM_DIR="$HOME/.nvm"
    [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
fi

## Bootstrap pyenv
if [ -d ~/.pyenv ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    PATH=$PYENV_ROOT/bin:$PATH
fi

## Load directory colors
if [ -n "$(type -p dircolors)" ]; then
    eval "$(dircolors -b)"
fi

## Editor config
if [[ -n "$SSH_CONNECTION" ]]; then
    export EDITOR=vim
else
    export EDITOR="emacsclient -t"
    export VISUAL="emacsclient -c -a emacs"
fi

## Pager config
unset LESS
export PAGER=less
export MANPAGER=$PAGER
export LESSHISTFILE=/dev/null

## Make less more friendly for non-text input files, see lesspipe(1)
if [ -n "$(type -p lesspipe.sh)" ]; then
    export LESSOPEN="| lesspipe.sh %s" LESS_ADVANCED_PREPROCESSOR=1
fi

## Turn off stop (^S) control character
stty stop undef

## Don't echo control characters
stty -echoctl

## Incrementally expand history and don't record useless commands
setopt extended_history
setopt hist_expire_dups_first
setopt hist_find_no_dups
setopt inc_append_history
unsetopt hist_verify

HISTSIZE=1000
SAVEHIST=2000
HISTORY_IGNORE="(ls|ll|l|bg|fg|history|clear|exit)"

## Aliases
if env ls --version 2>&1 | grep -q GNU; then
    alias l='env ls -alF --color=auto'
    alias l.='env ls -dF .* --color=auto'
    alias ll='env ls -lF --color=auto'
    alias ls='env ls --color=auto'
else
    alias l='env ls -alFG'
    alias l.='env ls -dFG .*'
    alias ll='env ls -lFG'
    alias ls='env ls -G'
fi

## Command completions
if [ -n "$(type -p aws_completer)" ]; then
    complete -C aws_completer aws
fi

## Load up "z"
. ~/bin/z.sh

## Go-related environment setup
[ -d /usr/local/go ] && export GOROOT=/usr/local/go
[ -d ~/go ] && export GOPATH=~/go

## Local environment customization
[ -f ~/.zshrc.local ] && . ~/.zshrc.local

## Finally, append . to the PATH
export PATH=$PATH:.
