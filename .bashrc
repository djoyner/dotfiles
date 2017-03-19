### ~/.bashrc

## If not running interactively, don't do anything
[ -z "$PS1" ] && return

## Common bash setup

# Report status of terminated background jobs immediately
set -b

# Auto-update LINES and COLUMNS
shopt -s checkwinsize

# Append to history file, ignoring spaces and duplicates, expand history, useful timestamp format
shopt -s histappend
HISTCONTROL="erasedups:ignoreboth"
HISTSIZE=1000
HISTFILESIZE=2000
HISTTIMEFORMAT='%F %T '

# Don't record some commands
export HISTIGNORE="&:[ ]*:exit:ls:bg:fg:history:clear"

# Save multi-line commands as one command
shopt -s cmdhist

# Match filenames in case-insensitive fashion
shopt -s nocaseglob

# Don't search PATH for possible completions when completion is attempted on
# an empty command line
shopt -qs no_empty_cmd_completion

# Allows space to complete and expand !$
bind Space:magic-space

# Perform file completion in a case insensitive fashion
bind "set completion-ignore-case on"

# Display matches for ambiguous patterns at first tab press
bind "set show-all-if-ambiguous on"

## Enable 256-color terminal support
if [ "$TERM" = "xterm" ] ; then
    if [ -z "$COLORTERM" ] ; then
        if [ -z "$XTERM_VERSION" ] ; then
            echo "Warning: Terminal is wrongly calling itself 'xterm'."
        else
            TERM="xterm-256color"
        fi
    else
        case "$COLORTERM" in
            gnome-terminal|mate-terminal)
                TERM="xterm-256color"
                ;;
            *)
                echo "Warning: Unrecognized COLORTERM: $COLORTERM"
                ;;
        esac
    fi
fi

## Other environment setup
[ -d /usr/local/go ] && export GOROOT=/usr/local/go
[ -d ~/go ] && export GOPATH=~/go

## Other look and feel

# Colorized prompt
if [ -n "$(type -p tput)" ] && tput setaf 1 >&/dev/null; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[01;34m\] \w \[\033[00m\]\$ '
else
    PS1="${debian_chroot:+($debian_chroot)}\u@\h \w \$ "
fi

# Set window title
if [ $TERM != "emacs" -a $TERM != "eterm-color" -a $TERM != "dumb" ];
then
    PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\007"'
fi

# Load directory colors
if [ -n "$(type -p dircolors)" ]; then
    eval "$(dircolors -b)"
fi

# Use color in grep output
export GREP_OPTIONS='--color=auto'

# Make less more friendly for non-text input files, see lesspipe(1)
if [ -n "$(type -p lesspipe)" ]; then
    eval "$(SHELL=/bin/sh lesspipe)"
fi

export PAGER=less
export MANPAGER=$PAGER

# Turn off stop (^S) control character
stty stop undef

# Don't echo control characters
stty -echoctl

## Aliases
case "$OSTYPE" in
    linux*)
        alias l='env ls -alF --color=auto'
        alias l.='env ls -dF .* --color=auto'
        alias ll='env ls -lF --color=auto'
        alias ls='env ls --color=auto'
        ;;

    darwin*)
        alias l='env ls -alFG'
        alias l.='env ls -dFG .*'
        alias ll='env ls -lFG'
        alias ls='env ls -G'
        ;;
esac

## Local environment customization
[ -f ~/.bashrc.local ] && . ~/.bashrc.local

### end ~/.bashrc
