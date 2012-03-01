### ~/.bashrc

## OS-specific environment setup
case "$OSTYPE" in
    linux*)
	export PATH=$PATH:/usr/local/bin:$HOME/bin:.
        export EDITOR=vim VISUAL=vim
	;;

    darwin*)
	export PATH=$PATH:/opt/local/bin:/opt/local/sbin:$HOME/bin:.
        export EDITOR="mvim -f" VISUAL="mvim -f"
	export PY_USE_XMLPLUS=1
	;;

    cygwin*)
	export PATH=$PATH:$HOME/bin:.
        export EDITOR=vim VISUAL=vim
        export CYGWIN="nodosfilewarning tty"
        shopt -s nocaseglob
	;;
esac

## Cabal path setup
if [ -d ~/.cabal ];
then
    export PATH=$PATH:~/.cabal/bin
fi

## Git local path setup
if [ -d /usr/local/git ];
then
    export PATH=$PATH:/usr/local/git/bin
    export MANPATH=$MANPATH:/usr/local/git/man
fi

## Ruby setup
export RUBYOPT=rubygems

## Command prompt setup
export PS1="[\u@\h \W]\\$ "
if [ $TERM != "emacs" ] && [ $TERM != "dumb" ];
then
    export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\007"'
else
    # For emacs shells
    unset PROMPT_COMMAND
fi

## bash setup
set -b
shopt -s checkwinsize
shopt -s histappend
export HISTCONTROL=ignoreboth

## Misc setup
export LS_COLORS="no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jpg=01;35:*.png=01;35:*.gif=01;35:*.bmp=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.png=01;35:*.mpg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:"

export PAGER=less
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

## Misc functions
function findwalkup()
{
    curr=''
    next=$PWD
    while [ "$curr" != "$next" ];
    do
	curr=$next
	if [ -e $curr/$1 ];
	then
	    cd $curr
	else
	    next=`dirname $curr`
	fi
    done
}

function rscreen()
{
    if [ $# -ge 1 ];
    then
	rhost=$1
	shift 1
	ssh $rhost -t TERM=screen /usr/bin/screen -DR $@
    else
	echo "usage: rscreen host [args...]"
    fi
}

## Alias setup
case "$OSTYPE" in
    linux*|cygwin*)
        alias l='/bin/ls -alF --color'
	alias l.='/bin/ls -dF .* --color=tty'
	alias ll='/bin/ls -lF --color=tty'
	alias ls='/bin/ls -F --color'
	;;

    darwin*)
        alias l='/bin/ls -alFG'
	alias l.='/bin/ls -dFG .*'
	alias ll='/bin/ls -lFG'
	alias ls='/bin/ls -FG'
	;;
esac

alias screen='TERM=screen /usr/bin/screen'
alias z='clear'

### end ~/.bashrc
