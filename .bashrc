### ~/.bashrc

## Development environment setup
export P4CONFIG=.p4env
export P4USER=DJoyner
export P4VRES=/usr/local/share/P4VResources

if [ "$OSTYPE" != "cygwin" ];
then
    export MAINLINE=~/working/TestCenter/mainline
    export P2_CORE=~/working/TestCenter/p2_core
    export CCACHE_DIR=~/working/.ccache
    export USE_CCACHE=1
    export SVN_EDITOR=$P4EDITOR
else
    export MAINLINE=c:/working/TestCenter/mainline
    export P2_CORE=c:/working/TestCenter/p2_core
    export SVN_EDITOR=c:/emacs-22.1/bin/emacsclient
fi

case "$OSTYPE" in
    linux*)
	CORES=`grep processor /proc/cpuinfo | wc -l`
	if [ $CORES -gt 2 ];
	then
	    export SCONSFLAGS="--warn=no-deprecated -j$CORES distcc=0"
	else
	    export SCONSFLAGS="--warn=no-deprecated -j0 distcc=0"
	fi

	export LD_LIBRARY_PATH=$P2_CORE/build/il/bld_ccpu_i386_pentium3/lib:$LD_LIBRARY_PATH
	export SPT_BOARD_TYPE=mock
	export HAL_HWINFO_FILE=$P2_CORE/framework/il/platform/initscripts/testmodule/mcpu/hwinfo/800-5135-1.ini
	;;

    darwin*)
	export PY_USE_XMLPLUS=1
	;;

    cygwin*)
	;;
esac

## git path setup
if [ -d /usr/local/git ];
then
    PATH=$PATH:/usr/local/git/bin
    export MANPATH=$MANPATH:/usr/local/git/man
fi

## Ruby setup
export RUBYOPT=rubygems

## Phoenix cross tools setup
if [ -d /export/crosstools ];
then
    export PHX_CROSS_TOOLS=/export/crosstools
fi

if [ -n "$PHX_CROSS_TOOLS" ];
then
    PATH=$PATH:$PHX_CROSS_TOOLS/mips/fp_be/bin
    PATH=$PATH:$PHX_CROSS_TOOLS/ppc/405/bin
    PATH=$PATH:$PHX_CROSS_TOOLS/x86/pentium3/bin
fi

if [ -d "$P2_CORE/build/il/cli" ];
then
    PATH=$PATH:$P2_CORE/build/il/cli
fi

## Additional Path setup
case "$OSTYPE" in
    linux*)
	PATH=$PATH:/usr/local/bin:$HOME/bin:.
	;;

    darwin*)
	PATH=$PATH:/opt/local/bin:/opt/local/sbin:$HOME/bin:.
	;;

    cygwin*)
	;;
esac

export PATH

## Command prompt setup
export PS1="[\u@\h \W]\\$ "
if [ $TERM != "emacs" ] && [ $TERM != "dumb" ];
then
    export PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}\007"'
else
    # For emacs shells
    unset PROMPT_COMMAND
fi

[ -r ~/bin/j.sh ] && source ~/bin/j.sh

## bash setup
set -b
shopt -s checkwinsize
shopt -s histappend
export HISTCONTROL=ignoreboth

## Misc setup
export LS_COLORS="no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jpg=01;35:*.png=01;35:*.gif=01;35:*.bmp=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.png=01;35:*.mpg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:"

if [ -z "$DISPLAY" ]; then
    export EDITOR=emacs P4EDITOR=emacs VISUAL=emacs
    export ALTERNATE_EDITOR=emacs 
else
    export EDITOR=emacsclient P4EDITOR=emacsclient VISUAL=emacsclient
    export ALTERNATE_EDITOR=emacs 
fi

export PAGER=less
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

## Misc functions
function find_walk_up()
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

alias p4cl='p4 changes -s pending -u $P4USER'
alias p4log='p4 filelog'
alias p4o='p4 opened'
alias p4r='p4 changes -s submitted -m 50'
alias p4uo='exec 3>&1; find . -type f -print0 | xargs -0 p4 fstat 2>&1 >&3 3>&1- | cut -f1 -d" " 3>&-; exec 3>&-'
alias p4which='p4 changes -m1 ...'

alias stc='cd $P2_CORE'
alias stctop='find_walk_up .p4env'
alias kernel='cd $P2_CORE/framework/il/kernel/linux-2.6.10_mvl401'

if [ -n "$PHX_CROSS_TOOLS" ];
then
    alias buildkernel='(cd $P2_CORE; rm build/il/bld_ccpu_broadcom_kernel_mips_fp_be/kernel/vmlinux; scons target=ccpu_broadcom_kernel arch=mips makeclean=0)'
fi

### end ~/.bashrc
