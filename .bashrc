### ~/.bashrc

## Development environment setup
export P4CONFIG=.p4env
export P4USER=DJoyner
export P4EDITOR="emacs -nw"
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
	    export SCONSFLAGS="-j$CORES distcc=0 opt=0 debug=1"
	else
	    export SCONSFLAGS="-j0 distcc=0 opt=0 debug=1"
	fi

	export LD_LIBRARY_PATH=$P2_CORE/build/il/bld_ccpu_x86/lib:$LD_LIBRARY_PATH
	export PYTHONPATH=$P2_CORE/build/il/pytest
	export SPT_BOARD_TYPE=mock
	export HAL_HWINFO_FILE=$P2_CORE/framework/il/platform/initscripts/testmodule/mcpu/hwinfo/800-5135-1.ini
	;;

    darwin*)
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

    if [ -d "$PHX_CROSS_TOOLS/intel" ];
    then
	source $PHX_CROSS_TOOLS/intel/cce/current/bin/iccvars.sh
	source $PHX_CROSS_TOOLS/intel/idbe/current/bin/idbvars.sh
	[ -d "$PHX_CROSS_TOOLS/intel/vtune" ] && source $PHX_CROSS_TOOLS/intel/vtune/bin/vtunevars.sh
    fi
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
	PATH=$PATH:/opt/local/bin:$HOME/bin:.
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

## bash setup
set -b
shopt -s checkwinsize

## Misc setup
export LS_COLORS="no=00:fi=00:di=01;34:ln=01;36:pi=40;33:so=01;35:bd=40;33;01:cd=40;33;01:or=40;31;01:ex=01;32:*.tar=01;31:*.tgz=01;31:*.arj=01;31:*.taz=01;31:*.lzh=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.gz=01;31:*.bz2=01;31:*.deb=01;31:*.rpm=01;31:*.jpg=01;35:*.png=01;35:*.gif=01;35:*.bmp=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.png=01;35:*.mpg=01;35:*.avi=01;35:*.fli=01;35:*.gl=01;35:*.dl=01;35:"

export PAGER=less

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

alias z='clear'

alias p4cl='p4 changes -s pending -u $P4USER'
alias p4log='p4 filelog'
alias p4o='p4 opened'
alias p4r='p4 changes -s submitted -m 50'
alias p4uo='p4 fstat `find . -type f -perm -u+w -print` | grep "no such"'

alias tc='cd $P2_CORE'
alias il='cd $P2_CORE/framework/il'
alias kernel='cd $P2_CORE/framework/il/kernel/linux-2.6.10_mvl401'
alias learn='cd $P2_CORE/content/traffic/l2l3/il/Generator/Learning'

if [ -n "$PHX_CROSS_TOOLS" ];
then
    alias buildkernel='(cd $P2_CORE; rm build/il/bld_ccpu_mips/kernel/vmlinux; cd $P2_CORE/framework/il/kernel/linux-2.6.10_mvl401; scons -u target=ccpu makeclean=0)'
fi

### end ~/.bashrc
