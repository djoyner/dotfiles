### ~/.profile

## OS-specific environment setup
case "$OSTYPE" in
    linux*)
        PATH=/usr/local/bin:$PATH
        export EDITOR=emacs
        export VISUAL=$EDITOR
        ;;

    darwin*)
        export EDITOR=emacs
        export VISUAL=$EDITOR
        ;;
esac

## Prepend miscellaneous directories to PATH
[ -d ~/bin ]    && PATH=~/bin:$PATH
[ -d ~/go/bin ] && PATH=~/go/bin:$PATH

## Other environment setup
[ -d /usr/local/go ] && export GOROOT=/usr/local/go
[ -d ~/go ] && export GOPATH=~/go

## Local environment customization
[ -f ~/.profile.local ] && . ~/.profile.local

## If running bash, source ~/.bashrc
[ -n "$BASH_VERSION" -a -z "$POSIXLY_CORRECT" -a -f ~/.bashrc ] && . ~/.bashrc

## Finally, append . to the PATH
export PATH=$PATH:.

### end ~/.profile
