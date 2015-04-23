### ~/.profile

## OS-specific environment setup
case "$OSTYPE" in
    linux*)
        export PATH=/usr/local/bin:$PATH
        export EDITOR="emacs -nw"
        ;;

    darwin*)
        export PATH=/usr/local/bin:$PATH
        export EDITOR="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
        ;;

    cygwin*)
        export EDITOR=vim
        export CYGWIN="nodosfilewarning tty"
        ;;
esac

export VISUAL=$EDITOR

## Prepend miscellaneous directories to PATH
[ -d ~/bin ]                        && PATH=~/bin:$PATH
[ -d ~/.cabal/bin ]                 && PATH=~/.cabal/bin:$PATH
[ -d ~/go/bin ]                     && PATH=~/go/bin:$PATH
[ -d ~/Library/Haskell/bin ]        && PATH=~/Library/Haskell/bin:$PATH
[ -d /usr/local/git ]               && PATH=/usr/local/git/bin:$PATH
[ -d /usr/local/go ]                && PATH=/usr/local/go/bin:$PATH

## Finally, append . to the PATH
export PATH=$PATH:.

## Local environment customization
[ -f ~/.profile.local ] && . ~/.profile.local

## If running bash, source ~/.bashrc
[ -n "$BASH_VERSION" -a -z "$POSIXLY_CORRECT" -a -f ~/.bashrc ] && . ~/.bashrc

### end ~/.profile
