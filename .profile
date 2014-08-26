### ~/.profile

## OS-specific environment setup
case "$OSTYPE" in
    linux*)
        export PATH=/usr/local/bin:$HOME/bin:$PATH
        export EDITOR=vim VISUAL=vim
        ;;

    darwin*)
        export PATH=/usr/local/bin:/usr/local/sbin:/opt/local/bin:/opt/local/sbin:/usr/local/git/bin:$HOME/bin:$PATH
        export EDITOR="mvim -f" VISUAL="mvim -f"
        export PY_USE_XMLPLUS=1
        ;;

    cygwin*)
        export PATH=$HOME/bin:$PATH
        export EDITOR=vim VISUAL=vim
        export CYGWIN="nodosfilewarning tty"
        ;;
esac

## Prepend miscellaneous directories to PATH
[ -d ~/bin ]                 && PATH=~/bin:$PATH
[ -d ~/.cabal/bin ]          && PATH=~/.cabal/bin:$PATH
[ -d ~/Library/Haskell/bin ] && PATH=~/Library/Haskell/bin:$PATH

## Finally, append . to the PATH
export PATH=$PATH:.

## Common environment setup
[ -d /usr/local/go ] && export GOROOT=/usr/local/go
export RUBYOPT=rubygems

## If running bash, source ~/.bashrc
[ -n "$BASH_VERSION" -a -z "$POSIXLY_CORRECT" ] && . "$HOME/.bashrc"

### end ~/.profile
