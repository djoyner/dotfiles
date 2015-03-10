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
[ -d ~/Applications/ghc-7.8.4.app ] && PATH=~/Applications/ghc-7.8.4.app/Contents/bin:$PATH
[ -d ~/Library/Haskell/bin ]        && PATH=~/Library/Haskell/bin:$PATH
[ -d /usr/local/git ]               && PATH=/usr/local/git/bin:$PATH
[ -d /usr/local/go ]                && PATH=/usr/local/go/bin:$PATH
[ -d /usr/local/packer ]            && PATH=/usr/local/packer:$PATH

## Finally, append . to the PATH
export PATH=$PATH:.

## Common environment setup
[ -e ~/.nix-profile/etc/profile.d/nix.sh ] && . ~/.nix-profile/etc/profile.d/nix.sh

## If running bash, source ~/.bashrc
[ -n "$BASH_VERSION" -a -z "$POSIXLY_CORRECT" ] && . "$HOME/.bashrc"

### end ~/.profile
