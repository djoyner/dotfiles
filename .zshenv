## Prepend miscellaneous directories to PATH
[ -d ~/bin ] && PATH=~/bin:$PATH
[ -d ~/go/bin ] && PATH=~/go/bin:$PATH

## Bootstrap Nix
[ -e ~/.nix-profile/etc/profile.d/nix.sh ] && . ~/.nix-profile/etc/profile.d/nix.sh

## Bootstrap Cargo
[ -d ~/.cargo/bin ] && PATH=~/.cargo/bin:$PATH

## Bootstrap pyenv
if [ -d ~/.pyenv ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    PATH=$PYENV_ROOT/bin:$PATH
fi
