#!/bin/bash

# Exit the script if any errors are encountered.
set -e

term_pid=$(ps -p $$ -o ppid=)
tty="/dev/$(ps h -p $term_pid -o tty)"
num_cols=$(stty -a <"$tty" | grep -Po '(?<=columns )\d+')

echo_fill () {
    text_size=${#1}
    rem_cols=$((num_cols-text_size-2))
    left_cols=$((rem_cols/2))
    right_cols=$left_cols
    if [ $((rem_cols%2)) -eq 1 ]; then
        right_cols=$((right_cols+1))
    fi
    printf -- '-%.0s' $(seq 1 $left_cols)
    printf " $1 "
    printf -- '-%.0s' $(seq 1 $right_cols)
    echo
}

skip_emacs=false
while [ $# -gt 0 ]; do
    case $1 in
        --skip-emacs)
            skip_emacs=true
            ;;
        *)
            echo "Invalid option: $1" >&2
            exit 1
            ;;
    esac
    shift
done

# dnf
echo_fill "DNF"
sudo dnf -y update

# flatpak
echo_fill "FLATPAK"
! which flatpak || flatpak -y update

# rust
echo_fill "RUST"
rustup update

# go
echo_fill "GO"
go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest

# nwg-look
echo_fill "NWG-LOOK"
[ -d $HOME/src/public/nwg-look ] || git clone https://github.com/nwg-piotr/nwg-look.git $HOME/src/public/nwg-look
cd $HOME/src/public/nwg-look
git fetch
if [ $(git rev-parse @) != $(git rev-parse @{u}) ]; then
    git pull
    make build
    sudo make install
fi

# emacs
if [ $skip_emacs = false ]; then
    echo_fill "EMACS"
    mkdir -p $HOME/src/public/apps
    [ -d $HOME/src/public/apps/emacs ] || git clone git://git.sv.gnu.org/emacs.git $HOME/src/public/apps/emacs
    cd $HOME/src/public/apps/emacs
    git clean -dfx
    git fetch
    if [ $(git rev-parse @) != $(git rev-parse @{u}) ]; then
        git pull
        ./autogen.sh
        ./configure --without-compress-install --with-native-compilation --with-mailutils --with-pgtk --with-tree-sitter
        make -j $(nproc)
        sudo make install
        emacs --eval "(progn (straight-pull-all) (straight-check-all) (kill-emacs))"
        fc-cache -fv
    fi
fi
