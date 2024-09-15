#!/bin/bash

# Exit the script if any errors are encountered.
set -e

# Print commands to stdout as they are run.
set -o xtrace

# dnf
sudo dnf -y update

# flatpak
! which flatpak || flatpak -y update

# rust
rustup update

# go
go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest

# swww
[ -d $HOME/src/public/swww ] || git clone https://github.com/LGFae/swww.git $HOME/src/public/swww
cd $HOME/src/public/swww
git fetch
if [ $(git rev-parse @) != $(git rev-parse @{u}) ]; then
    git pull
    cargo build --release
    sudo mv target/release/swww /usr/local/bin/swww
    sudo mv target/release/swww-daemon /usr/local/bin/swww-daemon
fi

# bun
curl -fsSL https://bun.sh/install | bash && \
  sudo ln -sf $HOME/.bun/bin/bun /usr/local/bin/bun

# HyprPanel
[ -d $HOME/src/public/HyprPanel ] || git clone https://github.com/Jas-SinghFSU/HyprPanel.git $HOME/src/public/HyprPanel
cd $HOME/src/public/HyprPanel
git fetch
if [ $(git rev-parse @) != $(git rev-parse @{u}) ]; then
    git pull
    [ ! -d $HOME/.config/ags ] || mv -f $HOME/.config/ags $HOME/.config/ags.bkup
    ln -sf $HOME/src/public/HyprPanel $HOME/.config/ags
fi

# hyprland contrib scratchpad
[ -d $HOME/src/public/contrib ] || git clone https://github.com/hyprwm/contrib.git $HOME/src/public/contrib
cd $HOME/src/public/contrib
git fetch
if [ $(git rev-parse @) != $(git rev-parse @{u}) ]; then
    git pull
    cd scratchpad
    sed -i s/"_menu_cmd=.*"/"_menu_cmd=\"fuzzel --dmenu -p scratchpad\""/g scratchpad
    sudo make install
fi

# nwg-look
[ -d $HOME/src/public/nwg-look ] || git clone https://github.com/nwg-piotr/nwg-look.git $HOME/src/public/nwg-look
cd $HOME/src/public/nwg-look
git fetch
if [ $(git rev-parse @) != $(git rev-parse @{u}) ]; then
    git pull
    make build
    sudo make install
fi

# emacs
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

echo "To update dart-sass:
1. Download and extract the latest release from https://github.com/sass/dart-sass/releases/
2. sudo mv dart-sass /opt/
3. sudo ln -sf /opt/dart-sass/sass /usr/local/bin/sass"
