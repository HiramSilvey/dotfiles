#!/bin/bash

# Exit the script if any errors are encountered.
set -e

# Print commands to stdout as they are run.
set -o xtrace

# rust
rustup update

# go
go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest

# emacs
mkdir -p $HOME/src/public/apps
[ -d $HOME/src/public/apps/emacs ] || git clone git://git.sv.gnu.org/emacs.git $HOME/src/public/apps/emacs
cd $HOME/src/public/apps/emacs
git clean -dfx
git pull
./autogen.sh
./configure --without-compress-install --with-native-compilation --with-mailutils --with-pgtk --with-tree-sitter
make -j $(nproc)
sudo make install

# fonts
emacs --eval "(progn (all-the-icons-install-fonts t) (nerd-icons-install-fonts t) (kill-emacs))"
fc-cache -fv

# clipse
[ -d $HOME/src/public/apps/clipse ] || git clone https://github.com/savedra1/clipse.git $HOME/src/public/apps/clipse
cd $HOME/src/public/apps/clipse
git pull
go mod tidy
go build -o clipse
sudo mv clipse /usr/local/bin/clipse

# swww
[ -d $HOME/src/public/swww ] || git clone https://github.com/LGFae/swww.git $HOME/src/public/swww
cd $HOME/src/public/swww
git pull
cargo build --release
sudo mv target/release/swww /usr/local/bin/swww
sudo mv target/release/swww-daemon /usr/local/bin/swww-daemon

# bun
curl -fsSL https://bun.sh/install | bash && \
  sudo ln -s $HOME/.bun/bin/bun /usr/local/bin/bun

# HyprPanel
[ -d $HOME/src/public/HyprPanel ] || git clone https://github.com/Jas-SinghFSU/HyprPanel.git $HOME/src/public/HyprPanel
cd $HOME/src/public/HyprPanel
git pull
[ -d $HOME/.config/ags ] || mv $HOME/.config/ags $HOME/.config/ags.bkup
ln -s $(pwd)/HyprPanel $HOME/.config/ags

# hyprland contrib scratchpad
[ -d $HOME/src/public/contrib ] || git clone https://github.com/hyprwm/contrib.git $HOME/src/public/contrib
cd $HOME/src/public/contrib
git pull
cd scratchpad
sed -i s/"_menu_cmd=.*"/"_menu_cmd=\"fuzzel --dmenu -p scratchpad\""/g scratchpad
sudo make install

echo "To update dart-sass:
1. Download and extract the latest release from https://github.com/sass/dart-sass/releases/
2. sudo mv dart-sass /opt/
3. sudo ln -s /opt/dart-sass/sass /usr/local/bin/sass"
