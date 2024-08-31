#!/bin/bash

# Exit the script if any errors are encountered.
set -e

# Print commands to stdout as they are run.
set -o xtrace

SCRIPT_DIRNAME=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
SCRIPT_BASENAME=$(basename $SCRIPT_DIRNAME)

# dnf
sudo dnf -y update

sudo dnf copr enable -y solopasha/hyprland
sudo dnf copr enable -y azandure/clipse

sudo dnf install -y hyprland hyprpaper hyprlock hypridle xdg-desktop-portal-hyprland qt5-qtwayland qt6-qtwayland pipewire wireplumber waybar dolphin firefox pavucontrol socat zsh stow curl git go cmake libtool libvterm grim slurp fuzzel qt6ct kvantum plasma-breeze-qt6 lz4-devel btop bluez hyprpicker NetworkManager wl-clipboard brightnessctl aylurs-gtk-shell clipse

# flatpak
! which flatpak || flatpak update

# rust
which rustup || curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup update

# zsh
[ -d $HOME/.oh-my-zsh ] || sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
[ -d $HOME/.oh-my-zsh/custom/themes/powerlevel10k ] || git clone --depth=1 https://github.com/romkatv/powerlevel10k.git $HOME/.oh-my-zsh/custom/themes/powerlevel10k
[ -d $HOME/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting ] || git clone https://github.com/zsh-users/zsh-syntax-highlighting.git $HOME/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
[ -d $HOME/.oh-my-zsh/custom/plugins/zsh-autosuggestions ] || git clone https://github.com/zsh-users/zsh-autosuggestions $HOME/.oh-my-zsh/custom/plugins/zsh-autosuggestions
[ -d $HOME/.oh-my-zsh/custom/plugins/autoupdate ] || git clone https://github.com/TamCore/autoupdate-oh-my-zsh-plugins $HOME/.oh-my-zsh/custom/plugins/autoupdate

# go
go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest

# hyprland contrib scratchpad
[ -d $HOME/src/public/contrib ] || git clone https://github.com/hyprwm/contrib.git $HOME/src/public/contrib
cd $HOME/src/public/contrib
git pull
cd scratchpad
sed -i s/"_menu_cmd=.*"/"_menu_cmd=\"fuzzel --dmenu -p scratchpad\""/g scratchpad
sudo make install

# swww
[ -d $HOME/src/public/swww ] || git clone https://github.com/LGFae/swww.git $HOME/src/public/swww
cd $HOME/src/public/swww
git pull
cargo build --release
sudo mv target/release/swww /usr/local/bin/swww
sudo mv target/release/swww-daemon /usr/local/bin/swww-daemon

# bun
curl -fsSL https://bun.sh/install | bash && \
  sudo ln -sf $HOME/.bun/bin/bun /usr/local/bin/bun

# HyprPanel
[ -d $HOME/src/public/HyprPanel ] || git clone https://github.com/Jas-SinghFSU/HyprPanel.git $HOME/src/public/HyprPanel
cd $HOME/src/public/HyprPanel
git pull
[ ! -d $HOME/.config/ags ] || mv -f $HOME/.config/ags $HOME/.config/ags.bkup
ln -sf $HOME/src/public/HyprPanel $HOME/.config/ags

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
emacs --eval "(progn (all-the-icons-install-fonts t) (nerd-icons-install-fonts t) (kill-emacs))"
fc-cache -fv

# Finish install.
[ ! -d $HOME/.emacs.d ] || mv -f $HOME/.emacs.d $HOME/.emacs.d.old
[ ! -d $HOME/.zshrc ] || mv -f $HOME/.zshrc $HOME/.zshrc.old

cd ${SCRIPT_DIRNAME}/..
stow $SCRIPT_BASENAME -t ${HOME}/

echo "To install dart-sass:
1. Download and extract the latest release from https://github.com/sass/dart-sass/releases/
2. sudo mv dart-sass /opt/
3. sudo ln -sf /opt/dart-sass/sass /usr/local/bin/sass"
