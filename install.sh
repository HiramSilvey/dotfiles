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
sudo dnf copr enable erikreider/SwayNotificationCenter

sudo dnf install -y hyprland hyprpaper hyprlock hypridle xdg-desktop-portal-hyprland qt5-qtwayland qt6-qtwayland pipewire wireplumber waybar firefox pavucontrol socat zsh stow curl git go cmake libtool libvterm grim slurp fuzzel qt6ct kvantum plasma-breeze-qt6 lz4-devel btop bluez hyprpicker NetworkManager wl-clipboard brightnessctl aylurs-gtk-shell clipse xcur2png the_silver_searcher ripgrep hyprpolkitagent xwaylandvideobridge SwayNotificationCenter meson pulseaudio-libs-devel

# flatpak
! which flatpak || flatpak -y update

# rust
which rustup || curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
rustup update

# go
go install golang.org/x/tools/gopls@latest
go install golang.org/x/tools/cmd/goimports@latest

# zsh
[ -d $HOME/.oh-my-zsh ] || sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
[ -d $HOME/.oh-my-zsh/custom/themes/powerlevel10k ] || git clone --depth=1 https://github.com/romkatv/powerlevel10k.git $HOME/.oh-my-zsh/custom/themes/powerlevel10k
[ -d $HOME/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting ] || git clone https://github.com/zsh-users/zsh-syntax-highlighting.git $HOME/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
[ -d $HOME/.oh-my-zsh/custom/plugins/zsh-autosuggestions ] || git clone https://github.com/zsh-users/zsh-autosuggestions $HOME/.oh-my-zsh/custom/plugins/zsh-autosuggestions
[ -d $HOME/.oh-my-zsh/custom/plugins/autoupdate ] || git clone https://github.com/TamCore/autoupdate-oh-my-zsh-plugins $HOME/.oh-my-zsh/custom/plugins/autoupdate

# SwayAudioIdleInhibit
[ -d $HOME/src/public/SwayAudioIdleInhibit ] || git clone https://github.com/ErikReider/SwayAudioIdleInhibit.git $HOME/src/public/SwayAudioIdleInhibit
cd $HOME/src/public/SwayAudioIdleInhibit
git pull
meson setup build -Dlogind-provider=systemd
meson compile -C build
sudo meson install -C build

# nwg-look
[ -d $HOME/src/public/nwg-look ] || git clone https://github.com/nwg-piotr/nwg-look.git $HOME/src/public/nwg-look
cd $HOME/src/public/nwg-look
git pull
make build
sudo make install

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
