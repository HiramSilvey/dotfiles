#!/bin/bash

# Exit the script if any errors are encountered.
set -e

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

sudo dnf install -y hyprland hyprpaper hyprlock hypridle xdg-desktop-portal-hyprland qt5-qtwayland qt6-qtwayland pipewire wireplumber waybar fuzzel dolphin firefox pavucontrol socat zsh stow curl git go cmake libtool libvterm

# zsh
[ -d $HOME/.oh-my-zsh ] || sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"
[ -d $HOME/.oh-my-zsh/custom/themes/powerlevel10k ] || git clone --depth=1 https://github.com/romkatv/powerlevel10k.git $HOME/.oh-my-zsh/custom/themes/powerlevel10k
[ -d $HOME/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting ] || git clone https://github.com/zsh-users/zsh-syntax-highlighting.git $HOME/.oh-my-zsh/custom/plugins/zsh-syntax-highlighting
[ -d $HOME/.oh-my-zsh/custom/plugins/zsh-autosuggestions ] || git clone https://github.com/zsh-users/zsh-autosuggestions $HOME/.oh-my-zsh/custom/plugins/zsh-autosuggestions
[ -d $HOME/.oh-my-zsh/custom/plugins/autoupdate ] || git clone https://github.com/TamCore/autoupdate-oh-my-zsh-plugins $HOME/.oh-my-zsh/custom/plugins/autoupdate

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
[ -d $HOME/src/public/apps/clipse ] || git clone https://github.com/savedra1/clipse $HOME/src/public/apps/clipse
cd $HOME/src/public/apps/clipse
git pull
go mod tidy
go build -o clipse
sudo mv clipse /usr/local/bin/clipse

# Finish install.
[ -d $HOME/.emacs.d ] && mv $HOME/.emacs.d $HOME/.emacs.d.old
[ -d $HOME/.zshrc ] && mv $HOME/.zshrc $HOME/.zshrc.old
stow $SCRIPT_DIR -t $HOME/
