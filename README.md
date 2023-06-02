# Installation (Linux)

## Prerequisites

1. [Zsh](https://www.zsh.org/)
1. [Oh My Zsh](https://ohmyz.sh/)
   1. [Powerlevel10k](https://github.com/romkatv/powerlevel10k#oh-my-zsh)
	  1. [MesloLGS NF](https://github.com/romkatv/powerlevel10k#fonts)
	  1. [Noto Sans Symbols 2](https://fonts.google.com/noto/specimen/Noto+Sans+Symbols+2)
   1. [zsh-syntax-highlighting](https://github.com/zsh-users/zsh-syntax-highlighting)
   1. [zsh-autosuggestions](https://github.com/zsh-users/zsh-autosuggestions)
1. [tmux](https://github.com/tmux/tmux)
1. [GNU Stow](https://www.gnu.org/software/stow/)
1. [GNU Emacs](https://www.gnu.org/software/emacs/) (>= 28)
1. [emacs-libvterm dependencies](https://github.com/akermu/emacs-libvterm#requirements)

## Steps

1. Clone the repository
1. Delete any existing `~/.zshrc`, `~/.tmux.conf`, `~/.emacs.d` files/directories
1. In shell, run `stow /path/to/dotfiles`
1. Open Emacs
   1. Follow the prompts to compile `vterm`
   1. Run `M-x all-the-icons-install-fonts` to properly support `all-the-icons`
   1. Run `M-x nerd-icons-install-fonts` to properly support `doom-modeline`
