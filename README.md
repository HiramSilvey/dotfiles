# Installation (Linux)

## Prerequisites

* [Zsh](https://www.zsh.org/)
* [Oh My Zsh](https://ohmyz.sh/)
   * [Powerlevel10k](https://github.com/romkatv/powerlevel10k#oh-my-zsh)
	  * [MesloLGS NF](https://github.com/romkatv/powerlevel10k#fonts)
	  * [Noto Sans Symbols 2](https://fonts.google.com/noto/specimen/Noto+Sans+Symbols+2)
   * [zsh-syntax-highlighting](https://github.com/zsh-users/zsh-syntax-highlighting)
   * [zsh-autosuggestions](https://github.com/zsh-users/zsh-autosuggestions)
* [tmux](https://github.com/tmux/tmux)
* [GNU Stow](https://www.gnu.org/software/stow/)
* [GNU Emacs](https://www.gnu.org/software/emacs/) (>= 28)
* [emacs-libvterm dependencies](https://github.com/akermu/emacs-libvterm#requirements)

## Steps

1. Clone the repository to `/path/to/dotfiles`
1. Delete all existing `~/.emacs.d`, `~/.tmux.conf`, `~/.zshrc`, and `~/.p10k.zsh` files/directories
1. Open your shell and run `stow /path/to/dotfiles`
1. Open `emacs`
   1. Follow the prompts to compile `vterm`
   1. Run `M-x all-the-icons-install-fonts` to properly support `all-the-icons`
   1. Run `M-x nerd-icons-install-fonts` to properly support `doom-modeline`
