# Installation (Linux)

## Prerequisites

* [Sway](https://swaywm.org/)
* [Zsh](https://www.zsh.org/)
  * [Oh My Zsh](https://ohmyz.sh/)
    * [Powerlevel10k](https://github.com/romkatv/powerlevel10k#oh-my-zsh)
      * [MesloLGS NF](https://github.com/romkatv/powerlevel10k#fonts)
      * [Noto Sans Symbols 2](https://fonts.google.com/noto/specimen/Noto+Sans+Symbols+2)
    * [zsh-syntax-highlighting](https://github.com/zsh-users/zsh-syntax-highlighting)
    * [zsh-autosuggestions](https://github.com/zsh-users/zsh-autosuggestions)
    * [autoupdate-oh-my-zsh-plugins](https://github.com/tamcore/autoupdate-oh-my-zsh-plugins)
* [GNU Emacs](https://www.gnu.org/software/emacs/) (>= 30)<sup>[1](#footnote1)</sup>
  * [emacs-libvterm dependencies](https://github.com/akermu/emacs-libvterm#requirements)
  * [Go](https://go.dev/doc/install)
    * [gopls](https://pkg.go.dev/golang.org/x/tools/gopls)
* [GNU Stow](https://www.gnu.org/software/stow/)

<a name="footnote1">1</a> Compiling `emacs` from source:
```console
git clone https://git.savannah.gnu.org/cgit/emacs.git && cd emacs
```
```console
./autogen.sh
```
```console
./configure --without-compress-install --with-native-compilation --with-json --with-mailutils --with-pgtk --with-tree-sitter
```
```console
make -j $(nproc)
```
```console
sudo make install
```
Note: When changing versions, run `git clean -d -f -x` before re-running the build commands above to remove previously generated files.

## Steps

1. Clone the repository to `/path/to/dotfiles`.
1. Delete/move all existing `emacs` and `zsh` files/directories.
1. Open your shell and run `stow /path/to/dotfiles`.
1. Configure `emacs`:
   1. Open `emacs`.
   1. Follow the prompts to compile `vterm`.
   1. Run `M-x all-the-icons-install-fonts` to properly support `all-the-icons`.
   1. Run `M-x nerd-icons-install-fonts` to properly support `doom-modeline`.
