# Installation (Linux)

## Prerequisites

* [tmux](https://github.com/tmux/tmux)
* [Zsh](https://www.zsh.org/)
  * [Oh My Zsh](https://ohmyz.sh/)
    * [Powerlevel10k](https://github.com/romkatv/powerlevel10k#oh-my-zsh)
      * [MesloLGS NF](https://github.com/romkatv/powerlevel10k#fonts)
      * [Noto Sans Symbols 2](https://fonts.google.com/noto/specimen/Noto+Sans+Symbols+2)
    * [zsh-syntax-highlighting](https://github.com/zsh-users/zsh-syntax-highlighting)
    * [zsh-autosuggestions](https://github.com/zsh-users/zsh-autosuggestions)
    * [autoupdate-oh-my-zsh-plugins](https://github.com/tamcore/autoupdate-oh-my-zsh-plugins)
* [GNU Emacs](https://www.gnu.org/software/emacs/) (>= 29)
  * [emacs-libvterm dependencies](https://github.com/akermu/emacs-libvterm#requirements)
  * (Optional) [LLDB dependencies](https://lldb.llvm.org/resources/build.html#preliminaries)
* [GNU Stow](https://www.gnu.org/software/stow/)

## Steps

1. Clone the repository to `/path/to/dotfiles`.
1. Delete all existing `~/.emacs.d`, `~/.tmux.conf`, `~/.zshrc`, and `~/.p10k.zsh` files/directories.
1. Open your shell and run `stow /path/to/dotfiles`.
1. Configure `emacs`.
   1. Open `emacs`.
   1. Follow the prompts to compile `vterm`.
   1. Run `M-x all-the-icons-install-fonts` to properly support `all-the-icons`.
   1. Run `M-x nerd-icons-install-fonts` to properly support `doom-modeline`.
   1. (Optional) Install `lldb-vscode` to support debugging via `LLDB` within `dap-mode`.
      1. Run the following commands:
         ```
         $ git clone https://github.com/llvm/llvm-project.git
         $ cd llvm-project
         $ cmake -G Ninja -DLLVM_ENABLE_PROJECTS="clang;lldb" -DCMAKE_BUILD_TYPE=Release ./llvm
         $ ninja lldb-vscode
         ```
      2. Follow these [installation steps](https://github.com/llvm/llvm-project/tree/main/lldb/tools/lldb-vscode#installation-for-visual-studio-code) to setup the proper directories.
