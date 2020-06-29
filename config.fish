# Support fish shell in emacs.
if test -n "$EMACS"
    set -x TERM eterm-color
end
function fish_title
    true
end

# Set aliases.
alias e="emacsclient -c -a=''"
alias te="emacsclient -t -a=''"
alias ke="emacsclient -e '(save-buffers-kill-emacs)'"

# Set env variables.
set -x EDITOR emacs
set -x ALTERNATE_EDITOR emacs

# Load additional local configurations.
if test -e $HOME/.config/fish/local.fish
    source $HOME/.config/fish/local.fish
end
