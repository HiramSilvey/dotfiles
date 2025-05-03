;;; keys.el -- Additional keybinding configuration.
;;; Commentary:
;;; Code:

;; Help navigate keybindings.
(use-package which-key
  :init (which-key-mode))

(use-package emacs
  :bind (("C-c /" . comment-or-uncomment-region)
         ("C-c q" . kill-buffer-and-window)
         ("C-c k c" . kill-current-buffer)
         ("C-c k o" . clean-buffer-list)
         ("C-c r" . eglot-rename)
         ("C-c c" . hs/copilot-toggle-overlay)
         ("C-c e p" . flymake-goto-prev-error)
         ("C-c e n" . flymake-goto-next-error)
         ("C-M-s-p" . windmove-up)
         ("C-M-s-n" . windmove-down)
         ("C-M-s-b" . windmove-left)
         ("C-M-s-f" . windmove-right)
         ("C-M-S-s-p" . windmove-swap-states-up)
         ("C-M-S-s-n" . windmove-swap-states-down)
         ("C-M-S-s-b" . windmove-swap-states-left)
         ("C-M-S-s-f" . windmove-swap-states-right))
  :init (which-key-add-key-based-replacements "C-c h" "help"))

;;; keys.el ends here
