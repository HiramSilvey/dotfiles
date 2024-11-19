;;; keys.el -- Additional keybinding configuration.
;;; Commentary:
;;; Code:

;; Help navigate keybindings.
(use-package which-key
  :init (which-key-mode))

(use-package keycast)

(use-package emacs
  :bind (("C-c /" . comment-or-uncomment-region)
         ("C-c q" . kill-buffer-and-window)
         ("C-c k c" . kill-current-buffer)
         ("C-c k o" . clean-buffer-list))
  :init (which-key-add-key-based-replacements "C-c h" "help"))

;;; keys.el ends here
