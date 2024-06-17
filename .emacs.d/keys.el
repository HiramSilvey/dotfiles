;;; keys.el -- Additional keybinding configuration.
;;; Commentary:
;;; Code:

;; Help navigate keybindings.
(use-package which-key
  :init (which-key-mode))

(use-package emacs
  :bind (("C-c q" . kill-buffer-and-window)
         ("C-c /" . comment-or-uncomment-region)))

;;; keys.el ends here
