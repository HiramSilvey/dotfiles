;;; navigation.el -- In-buffer text navigation configuration.
;;; Commentary:
;;; Code:

;; Jump to visible text!
(use-package avy
  :bind (("C-c c" . avy-goto-char)
         ("C-c w" . avy-goto-word-1)
         ("C-c C-j" . avy-resume))
  :config (avy-setup-default))

;; Dead-simple xref lookups.
(use-package dumb-jump
  :config (add-hook 'xref-backend-functions 'dumb-jump-xref-activate))

;; Select region outwards.
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;; navigation.el ends here
