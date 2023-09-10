;;; tools.el -- Larger tools configuration.
;;; Commentary:
;;; Code:

;; Git support.
(use-package magit
  :ensure t)

;; Blazingly fast terminal emulator.
(use-package vterm
  :ensure t
  :config (setq vterm-buffer-name-string "<vterm>%s"))

;; Allow multiple vterm buffers.
(use-package multi-vterm
  :ensure t
  :bind ("C-c t" . multi-vterm))

;;; tools.el ends here.
