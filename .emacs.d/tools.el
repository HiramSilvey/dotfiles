;;; tools.el -- Larger tools configuration.
;;; Commentary:
;;; Code:

;; Git support.
(use-package magit
  :ensure t
  :config (setq magit-define-global-key-bindings 'recommended))

;; Emacs shell customization.
(use-package eshell
  :bind ("C-c s" . eshell-new)
  :hook (eshell-mode . (lambda ()
                         (local-set-key (kbd "C-d") 'eshell-life-is-too-much))))

;; Blazingly fast terminal emulator.
(use-package vterm
  :ensure t
  :config (setq vterm-buffer-name-string "<vterm>%s"))

;; Allow multiple vterm buffers.
(use-package multi-vterm
  :ensure t
  :bind ("C-c t" . multi-vterm))

(use-package emacs
  :init
  (defun eshell-new()
    "Open a new instance of eshell."
    (interactive)
    (eshell 'N)))

;;; tools.el ends here.
