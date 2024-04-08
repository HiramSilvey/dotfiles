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
  :hook
  (eshell-mode . (lambda ()
                   ;; Disable auto company pop-up and instead bind it to TAB.
                   (setq-local company-idle-delay nil)
                   (local-set-key [tab] 'company-complete-common)
                   ;; Add binding to exit eshell quickly.
                   (local-set-key (kbd "C-d") 'eshell-life-is-too-much))))

;; Fish-like autosuggestions in eshell!
(use-package capf-autosuggest
  :ensure t
  :hook ((comint-mode eshell-mode) . capf-autosuggest-mode))

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
