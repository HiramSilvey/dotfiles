;;; tools.el -- Larger tools configuration.
;;; Commentary:
;;; Code:

;; Git porcelain.
(use-package magit
  :custom (magit-define-global-key-bindings
           'recommended "Use magit recommended C-c keybindings."))

;; Docker support.
(use-package docker
  :bind ("C-c d" . docker))

;; Emacs shell customization.
(use-package eshell
  :bind ("C-c s" . eshell-new)
  :hook
  ((eshell-mode . (lambda ()
                   ;; Disable auto company pop-up and instead bind it to TAB.
                   (setq-local company-idle-delay nil)
                   (local-set-key [tab] 'company-complete-common)
                   ;; Add binding to exit eshell quickly.
                   (local-set-key (kbd "C-d") 'eshell-life-is-too-much)))
   (eshell-directory-change . rename-eshell-buffer))
  :custom
  ;; Prevent renaming eshell buffers from calling eshell-mode again.
  (rename-eshell-buffer-hook nil)
  (eshell-history-append t)
  :config
  (defun rename-eshell-buffer()
    "Rename the current eshell buffer based on the current directory."
    (rename-buffer
     (concat "<eshell>:" (abbreviate-file-name default-directory)) t))
  (defun eshell-new()
    "Open a new instance of eshell."
    (interactive)
    (eshell 'N)
    (rename-eshell-buffer)))

;; Make Emacs shell prompt pretty.
(use-package eshell-prompt-extras
  :custom (eshell-prompt-function 'epe-theme-lambda))

;; Fish-like autosuggestions in eshell!
(use-package capf-autosuggest
  :hook ((comint-mode eshell-mode) . capf-autosuggest-mode))

;; Blazingly fast terminal emulator.
(use-package vterm
  :custom (vterm-buffer-name-string
           "<vterm>%s" "Mark vterm buffer names with a prefix."))

;; Allow multiple vterm buffers.
(use-package multi-vterm
  :bind ("C-c t" . multi-vterm))

;;; tools.el ends here.
