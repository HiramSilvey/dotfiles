;;; tools.el -- Larger tools configuration.
;;; Commentary:
;;; Code:

;; Git support.
(use-package magit
  :ensure t
  :custom (magit-define-global-key-bindings
           'recommended "Use magit recommended C-c keybindings."))

;; Emacs shell customization.
(use-package eshell
  :bind ("C-c s" . eshell-new)
  :hook
  (eshell-mode . (lambda ()
                   ;; Disable auto company pop-up and instead bind it to TAB.
                   (setq-local company-idle-delay nil)
                   (local-set-key [tab] 'company-complete-common)
                   ;; Add binding to exit eshell quickly.
                   (local-set-key (kbd "C-d") 'eshell-life-is-too-much)))
  ;; Append each command to the history file immediately.
  (eshell-pre-command . eshell-append-history)
  :custom (eshell-save-history-on-exit nil))

;; Make Emacs shell prompt pretty.
(use-package eshell-prompt-extras
  :ensure t
  :custom (eshell-prompt-function 'epe-theme-lambda))

;; Fish-like autosuggestions in eshell!
(use-package capf-autosuggest
  :ensure t
  :hook ((comint-mode eshell-mode) . capf-autosuggest-mode))

;; Blazingly fast terminal emulator.
(use-package vterm
  :ensure t
  :custom (vterm-buffer-name-string
           "<vterm>%s" "Mark vterm buffer names with a prefix."))

;; Allow multiple vterm buffers.
(use-package multi-vterm
  :ensure t
  :bind ("C-c t" . multi-vterm))

(use-package emacs
  :init
  (defun eshell-new()
    "Open a new instance of eshell."
    (interactive)
    (eshell 'N))

  ;; Source: https://emacs.stackexchange.com/a/18569
  (defun eshell-append-history ()
    "Call `eshell-write-history' with the `append' parameter set to `t'."
    (when eshell-history-ring
      (let ((newest-cmd-ring (make-ring 1)))
        (ring-insert newest-cmd-ring (car (ring-elements eshell-history-ring)))
        (let ((eshell-history-ring newest-cmd-ring))
          (eshell-write-history eshell-history-file-name t))))))

;;; tools.el ends here.
