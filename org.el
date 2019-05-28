;;; org.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Org mode config.

;;; Code:

;; Install Org
(package-install 'org)

;; Resolve TAB key conflict with YASnippet
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))

;; Add Org-specific keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;;; org.el ends here
