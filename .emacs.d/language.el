;;; language.el -- Supported language configuration.
;;; Commentary:
;;; Code:

;; Support Cargo configuration files.
(use-package cargo-mode
  :ensure t
  :hook ((rust-mode rust-ts-mode) . cargo-minor-mode))

;; Support Git configuration files.
(use-package git-modes
  :ensure t)

;; Support Markdown.
(use-package markdown-mode
  :ensure t
  :config (setq markdown-command '("pandoc" "--from=markdown" "--to=html5")))

;; Org mode!
(use-package org
  :ensure t
  :hook (org-mode . visual-line-mode)  ;; Wrap lines visually.
  :bind ("C-c a" . org-agenda)
  :config
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(D)" "CANCELED(x@)")))

  ;; Add state change log lines into hidden drawers by default.
  (setq org-log-into-drawer t)

  ;; Minor visual tweaks.
  (setq org-ellipsis " ▾")
  (setq org-startup-indented t)

  ;; Replace list hyphen with bullet visually.
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

;; Prettify org bullets.
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●" "○")))

;; Support Rust.
(use-package rust-mode
  :ensure t
  :hook ((rust-mode rust-ts-mode) . rust-format-on-save))

;; Support Go.
(use-package go-mode
  :ensure t
  :config
  ;; Global Go language server configuration.
  (setq-default eglot-workspace-configuration
    '((:gopls .
        ((staticcheck . t)
         (matcher . "CaseSensitive")))))
  :hook
  (before-save . (lambda ()
                   (call-interactively 'eglot-code-action-organize-imports)))
  (before-save . gofmt-before-save))

(use-package emacs
  :init
  ;; Bind "C-c o" to swap between C/C++ source and header files.
  ;; Note: Customize `ff-other-file-alist' to easily extend this to tests and/or
  ;; other languages.
  ;; Additionally ensure eglot is started automatically on clang-supported
  ;; languages.
  (dolist (hook '(c-common-mode-hook
                  c++-ts-mode-hook
                  c-or-c++-ts-mode-hook
                  c-ts-mode-hook))
    (add-hook hook (lambda()
                     (local-set-key (kbd "C-c o") 'ff-find-other-file)
                     (eglot-ensure))))
  (add-hook 'rust-ts-mode-hook 'eglot-ensure))

;;; language.el ends here.
