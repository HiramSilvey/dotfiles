;;; language.el -- Supported language configuration.
;;; Commentary:
;;; Code:

;; Support Cargo configuration files.
(use-package cargo-mode
  :hook ((rust-mode rust-ts-mode) . cargo-minor-mode))

;; Support Git configuration files.
(use-package git-modes)

;; Support Dockerfiles.
(use-package dockerfile-mode)

;; Support Terraform files.
(use-package terraform-mode
  :custom (terraform-format-on-save t))

;; Support Varnish Configuration Language files.
(use-package vcl-mode)

;; Support Bazel and Starlark files.
(use-package bazel
  :config (add-to-list 'auto-mode-alist '("\\.star\\'" . bazel-starlark-mode)))

;; Configure yaml-ts-mode support.
(use-package yaml-ts-mode
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode)))

;; Support Protocol Buffer files.
(use-package protobuf-mode)

;; Support Markdown.
(use-package markdown-mode
  :custom (markdown-command '("pandoc" "--from=markdown" "--to=html5")))

;; Support Rust.
(use-package rust-mode
  :hook ((rust-mode rust-ts-mode) . rust-format-on-save))

;; Support Go.
(use-package go-mode)

;; Configure go-ts-mode support.
(use-package reformatter)
(use-package go-ts-mode
  :after reformatter
  :hook (go-ts-mode . go-format-on-save-mode)
  :config
  (reformatter-define go-format
                      :program "goimports"
                      :args '("/dev/stdin"))
  ;; Global Go language server configuration.
  (setq-default eglot-workspace-configuration
    '((:gopls .
        ((staticcheck . t)
         (matcher . "CaseSensitive"))))))

(use-package emacs
  :init
  ;; 1. Bind "C-c z" to swap between C/C++ source and header files.
  ;; Note: Customize `ff-other-file-alist' to easily extend this to tests and/or
  ;; other languages.
  ;; 2. Ensure eglot is started automatically on LSP-supported languages.
  (let* ((ff-hooks '(c-common-mode-hook
                    c++-ts-mode-hook
                    c-or-c++-ts-mode-hook
                    c-ts-mode-hook))
         (eglot-hooks (append ff-hooks '(go-mode-hook
                                         go-ts-mode-hook
                                         rust-mode-hook
                                         rust-ts-mode-hook))))
    (dolist (hook ff-hooks)
      (add-hook hook '(local-set-key (kbd "C-c z") 'ff-find-other-file)))
    (dolist (hook eglot-hooks)
      (add-hook hook 'eglot-ensure))))

;;; language.el ends here.
