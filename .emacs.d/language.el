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
  :config
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(d)" "|" "DONE(D)" "CANCELED(x@)"))))

;; Support Rust.
(use-package rust-mode
  :ensure t
  :hook ((rust-mode rust-ts-mode) . rust-format-on-save))

(use-package find-file
  :config
  ;; Support swapping between C++ files in Unreal Engine's recommended directory
  ;; structure.
  (setq cc-search-directories (nconc cc-search-directories
                                     '("../Public" "../Private"))))

(use-package emacs
  :init
  ;; Prefer tree sitter major modes where applicable.
  ;; TODO: Convert this into a less hacky solution once TS modes are more well
  ;; supported.
  (setq major-mode-remap-alist
        '((bash-mode . bash-ts-mode)
          (c++-mode . c++-ts-mode)
          (c-or-c++-mode . c-or-c++-ts-mode)
          (c-mode . c-ts-mode)
          (cmake-mode . cmake-ts-mode)
          (csharp-mode . csharp-ts-mode)
          (css-mode . css-ts-mode)
          (dockerfile-mode . dockerfile-ts-mode)
          (elixir-mode . elixir-ts-mode)
          (go-mod-mode . go-mod-ts-mode)
          (go-mode . go-ts-mode)
          (heex-mode . heex-ts-mode)
          (html-mode . html-ts-mode)
          (java-mode . java-ts-mode)
          (js-mode . js-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (ruby-mode . ruby-ts-mode)
          (rust-mode . rust-ts-mode)
          (toml-mode . toml-ts-mode)
          (mode . tsx-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (yaml-mode . yaml-ts-mode)))
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
