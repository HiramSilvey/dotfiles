;;; init.el -- Hiram's configuration
;;; Commentary:
;;; Code:

;; Enable installing packages from MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'.
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

;; Enable VERTical Interactive COmpletion.
(use-package vertico
  :ensure t
  :config (vertico-mode))

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator 'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :config (savehist-mode))

;; Enable richer vertico annotations using the marginalia package.
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' only in the minibuffer
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :config (marginalia-mode))

;; Additional useful vertico & misc configurations.
(use-package emacs
  :init
  ;; Powerlevel10k-compatible font.
  (set-face-attribute 'default nil :font "MesloLGS NF" :height 143)
  ;; Default unicode fallback font.
  (set-fontset-font "fontset-default" 'unicode "Noto Sans Symbols 2")

  ;; `TAB' indents correctly with spaces.
  (setq-default indent-tabs-mode nil)

  ;; Toggle top bars off.
  (menu-bar-mode -1)
  (tool-bar-mode -1)

  ;; Bind "C-c o" to swap between C/C++ source and header files.
  ;; Note: Customize `ff-other-file-alist' to easily extend this to tests and/or
  ;; other languages.
  (dolist (hook '(c-ts-mode-hook
                  c++-ts-mode-hook
                  c-or-c++-ts-mode-hook))
    (add-hook hook (lambda()
                     (local-set-key (kbd "C-c o") 'ff-find-other-file))))

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add 'completing-read-multiple :filter-args 'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook 'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
        'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; Remove extra whitespace on file save.
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;; Consolidate backup file location.
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

  ;; Load local file configurations if present.
  (if (file-readable-p "~/.emacs.d/local.el")
      (load "~/.emacs.d/local.el"))
  )

;; Highlight TODO keywords.
(use-package hl-todo
  :ensure t
  :config (hl-todo-mode))

;; Enable a snazzy modeline.
(use-package doom-modeline
  :ensure t
  :config (doom-modeline-mode 1))

;; Temporarily highlight modified regions.
(use-package goggles
  :ensure t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config (setq-default goggles-pulse t))  ;; set to nil to disable pulsing

;; Extra dired colors.
(use-package diredfl
  :ensure t)

;; Dired using fd.
(use-package fd-dired
  :ensure t)

;; Highlight VC changes in the lefthand gutter.
(use-package diff-hl
  :ensure t
  :config (global-diff-hl-mode))

;; Improved, yet simple, undo + redo functionality.
(use-package undo-fu
  :ensure t
  :config
  (global-unset-key (kbd "C-/"))
  (global-unset-key (kbd "M-_"))
  (global-set-key (kbd "C-/")   'undo-fu-only-undo)
  (global-set-key (kbd "M-_")   'undo-fu-only-redo))

;; Undo + redo across emacs sessions.
(use-package undo-fu-session
  :ensure t
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (undo-fu-session-global-mode))

;; Blazingly fast terminal emulator.
(use-package vterm
  :ensure t)

;; Allow multiple vterm buffers.
(use-package multi-vterm
  :ensure t
  :config
  (global-set-key (kbd "C-c t") 'multi-vterm))

;; Syntax checker.
(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

(use-package flycheck-popup-tip
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-popup-tip-mode))

;; Dead-simple xref lookups.
(use-package dumb-jump
  :ensure t
  :config (add-hook 'xref-backend-functions 'dumb-jump-xref-activate))

;; Git support.
(use-package magit
  :ensure t)

;; Org mode!
(use-package org
  :ensure t
  :config
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(d)" "|" "DONE(D)" "CANCELED(x@)"))))

;; Highlight code parentheses.
(use-package highlight-parentheses
  :ensure t
  :hook (prog-mode . highlight-parentheses-mode))

;; COMPlete ANYthing.
(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

;; Ensure exec-path matches the shell $PATH
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;; Project-level interaction library.
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Help navigate keybindings.
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; Featureful LSP support.
(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  ((c++-mode c++-ts-mode rust-mode rust-ts-mode) . lsp)
  :config
  (setq lsp-keymap-prefix "C-c l")
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map))

;; `lsp-mode' supported debugger.
(use-package dap-mode
  :ensure t
  :after lsp-mode
  ;; Support debugging Rust.
  :config (dap-register-debug-template "Rust::GDB Run Configuration"
                                       (list :type "gdb"
                                             :request "launch"
                                             :name "GDB::Run"
                                             :gdbpath "rust-gdb"
                                             :target nil
                                             :cwd nil)))

;; Prefer tree-sitter enabled modes when installed.
(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; Auto-format clang supported languages on save.
(use-package clang-format
  :ensure t
  :init (defun clang-format-on-save ()
          (add-hook 'before-save-hook 'clang-format-buffer nil 'local))
  :hook ((c++-ts-mode
          c-or-c++-ts-mode
          c-ts-mode
          csharp-ts-mode
          java-ts-mode
          js-ts-mode
          json-ts-mode)
         . clang-format-on-save))

;; Add support for Rust code.
(use-package rust-mode
  :ensure t
  :hook ((rust-mode rust-ts-mode) . rust-format-on-save))

;; Add support for Cargo configuration files.
(use-package cargo-mode
  :ensure t
  :hook ((rust-mode rust-ts-mode) . cargo-minor-mode))

;; Add support for Markdown files.
(use-package markdown-mode
  :ensure t
  :config (setq markdown-command '("pandoc" "--from=markdown" "--to=html5")))

;; Pretty icons.
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; Icons needed for `doom-modeline'.
(use-package nerd-icons
  :ensure t)

;; Differentiate code buffers from everything else.
(use-package solaire-mode
  :ensure t
  :config (solaire-global-mode +1))

;; Pretty theme.
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors.
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Automatically update pending packages on startup. Checks for updates weekly.
(use-package auto-package-update
  :ensure t
  :config (auto-package-update-maybe))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
