;;; init.el -- Hiram's configuration
;;; Commentary:
;;; Code:

;; Enable installing packages from MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; Enable use-package.
(eval-when-compile (require 'use-package))

;; Enable VERTical Interactive COmpletion.
(use-package vertico
  :ensure t
  :init (vertico-mode))

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
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init (savehist-mode))

;; Enable richer vertico annotations using the marginalia package.
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle` only in the minibuffer
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

;; Additional useful vertico & misc configurations.
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Toggle menu-bar off.
  (menu-bar-mode -1)

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
  :init (hl-todo-mode))

;; Enable a snazzy modeline.
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Temporarily highlight modified regions.
(use-package goggles
  :ensure t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing

;; Extra dired colors.
(use-package diredfl
  :ensure t)

;; Dired using fd.
(use-package fd-dired
  :ensure t)

;; Highlight VC changes in the lefthand gutter.
(use-package diff-hl
  :ensure t
  :init (global-diff-hl-mode))

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
  :init (global-undo-fu-session-mode))

;; Blazingly fast terminal emulator.
(use-package vterm
  :ensure t
  :config
  (global-unset-key (kbd "C-c t"))
  (global-set-key (kbd "C-c t") 'vterm))

;; Syntax checker.
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-popup-tip
  :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))

;; Dead-simple xref lookups.
(use-package dumb-jump
  :ensure t
  :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Git support.
(use-package magit
  :ensure t)

;; Org mode!
(use-package org
  :ensure t
  :config (setq org-todo-keywords
	  '((sequence "TODO(t)" "DOING(d)" "|" "DONE(D)" "CANCELED(x@)"))))

;; Highlight code parentheses.
(use-package highlight-parentheses
  :ensure t
  :config (add-hook 'prog-mode-hook #'highlight-parentheses-mode))

;; COMPlete ANYthing.
(use-package company
  :ensure t
  :config (add-hook 'after-init-hook 'global-company-mode))

;; Auto-format C/C++ code on save.
(use-package clang-format
  :ensure t)
(use-package cc-mode
  :after clang-format
  :config (add-hook 'c-mode-common-hook
		    (lambda ()
		      (add-hook 'before-save-hook 'clang-format-buffer nil 'local))))

;; Add support for Rust code.
(use-package rust-mode
  :ensure t
  ;; `TAB` indents correctly with spaces.
  :config (add-hook 'rust-mode-hook
		    (lambda () (setq indent-tabs-mode nil)))
  ;; Format rust files on save using rustfmt.
  (setq rust-format-on-save t))

;; Add support for Cargo configuration files.
(use-package cargo-mode
  :ensure t
  :config (add-hook 'rust-mode-hook 'cargo-minor-mode)
  :after rust-mode)

;; Add support for Markdown files.
(use-package markdown-mode
  :ensure t
  :init (setq markdown-command '("pandoc" "--from=markdown" "--to=html5")))

;; Pretty icons.
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Pretty theme.
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)

  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(beacon-color "#cc6666")
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#7ec98f")
 '(cua-overwrite-cursor-color "#e5c06d")
 '(cua-read-only-cursor-color "#8ac6f2")
 '(custom-safe-themes
   '("1bddd01e6851f5c4336f7d16c56934513d41cc3d0233863760d1798e74809b4b" default))
 '(exwm-floating-border-color "#191b20")
 '(fci-rule-color "#2f2f2e")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(highlight-changes-colors '("#e5786d" "#834c98"))
 '(highlight-parentheses-colors '("#7ec98f" "#e5c06d" "#a4b5e6" "#834c98" "#8ac6f2"))
 '(highlight-symbol-colors
   '("#55204c0039fc" "#3f0a4e4240dc" "#5a2849c746fd" "#3fd2334a42f4" "#426a4d5455d9" "#537247613a13" "#46c549b0535c"))
 '(highlight-symbol-foreground-color "#999891")
 '(highlight-tail-colors
   '(("#2f2f2e" . 0)
     ("#3d464c" . 20)
     ("#3b473c" . 30)
     ("#41434a" . 50)
     ("#4c4536" . 60)
     ("#4b4136" . 70)
     ("#4d3936" . 85)
     ("#2f2f2e" . 100)))
 '(hl-bg-colors
   '("#4c4536" "#4b4136" "#504341" "#4d3936" "#3b313d" "#41434a" "#3b473c" "#3d464c"))
 '(hl-fg-colors
   '("#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29"))
 '(inhibit-startup-screen t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(lsp-ui-doc-border "#999891")
 '(nrepl-message-colors
   '("#ffb4ac" "#ddaa6f" "#e5c06d" "#3d464c" "#e3eaea" "#41434a" "#7ec98f" "#e5786d" "#834c98"))
 '(objed-cursor-color "#ff6c6b")
 '(package-selected-packages
   '(company highlight-parentheses doom-themes magit dumb-jump flycheck-popup-tip-mode flycheck-popup-tip flycheck vterm undo-fu-session undo-fu diff-hl fd-dired diredfl all-the-icons-dired goggles doom-modeline hl-todo orderless vertico use-package))
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#282c34"))
 '(pos-tip-background-color "#2f2f2e")
 '(pos-tip-foreground-color "#999891")
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(size-indication-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#8ac6f2" "#2f2f2e" 0.2))
 '(term-default-bg-color "#2a2a29")
 '(term-default-fg-color "#8d8b86")
 '(tool-bar-mode nil)
 '(tool-bar-position 'bottom)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#ffb4ac")
     (40 . "#f3a0bb068dbb")
     (60 . "#ecf0bdf57dd8")
     (80 . "#e5c06d")
     (100 . "#d1fcc2679b35")
     (120 . "#c551c35ab143")
     (140 . "#b610c464c727")
     (160 . "#a327c588dd05")
     (180 . "#8ac6f2")
     (200 . "#89fec7dad1d0")
     (220 . "#8863c85ec150")
     (240 . "#85eec8dcb0cf")
     (260 . "#82a3c956a041")
     (280 . "#7ec98f")
     (300 . "#9131c244b2d6")
     (320 . "#98acbe43c439")
     (340 . "#9f20ba15d58f")
     (360 . "#a4b5e6")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#2a2a29" "#2f2f2e" "#504341" "#ffb4ac" "#3d464c" "#8ac6f2" "#4c4536" "#e5c06d" "#41434a" "#a4b5e6" "#4d3936" "#e5786d" "#3b473c" "#7ec98f" "#8d8b86" "#74736f"))
 '(window-divider-mode nil)
 '(xterm-color-names
   ["#2f2f2e" "#ffb4ac" "#8ac6f2" "#e5c06d" "#a4b5e6" "#e5786d" "#7ec98f" "#e8e5db"])
 '(xterm-color-names-bright
   ["#2a2a29" "#ddaa6f" "#6a6a65" "#74736f" "#8d8b86" "#834c98" "#999891" "#f6f3e8"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "MesloLGS NF" :foundry "PfEd" :slant normal :weight normal :height 143 :width normal)))))
