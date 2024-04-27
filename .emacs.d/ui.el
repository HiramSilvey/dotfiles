;;; ui.el -- User interface configuration.
;;; Commentary:
;;; Code:

;; Pretty icons.
(use-package all-the-icons)
(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; Breadcrumbs within the modeline providing more information on the current
;; cursor location.
(use-package breadcrumb
  :custom (global-mode-string '(:eval (breadcrumb-imenu-crumbs))))

;; Highlight VC changes in the lefthand gutter.
(use-package diff-hl
  :init (global-diff-hl-mode))

;; Extra dired colors.
(use-package diredfl)

;; Enable a snazzy modeline.
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (column-number-mode t "Display in which column the cursor is."))

;; Pretty theme.
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t "Universally enable bold.")
  (doom-themes-enable-italic t "Universally enable italics.")
  :config
  (load-theme 'doom-gruvbox t)

  ;; Enable flashing mode-line on errors.
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Temporarily highlight modified regions.
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config (setq-default goggles-pulse t))  ;; set to nil to disable pulsing

;; Highlight TODO keywords.
(use-package hl-todo
  :init (hl-todo-mode))

;; Highlight code parentheses.
(use-package highlight-parentheses
  :hook (prog-mode . highlight-parentheses-mode))

;; Icons needed for `doom-modeline'.
(use-package nerd-icons)

;; Differentiate code buffers from everything else.
(use-package solaire-mode
  :init (solaire-global-mode +1))

(use-package emacs
  :init
  ;; Toggle unnecessary UI bars off.
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  ;; Powerlevel10k-compatible font.
  (set-face-attribute 'default nil :font "MesloLGS NF" :height 143)
  ;; Default unicode fallback font.
  (set-fontset-font "fontset-default" 'unicode "Noto Sans Symbols 2"))

;;; ui.el ends here.
