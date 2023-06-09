;;; ui.el -- User interface configuration.
;;; Commentary:
;;; Code:

;; Pretty icons.
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

;; Highlight VC changes in the lefthand gutter.
(use-package diff-hl
  :init (global-diff-hl-mode)
  :ensure t)

;; Extra dired colors.
(use-package diredfl
  :ensure t)

;; Enable a snazzy modeline.
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (setq column-number-mode t))

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

;; Temporarily highlight modified regions.
(use-package goggles
  :ensure t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config (setq-default goggles-pulse t))  ;; set to nil to disable pulsing

;; Highlight TODO keywords.
(use-package hl-todo
  :init (hl-todo-mode)
  :ensure t)

;; Draw vertical lines representing indent levels.
(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode . highlight-indent-guides-mode))

;; Highlight code parentheses.
(use-package highlight-parentheses
  :ensure t
  :hook (prog-mode . highlight-parentheses-mode))

;; Icons needed for `doom-modeline'.
(use-package nerd-icons
  :ensure t)

;; Differentiate code buffers from everything else.
(use-package solaire-mode
  :ensure t
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
