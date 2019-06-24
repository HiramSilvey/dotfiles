;;; visual.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Visual preferences.

;;; Code:

;; Load the theme
(package-install 'moe-theme)
(require 'moe-theme)
(load-theme 'moe-dark t)

;; Disable GUI toolbar
(tool-bar-mode -1)

;; Highlight matching braces
(show-paren-mode 1)

;; Show column number cursor position
(column-number-mode 1)

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable smooth scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;;; visual.el ends here
