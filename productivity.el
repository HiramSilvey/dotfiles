;;; productivity.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Loads the productivity packages.

;;; Code:

;; Install yasnippet and enable globally
(package-install 'yasnippet)
(require 'yasnippet)
(yas-global-mode 1)

;; Install undo tree and enable globally
(package-install 'undo-tree)
(require 'undo-tree)
(global-undo-tree-mode 1)

;; Automatically refresh the file when it changes on disk
(global-auto-revert-mode t)

;; Cleanup bogus whitespace before saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;;; productivity.el ends here
