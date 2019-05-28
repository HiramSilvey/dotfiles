;;; misc.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Loads the miscellaneous low-priority packages.

;;; Code:

;; Install yasnippet and enable globally
(package-install 'yasnippet)
(yas-global-mode 1)

;; Install undo tree and enable globally
(package-install 'undo-tree)
(global-undo-tree-mode 1)

;; Automatically refresh the file when it changes on disk
(global-auto-revert-mode t)

;; Cleanup bogus whitespace before saving
(add-hook 'before-save-hook 'whitespace-cleanup)

;;; misc.el ends here
