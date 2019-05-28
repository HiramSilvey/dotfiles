;;; macos.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; A macOS-specific config.

;;; Code:

;; macOS-specific: Fix $PATH for flycheck
(package-install 'exec-path-from-shell)
(exec-path-from-shell-initialize)


;;; macos.el ends here
