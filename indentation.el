;;; indentation.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Loads the indentation packages.

;;; Code:

;; Install smart tabs
(package-install 'smart-tabs-mode)
(require 'smart-tabs-mode)

;; Set the default C indentation to 2 columns
(setq-default tab-width 2)

;; Disable tabs globally (spaces only)
(setq-default indent-tabs-mode nil)

;; Enable smart tabs for specific languages
(smart-tabs-insinuate 'c 'c++ 'python 'javascript)

;; Enable tabs only for modes with smart tabs handling
(add-hook 'c-mode-common-hook
          (lambda () (setq indent-tabs-mode t)))

;;; indentation.el ends here
