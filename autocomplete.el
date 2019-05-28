;;; autocomplete.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Loads the autocomplete packages.

;;; Code:

;; Install company mode with specific language backends
(package-install 'company)
(package-install 'company-c-headers)
(package-install 'company-go)

;; Enable company mode globally
(add-hook 'after-init-hook 'global-company-mode)

;; Enable interactively do things
(require 'ido)
(ido-mode t)

;;; autocomplete.el ends here
