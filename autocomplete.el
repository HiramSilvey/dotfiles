;;; autocomplete.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Loads the autocomplete packages.

;;; Code:

;; Install company mode with specific language backends
(package-install 'company)
(require 'company)
(package-install 'company-c-headers)
(require 'company-c-headers)
(package-install 'company-go)
(require 'company-go)

;; Enable company mode globally
(add-hook 'after-init-hook 'global-company-mode)

;; Install RACER: Rust Auto-Complete-ER
(package-install 'racer)
(require 'racer)

;; Enable RACER in rust-mode
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)

;; Enable interactively do things
(require 'ido)
(ido-mode t)

;;; autocomplete.el ends here
