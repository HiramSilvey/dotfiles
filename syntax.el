;;; syntax.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Loads the syntax packages.

;;; Code:

;; Install flycheck and enable globally
(package-install 'flycheck)
(require 'flycheck)
(global-flycheck-mode)

;; Turn on golang linters in flycheck
(package-install 'flycheck-golangci-lint)
(require 'flycheck-golangci-lint)
(with-eval-after-load 'go-mode
  '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

;; Turn on rustlang linters in flycheck
(package-install 'flycheck-rust)
(require 'flycheck-rust)
(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;;; syntax.el ends here
