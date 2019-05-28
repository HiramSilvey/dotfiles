;;; syntax.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Loads the syntax packages.

;;; Code:

;; Install flycheck and enable globally
(package-install 'flycheck)
(global-flycheck-mode)

;; Turn on golang linters in flycheck
(package-install 'flycheck-golangci-lint)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

;;; syntax.el ends here
