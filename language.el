;;; language.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Loads the language-specific packages.

;;; Code:

;; Support C, C++, Java, etc.
(require 'cc-mode)

;; Support Go
(package-install 'go-mode)

;; Support Rust
(package-install 'rust-mode)
(package-install 'cargo)
(setq rust-format-on-save t)

;;; language.el ends here
