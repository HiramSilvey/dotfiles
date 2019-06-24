;;; language.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Loads the language-specific packages.

;;; Code:

;; Support C, C++, Java, etc.
(require 'cc-mode)

;; Support Go
(package-install 'go-mode)
(require 'go-mode)

;; Support Rust & Cargo
(package-install 'rust-mode)
(require 'rust-mode)
(setq rust-format-on-save t)

(package-install 'cargo)
(require 'cargo)
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;;; language.el ends here
