;;; language.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Loads the language-specific packages.

;;; Code:

;; Support C, C++, Java, etc.
(require 'cc-mode)

;; Support golang
(package-install 'go-mode)

;;; language.el ends here
