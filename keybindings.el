;;; keybindings.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Loads the general emacs-wide keybindings.

;;; Code:

;; Swap the u and x keys for all emacs commands
(define-key key-translation-map [?\C-x] [?\C-u])
(define-key key-translation-map [?\C-u] [?\C-x])
(define-key key-translation-map [?\M-x] [?\M-u])
(define-key key-translation-map [?\M-u] [?\M-x])

;;; keybindings.el ends here
