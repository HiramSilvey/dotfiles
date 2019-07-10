;;; keybindings.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Loads the general emacs-wide keybindings.

;;; Code:

;; Swap the u and x keys for all emacs commands
(defun swap-ctl-u-ctl-x (frame)
    "Swap the ctl-u and ctl-x keys in the newly created FRAME."
    (with-selected-frame frame
      (define-key input-decode-map [?\C-x] [?\C-u])
      (define-key input-decode-map [?\C-u] [?\C-x])))
(add-hook 'after-make-frame-functions #'swap-ctl-u-ctl-x)
(define-key key-translation-map [?\M-u] [?\M-x])

;;; keybindings.el ends here
