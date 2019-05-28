;;; .init.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Load all Emacs configs.

;;; Code:

(load "~/.emacs.d/configs/high-priority.el")
(load "~/.emacs.d/configs/keybindings.el")
(load "~/.emacs.d/configs/pacman.el")
(load "~/.emacs.d/configs/language.el")
(load "~/.emacs.d/configs/autocomplete.el")
(load "~/.emacs.d/configs/syntax.el")
(load "~/.emacs.d/configs/indentation.el")
(load "~/.emacs.d/configs/org.el")
(load "~/.emacs.d/configs/visual.el")
(load "~/.emacs.d/configs/misc.el")

(if (eq system-type 'darwin)
    (load "~/.emacs.d/configs/macos.el")
  )

(if (file-readable-p "~/.emacs.d/configs/local.el")
    (load "~/.emacs.d/configs/local.el")
  )

;;; init.el ends here
