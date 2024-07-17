;;; init.el -- Hiram's configuration.
;;; Commentary:
;;; Code:

;; Prevent `package.el' from loading packages prior to this init file loading.
(setq package-enable-at-startup nil)

;; Boostrap `straight.el'.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Tell `use-package' to use `straight.el' by default.
(setq straight-use-package-by-default t)

;; Quick bugfix for an undeclared package dependency.
(use-package project)

(use-package emacs
  :init
  (load "~/.emacs.d/keys.el")
  (load "~/.emacs.d/ui.el")
  (load "~/.emacs.d/navigation.el")
  (load "~/.emacs.d/language.el")
  (load "~/.emacs.d/utility.el")
  (load "~/.emacs.d/search.el")
  (load "~/.emacs.d/tools.el")

  ;; Load local file configurations if present.
  (if (file-readable-p "~/.emacs.d/local.el")
      (load "~/.emacs.d/local.el")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
