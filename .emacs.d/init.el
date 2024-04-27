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

;; Bootstrap `use-package'.
(require 'gnutls)
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(require 'bind-key)

;; Tell `use-package' to use `straight.el' by default.
(setq straight-use-package-by-default t)

(use-package emacs
  :init
  (load "~/.emacs.d/language.el")
  (load "~/.emacs.d/search.el")
  (load "~/.emacs.d/tools.el")
  (load "~/.emacs.d/ui.el")
  (load "~/.emacs.d/utility.el")

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
