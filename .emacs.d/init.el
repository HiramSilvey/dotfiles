;;; init.el -- Hiram's configuration.
;;; Commentary:
;;; Code:

;; Enable installing packages from MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'.
(require 'gnutls)
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

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
 '(package-selected-packages nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
