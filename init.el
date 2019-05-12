;;; .init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:

;; -----------------------------------------------------------------------------
;; High-priority miscellaneous
;; -----------------------------------------------------------------------------

(require 'package)
(require 'cc-mode)

;; Start the server for emacsclient
(server-start)

;; Save auto-save backup files in a dedicated directory
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; -----------------------------------------------------------------------------
;; Keybindings
;; -----------------------------------------------------------------------------

;; Swap the u and x keys for all emacs commands
(define-key key-translation-map [?\C-x] [?\C-u])
(define-key key-translation-map [?\C-u] [?\C-x])
(define-key key-translation-map [?\M-x] [?\M-u])
(define-key key-translation-map [?\M-u] [?\M-x])

;; macOS-specific: Set command to meta
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; -----------------------------------------------------------------------------
;; MELPA
;; -----------------------------------------------------------------------------
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable
  ;; as desired
  (add-to-list 'package-archives (cons "melpa"
				       (concat proto "://melpa.org/packages/"))
	       t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives
		 (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; Initialize
(package-initialize)
(package-refresh-contents)

;; -----------------------------------------------------------------------------
;; Company mode
;; -----------------------------------------------------------------------------

(package-install 'company)

;; Install specific language backends
(package-install 'company-c-headers)
(package-install 'company-go)

;; Enable globally
(add-hook 'after-init-hook 'global-company-mode)

;; -----------------------------------------------------------------------------
;; Yasnippets
;; -----------------------------------------------------------------------------

(package-install 'yasnippet)

;; Enable globally
(yas-global-mode 1)

;; -----------------------------------------------------------------------------
;; Flycheck
;; -----------------------------------------------------------------------------

(package-install 'flycheck)

;; Enable globally
(global-flycheck-mode)

;; OSX-specific fix for flycheck
(package-install 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Turn on golang linters in flycheck
(package-install 'flycheck-golangci-lint)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

;; -----------------------------------------------------------------------------
;; Smart tabs
;; -----------------------------------------------------------------------------

(package-install 'smart-tabs-mode)

;; Set the default C indentation to 2 columns
(setq-default c-basic-offset 2)

;; Disable tabs globally (spaces only)
(setq-default indent-tabs-mode nil)

;; Use C offset values for indenting Go code
(smart-tabs-add-language-support go go-mode-hook
  ((c-indent-line . c-basic-offset)
   (c-indent-region . c-basic-offset)))

;; Enable smart tabs for specific languages
(smart-tabs-insinuate 'c 'c++ 'go 'python 'javascript)

;; Enable tabs only for modes with smart tabs handling
(add-hook 'c-mode-common-hook
	  (lambda () (setq indent-tabs-mode t)))

;; -----------------------------------------------------------------------------
;; Miscellaneous visual
;; -----------------------------------------------------------------------------

;; Load the theme
(package-install 'moe-theme)
(load-theme 'moe-dark t)

;; Disable GUI toolbar
(tool-bar-mode -1)

;; Highlight matching braces
(show-paren-mode 1)

;; Show column number cursor position
(column-number-mode 1)

;; Start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enable smooth scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; Highlight characters past the 80-column limit
(require 'whitespace)
(setq whitespace-style '(face lines-tail))
;; Aggressively cleanup bogus whitespace
(setq whitespace-action '(cleanup))
(global-whitespace-mode t)

;; -----------------------------------------------------------------------------
;; Low-priority miscellaneous
;; -----------------------------------------------------------------------------

;; Automatically refresh the file when it changes on disk
(global-auto-revert-mode t)

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flycheck yasnippet smart-tabs-mode moe-theme flycheck-golangci-lint exec-path-from-shell company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-line ((t (:background "dark red" :foreground "White")))))
