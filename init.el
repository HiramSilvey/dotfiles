;;; .init.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; -----------------------------------------------------------------------------
;; High-priority
;; -----------------------------------------------------------------------------

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

;; -----------------------------------------------------------------------------
;; Package management
;; -----------------------------------------------------------------------------

;; Add ELPA and MELPA repositories
(require 'package)
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
  (add-to-list 'package-archives (cons "melpa"
               (concat proto "://melpa.org/packages/"))
         t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives
     (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; Initialize and refresh packages
(package-initialize)
(package-refresh-contents)

;; -----------------------------------------------------------------------------
;; Language support
;; -----------------------------------------------------------------------------

;; Support C, C++, Java, etc.
(require 'cc-mode)

;; Support golang
(package-install 'go-mode)

;; -----------------------------------------------------------------------------
;; Autocomplete
;; -----------------------------------------------------------------------------

;; Install company mode with specific language backends
(package-install 'company)
(package-install 'company-c-headers)
(package-install 'company-go)

;; Enable company mode globally
(add-hook 'after-init-hook 'global-company-mode)

;; Enable interactively do things
(require 'ido)
(ido-mode t)

;; -----------------------------------------------------------------------------
;; Org mode
;; -----------------------------------------------------------------------------

;; Install org mode
(package-install 'org)

;; Fix TAB conflict with YASnippet
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))

;; Set org-specific keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; -----------------------------------------------------------------------------
;; Syntax checking
;; -----------------------------------------------------------------------------

;; Install flycheck and enable globally
(package-install 'flycheck)
(global-flycheck-mode)

;; macOS-specific: Fix $PATH for flycheck
(package-install 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Turn on golang linters in flycheck
(package-install 'flycheck-golangci-lint)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-golangci-lint-setup))

;; -----------------------------------------------------------------------------
;; Indentation
;; -----------------------------------------------------------------------------

;; Install smart tabs
(package-install 'smart-tabs-mode)

;; Set the default C indentation to 2 columns
(setq-default tab-width 2)

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
;; Visual
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

;; Enable smooth scrolling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; Enable whitespace cleanup
(require 'whitespace)

;; -----------------------------------------------------------------------------
;; Other
;; -----------------------------------------------------------------------------

;; Install yasnippet and enable globally
(package-install 'yasnippet)
(yas-global-mode 1)

;; Install undo tree and enable globally
(package-install 'undo-tree)
(global-undo-tree-mode 1)

;; Automatically refresh the file when it changes on disk
(global-auto-revert-mode t)

;; Cleanup bogus whitespace before saving
(add-hook 'before-save-hook 'whitespace-cleanup)

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
 )
