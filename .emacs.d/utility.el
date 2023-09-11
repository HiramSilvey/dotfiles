;;; utility.el -- Small miscellaneous utilities configuration.
;;; Commentary:
;;; Code:

;; Automatically update pending packages on startup. Checks for updates weekly.
(use-package auto-package-update
  :ensure t
  :config (auto-package-update-maybe))

;; Auto-format clang supported languages on save.
(use-package clang-format
  :ensure t
  :init (defun clang-format-on-save ()
          (add-hook 'before-save-hook 'clang-format-buffer nil 'local))
  :hook ((c++-ts-mode
          c-or-c++-ts-mode
          c-ts-mode
          csharp-ts-mode
          java-ts-mode
          js-ts-mode
          json-ts-mode)
         . clang-format-on-save))

;; COMPlete ANYthing.
(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

;; Dead-simple xref lookups.
(use-package dumb-jump
  :ensure t
  :config (add-hook 'xref-backend-functions 'dumb-jump-xref-activate))

;; Ensure exec-path matches the shell $PATH
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;; Select region outwards.
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Improved, yet simple, undo + redo functionality.
(use-package undo-fu
  :ensure t
  :bind (("C-/" . undo-fu-only-undo)
         ("M-_" . unfo-fu-only-redo)))

;; Undo + redo across emacs sessions.
(use-package undo-fu-session
  :ensure t
  :init (undo-fu-session-global-mode)
  :config (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

;; Help navigate keybindings.
(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package emacs
  :init
  ;; `TAB' indents correctly with spaces.
  (setq-default indent-tabs-mode nil)

  ;; Remove extra whitespace on file save.
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;; Consolidate backup file location.
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups"))))

;;; utility.el ends here
