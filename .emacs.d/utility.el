;;; utility.el -- Small miscellaneous utilities configuration.
;;; Commentary:
;;; Code:

;; Auto-format clang supported languages on save.
(use-package clang-format
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

;; Completion in region function.
(use-package corfu
  ; Free the RET key for less intrusive behavior.
  :bind (:map corfu-map ("RET" . nil))
  :init
  (global-corfu-mode)
  :custom
  ;; Enable auto completion and configure quitting.
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-quit-no-match 'separator)
  ;; Enable corfu to use orderless.
  (corfu-separator ?_)
  :hook
  (eshell-mode . (lambda ()
                   (setq-local corfu-auto nil)
                   (corfu-mode))))

;; Ensure exec-path matches the shell $PATH.
(use-package exec-path-from-shell
  :custom (exec-path-from-shell-arguments nil)
  :config
  (dolist (var '("SSH_AUTH_SOCK"
                 "SSH_AGENT_PID"
                 "GPG_AGENT_INFO"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize))

;; Prefer tree-sitter enabled modes when installed.
(use-package treesit-auto
  :functions (global-treesit-auto-mode)
  :custom (treesit-auto-install
           'prompt "Prompt to auto-install missing tree-sitter grammars.")
  :config (global-treesit-auto-mode))

;; Improved, yet simple, undo + redo functionality.
(use-package undo-fu
  :bind (("C-/" . undo-fu-only-undo)
         ("M-_" . unfo-fu-only-redo)))

;; Undo + redo across emacs sessions.
(use-package undo-fu-session
  :init (undo-fu-session-global-mode)
  :custom (undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'")))

(use-package yasnippet
  :init (yas-global-mode 1)
  :bind ("M-<ret>" . yas-expand)
  :config
  (which-key-add-key-based-replacements "C-c &" "yasnippet"))
(use-package yasnippet-snippets)

(use-package emacs
  :init
  ;; `TAB' indents correctly with spaces.
  (setq-default indent-tabs-mode nil)

  ;; `TAB' indents and completes.
  (setq tab-always-indent 'complete)

  ;; Remove extra whitespace on file save.
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;; Consolidate backup file location.
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

  ;; Always save an auth-source entry for further use by additional auth-source
  ;; backends.
  (setq auth-source-save-behavior nil)

  ;; Hide commands in M-x which do not apply to the current mode. Corfu commands
  ;; are hidden, since they are not used via M-x. This setting is useful beyond
  ;; Corfu.
  (setq read-extended-command-predicate #'command-completion-default-include-p))

;;; utility.el ends here
