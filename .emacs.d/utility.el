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

;; GitHub Copilot.
(use-package copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-<tab>" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion)
              ("C-<S-tab>" . 'copilot-accept-completion-by-line)
              ("C-<iso-lefttab>" . 'copilot-accept-completion-by-line))
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))

  (defun hs/copilot-toggle-overlay ()
    "Toggle the copilot overlay."
    (interactive)
    (setq copilot-idle-delay (if (eq copilot-idle-delay 0) nil 0))))

;; Completion in region function.
(use-package corfu
  ; Free the RET key for less intrusive behavior.
  :bind (:map corfu-map
              ("RET" . nil)
              ("C-n" . nil)
              ("C-p" . nil))
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

;; Emacs Mini-Buffer Actions Rooted in Keymaps. I.e. a super-charged
;; right-click.
(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config

  ;; Hide the mode line of the Embark live/completions buffers.
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Integrate embark with consult.
(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Ensure exec-path matches the shell $PATH.
(use-package exec-path-from-shell
  :custom (exec-path-from-shell-arguments nil)
  :config
  (exec-path-from-shell-copy-envs '("SSH_AUTH_SOCK"
                                    "SSH_AGENT_PID"
                                    "GPG_AGENT_INFO"
                                    "GOPRIVATE"))
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

;; Writable grep buffers!
(use-package wgrep)

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
