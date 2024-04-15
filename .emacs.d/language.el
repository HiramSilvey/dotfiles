;;; language.el -- Supported language configuration.
;;; Commentary:
;;; Code:

;; Support Cargo configuration files.
(use-package cargo-mode
  :ensure t
  :hook ((rust-mode rust-ts-mode) . cargo-minor-mode))

;; Support Git configuration files.
(use-package git-modes
  :ensure t)

;; Support Markdown.
(use-package markdown-mode
  :ensure t
  :custom (markdown-command '("pandoc" "--from=markdown" "--to=html5")))

;; Org mode!
(use-package org
  :ensure t
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  :hook (org-mode . visual-line-mode)  ;; Wrap lines visually.
  :custom
  (org-todo-keywords '((sequence "TODO(t)" "|" "DONE(D)" "CANCELED(x@)")))
  (org-log-into-drawer t "Add state change log lines into hidden drawers by default.")
  (org-log-done 'time "Log the time tasks are completed.")
  (org-agenda-start-with-log-mode t "Full day log in agenda view.")
  (org-ellipsis " ▾" "Update end-of-line elipsis to a nicer-looking arrow.")
  (org-startup-indented t "Display lines as intented for a cleaner view.")
  (org-archive-location "~/Documents/Org/archive.org::datetree/")
  (org-priority-highest 65 "ASCII value of 'A'.")
  (org-priority-lowest 68 "ASCII value of 'D'.")
  (org-priority-default 67 "ASCII value of 'C'.")
  (org-priority-faces
      '((?A . '(org-priority))
        (?B . '(:foreground "dark orange"))
        (?C . '(shadow))
        (?D . '(shadow)))
      "Differentiate priorities visually.")
  :config

  ;; Load local agenda file list if present.
  (let ((hs/agenda-files "~/.emacs.d/agenda_files.txt"))
    (if (file-readable-p hs/agenda-files)
        (setq org-agenda-files
              (with-temp-buffer
                (insert-file-contents hs/agenda-files)
                (split-string (buffer-string) "\n" t)))))

  (setq org-capture-templates
        `(("b" "Bookmark" entry (file "~/Documents/Org/bookmarks.org")
           "* [[%^{Link}][%^{Description}]]" :immediate-finish t)

          ;; TODO: Sort the whole file automatically after inserting new term.
          ("g" "Glossary term" entry (file "~/Documents/Org/glossary.org")
           "* %^{Term}: %^{Definition}" :immediate-finish t)

          ("t" "Task" entry (file+olp "~/Documents/Org/tasks.org" "Inbox" "Tasks")
           "* TODO %?\n%U")

          ("n" "Note" item (file+olp "~/Documents/Org/tasks.org" "Inbox" "Notes")
           "- %?\n  %U\n")))

  ;; Replace list hyphen with bullet visually.
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

;; Prettify org bullets.
(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●" "○")))

;; Support Rust.
(use-package rust-mode
  :ensure t
  :hook ((rust-mode rust-ts-mode) . rust-format-on-save))

;; Support Go.
(use-package go-mode
  :ensure t
  :config
  ;; Global Go language server configuration.
  (setq-default eglot-workspace-configuration
    '((:gopls .
        ((staticcheck . t)
         (matcher . "CaseSensitive")))))
  :hook
  (before-save . (lambda ()
                   (call-interactively 'eglot-code-action-organize-imports)))
  (before-save . gofmt-before-save))

(use-package emacs
  :init
  ;; Bind "C-c o" to swap between C/C++ source and header files.
  ;; Note: Customize `ff-other-file-alist' to easily extend this to tests and/or
  ;; other languages.
  ;; Additionally ensure eglot is started automatically on clang-supported
  ;; languages.
  (dolist (hook '(c-common-mode-hook
                  c++-ts-mode-hook
                  c-or-c++-ts-mode-hook
                  c-ts-mode-hook))
    (add-hook hook (lambda()
                     (local-set-key (kbd "C-c o") 'ff-find-other-file)
                     (eglot-ensure))))
  (add-hook 'rust-ts-mode-hook 'eglot-ensure))

;;; language.el ends here.
