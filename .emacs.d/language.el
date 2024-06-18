;;; language.el -- Supported language configuration.
;;; Commentary:
;;; Code:

;; Support Cargo configuration files.
(use-package cargo-mode
  :hook ((rust-mode rust-ts-mode) . cargo-minor-mode))

;; Support Git configuration files.
(use-package git-modes)

;; Support Dockerfiles.
(use-package dockerfile-mode)

;; Support Terraform files.
(use-package terraform-mode
  :custom (terraform-format-on-save t))

;; Support Varnish Configuration Language files.
(use-package vcl-mode)

;; Support Bazel and Starlark files.
(use-package bazel
  :config (add-to-list 'auto-mode-alist '("\\.star\\'" . bazel-starlark-mode)))

;; Configure yaml-ts-mode support.
(use-package yaml-ts-mode
  :config (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode)))

;; Support Protocol Buffer files.
(use-package protobuf-mode)

;; Support Markdown.
(use-package markdown-mode
  :custom (markdown-command '("pandoc" "--from=markdown" "--to=html5")))

;; Org mode!
(use-package org
  :init (which-key-add-key-based-replacements "C-c o" "org")
  :bind (("C-c o c" . org-capture)
         ("C-c o a" . org-agenda))
  :hook (org-mode . visual-line-mode)  ;; Wrap lines visually.
  :custom
  (org-todo-keywords '((sequence "TODO(t)" "|" "DONE(D)" "CANCELED(x@)")))
  (org-log-into-drawer t "Add state change log lines into hidden drawers by default.")
  (org-log-done 'time "Log the time tasks are completed.")
  (org-agenda-start-with-log-mode t "Full day log in agenda view.")
  (org-ellipsis " ▾" "Update end-of-line elipsis to a nicer-looking arrow.")
  (org-startup-indented t "Display lines as intented for a cleaner view.")
  (org-agenda-files '("~/Documents/Org/tasks.org" "~/Documents/Org/notes.org"))
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
  (setq org-capture-templates
        `(("b" "Bookmark" entry (file+olp "~/Documents/Org/bookmarks.org" "Inbox")
           "* [[%^{Link}][%^{Title}]]" :immediate-finish t)

          ;; TODO: Sort the whole file automatically after inserting new term.
          ("g" "Glossary term" entry (file "~/Documents/Org/glossary.org")
           "* %^{Term}: %^{Definition}")

          ("t" "Task" entry (file+olp "~/Documents/Org/tasks.org" "Inbox")
           "* TODO %?\n%U")

          ("n" "Note" entry (file+olp+datetree "~/Documents/Org/notes.org")
           "* %<%F %a %H:%M> %^g\n%?")))

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
  :hook ((rust-mode rust-ts-mode) . rust-format-on-save))

;; Support Go.
(use-package go-mode)

;; Configure go-ts-mode support.
(use-package reformatter)
(use-package go-ts-mode
  :after reformatter
  :hook (go-ts-mode . go-format-on-save-mode)
  :config
  (reformatter-define go-format
                      :program "goimports"
                      :args '("/dev/stdin"))
  ;; Global Go language server configuration.
  (setq-default eglot-workspace-configuration
    '((:gopls .
        ((staticcheck . t)
         (matcher . "CaseSensitive"))))))

(use-package emacs
  :init
  ;; 1. Bind "C-c z" to swap between C/C++ source and header files.
  ;; Note: Customize `ff-other-file-alist' to easily extend this to tests and/or
  ;; other languages.
  ;; 2. Ensure eglot is started automatically on LSP-supported languages.
  (let* ((ff-hooks '(c-common-mode-hook
                    c++-ts-mode-hook
                    c-or-c++-ts-mode-hook
                    c-ts-mode-hook))
         (eglot-hooks (append ff-hooks '(go-mode-hook
                                         go-ts-mode-hook
                                         rust-mode-hook
                                         rust-ts-mode-hook))))
    (dolist (hook ff-hooks)
      (add-hook hook '(local-set-key (kbd "C-c z") 'ff-find-other-file)))
    (dolist (hook eglot-hooks)
      (add-hook hook 'eglot-ensure))))

;;; language.el ends here.
