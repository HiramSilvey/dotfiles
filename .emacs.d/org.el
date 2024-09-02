;;; org.el -- Org mode configuration.
;;; Commentary:
;;; Code:

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

;; Personal wiki!
(use-package org-roam)

;;; org.el ends here.
