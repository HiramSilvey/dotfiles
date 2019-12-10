;;; org.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Org mode config.

;;; Code:

;; Install Org
(package-install 'org)
(require 'org)

;; Resolve TAB key conflict with YASnippet
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local yas/trigger-key [tab])
            (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand)))

;; Add Org-specific keybindings
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

;; Enable a cleaner indentation view
(add-hook 'org-mode-hook 'org-indent-mode)

;; Add custom TODO states and prompt for a note when canceled
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)" "CANCELED(x@)")))

;; Change TODO entries to DONE when all children are done
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; Add custom face colors for custom TODO states
(setq org-todo-keyword-faces
      '(("CANCELED" . (:foreground "light grey" :inverse-video t :weight bold))))

;; Log the date when the TODO state of an item changes to a DONE state
(setq org-log-done-with-time nil) ;; Do not log the time with the date
(setq org-log-done 'time)

;;; org.el ends here
