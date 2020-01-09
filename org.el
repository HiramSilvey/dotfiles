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
      '((sequence "TODO(t)" "DOING(d)" "|" "DONE(D)" "CANCELED(x@)")))

(defun hjs/update-parent-state ()
  "When all subentries or checkboxes are complete, mark the entry as DONE.
When some subentries or checkboxes are complete, mark the entry as DOING.
When no subentries or checkboxes are complete, mark the entry as TODO.
This function requires the entry to have a [/] or [%] cookie."
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)) end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
          (cond ((match-end 1)
                 (cond ((equal (match-string 1) "0%") (org-todo "TODO"))
                       ((equal (match-string 1) "100%") (org-todo "DONE"))
                       (t (org-todo "DOING"))))
                ((and (match-end 2) (> (match-end 2) (match-beginning 2)))
                 (cond ((equal (match-string 2) "0") (org-todo "TODO"))
                       ((equal (match-string 2) (match-string 3)) (org-todo "DONE"))
                       (t (org-todo "DOING")))))))))

;; see http://thread.gmane.org/gmane.emacs.orgmode/42715
(eval-after-load 'org-list
  '(add-hook 'org-checkbox-statistics-hook (function hjs/update-parent-state)))

(defun hjs/update-todo-parent-state (n-done n-not-done)
  (hjs/update-parent-state)
  )
(add-hook 'org-after-todo-statistics-hook (function hjs/update-todo-parent-state))

;; Add custom face colors for custom TODO states
(setq org-todo-keyword-faces
      '(("DOING" . (:foreground "DarkGoldenRod" :background "LightGoldenRod" :weight bold))
        ("CANCELED" . (:foreground "light grey" :inverse-video t :weight bold))))

;; Log the date when the TODO state of an item changes to a DONE state
(setq org-log-done-with-time nil) ;; Do not log the time with the date
(setq org-log-done 'time)

;;; org.el ends here
