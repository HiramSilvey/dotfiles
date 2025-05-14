;;; org.el -- Org mode configuration.
;;; Commentary:
;;; Code:

;; Org mode!
(use-package org
  :init (which-key-add-key-based-replacements "C-c n" "org")
  :bind (("C-c n a" . org-agenda)
         ("C-c n t" . org-todo-list))
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
     (?C . '(:foreground "steel blue"))
     (?D . '(shadow)))
   "Differentiate priorities visually.")
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
(use-package org-roam
  :demand t
  :hook (org-mode . hs/update-tag-before-save)
  :custom
  (org-roam-directory (file-truename "~/Documents/Org/"))
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("m" "topic (miscellaneous)" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n")
      :unnarrowed t)
     ("c" "component" plain
      "%^{Summary}\n\nOwner: %^{Owner}\nDocs: [[%^{Wiki URL}][Wiki]]%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n")
      :unnarrowed t)
     ("t" "team" plain
      "[[%^{Wiki URL}][Wiki]]%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n I" . org-roam-node-insert-immediate)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         :map org-mode-map
         ("C-M-i"   . completion-at-point))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface.
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)

  (defun hs/update-tag-before-save ()
    "Adds a hook to update the project tag iff the current file is an org roam node."
    (when (vulpea-buffer-p)
      (add-hook 'before-save-hook #'vulpea-project-update-tag nil t)))

  ;; Source: https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
  (defun org-roam-node-insert-immediate (arg &rest args)
  (interactive "P")
  (let ((args (push arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

  ;; Source: https://gist.github.com/d12frosted/a60e8ccb9aceba031af243dff0d19b2e
  (defun vulpea-project-p ()
    "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks."
    (seq-find
     (lambda (type)
       (eq type 'todo))
     (org-element-map
      (org-element-parse-buffer 'headline)
      'headline
      (lambda (h)
        (org-element-property :todo-type h)))))

  (defun vulpea-project-update-tag ()
    "Update PROJECT tag in the current buffer."
    (when (and (not (active-minibuffer-window))
               (vulpea-buffer-p))
      (save-excursion
        (goto-char (point-min))
        (let* ((tags (vulpea-buffer-tags-get))
               (original-tags tags))
          (if (vulpea-project-p)
              (setq tags (cons "project" tags))
            (setq tags (remove "project" tags)))

          ;; cleanup duplicates
          (setq tags (seq-uniq tags))

          ;; update tags if changed
          (when (or (seq-difference tags original-tags)
                    (seq-difference original-tags tags))
            (apply #'vulpea-buffer-tags-set tags))))))

  (defun vulpea-buffer-p ()
    "Return non-nil if the currently visited buffer is a note."
    (and buffer-file-name
         (string-prefix-p
          (expand-file-name (file-name-as-directory org-roam-directory))
          (file-name-directory buffer-file-name))))

  (defun vulpea-project-files ()
    "Return a list of note files containing 'project' tag." ;
    (seq-uniq
     (seq-map
      #'car
      (org-roam-db-query
       [:select [nodes:file]
                :from tags
                :left-join nodes
                :on (= tags:node-id nodes:id)
                :where (like tag (quote "%\"project\"%"))]))))

  (defun vulpea-agenda-files-update (&rest _)
    "Update the value of `org-agenda-files'."
    (setq org-agenda-files (vulpea-project-files)))

  (advice-add 'org-agenda :before #'vulpea-agenda-files-update)
  (advice-add 'org-todo-list :before #'vulpea-agenda-files-update)

  ;; functions borrowed from `vulpea' library
  ;; https://github.com/d12frosted/vulpea/blob/6a735c34f1f64e1f70da77989e9ce8da7864e5ff/vulpea-buffer.el

  (defun vulpea-buffer-tags-get ()
    "Return filetags value in current buffer."
    (vulpea-buffer-prop-get-list "filetags" "[ :]"))

  (defun vulpea-buffer-tags-set (&rest tags)
    "Set TAGS in current buffer.

If filetags value is already set, replace it."
    (if tags
        (vulpea-buffer-prop-set
         "filetags" (concat ":" (string-join tags ":") ":"))
      (vulpea-buffer-prop-remove "filetags")))

  (defun vulpea-buffer-tags-add (tag)
    "Add a TAG to filetags in current buffer."
    (let* ((tags (vulpea-buffer-tags-get))
           (tags (append tags (list tag))))
      (apply #'vulpea-buffer-tags-set tags)))

  (defun vulpea-buffer-tags-remove (tag)
    "Remove a TAG from filetags in current buffer."
    (let* ((tags (vulpea-buffer-tags-get))
           (tags (delete tag tags)))
      (apply #'vulpea-buffer-tags-set tags)))

  (defun vulpea-buffer-prop-set (name value)
    "Set a file property called NAME to VALUE in buffer file.
If the property is already set, replace its value."
    (setq name (downcase name))
    (org-with-point-at 1
                       (let ((case-fold-search t))
                         (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                                                (point-max) t)
                             (replace-match (concat "#+" name ": " value) 'fixedcase)
                           (while (and (not (eobp))
                                       (looking-at "^[#:]"))
                             (if (save-excursion (end-of-line) (eobp))
                                 (progn
                                   (end-of-line)
                                   (insert "\n"))
                               (forward-line)
                               (beginning-of-line)))
                           (insert "#+" name ": " value "\n")))))

  (defun vulpea-buffer-prop-set-list (name values &optional separators)
    "Set a file property called NAME to VALUES in current buffer.
VALUES are quoted and combined into single string using
`combine-and-quote-strings'.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t.
If the property is already set, replace its value."
    (vulpea-buffer-prop-set
     name (combine-and-quote-strings values separators)))

  (defun vulpea-buffer-prop-get (name)
    "Get a buffer property called NAME as a string."
    (org-with-point-at 1
                       (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                                                (point-max) t)
                         (buffer-substring-no-properties
                          (match-beginning 1)
                          (match-end 1)))))

  (defun vulpea-buffer-prop-get-list (name &optional separators)
    "Get a buffer property NAME as a list using SEPARATORS.
If SEPARATORS is non-nil, it should be a regular expression
matching text that separates, but is not part of, the substrings.
If nil it defaults to `split-string-default-separators', normally
\"[ \f\t\n\r\v]+\", and OMIT-NULLS is forced to t."
    (let ((value (vulpea-buffer-prop-get name)))
      (when (and value (not (string-empty-p value)))
        (split-string-and-unquote value separators))))

  (defun vulpea-buffer-prop-remove (name)
    "Remove a buffer property called NAME."
    (org-with-point-at 1
                       (when (re-search-forward (concat "\\(^#\\+" name ":.*\n?\\)")
                                                (point-max) t)
                         (replace-match ""))))
  )

;;; org.el ends here.
