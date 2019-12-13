;;; keybindings.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Loads the general emacs-wide keybindings.

;;; Code:

;; Remap cut and copy commands to cut and copy whole lines respectively when no region is selected
(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2015-06-10"
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-region (line-beginning-position) (line-beginning-position 2))))))

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region').

URL `http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html'
Version 2018-09-10"
  (interactive)
  (if current-prefix-arg
      (progn
        (copy-region-as-kill (point-min) (point-max)))
    (if (use-region-p)
        (progn
          (copy-region-as-kill (region-beginning) (region-end)))
      (if (eq last-command this-command)
          (if (eobp)
              (progn )
            (progn
              (kill-append "\n" nil)
              (kill-append
               (buffer-substring-no-properties (line-beginning-position) (line-end-position))
               nil)
              (progn
                (end-of-line)
                (forward-char))))
        (if (eobp)
            (if (eq (char-before) 10 )
                (progn )
              (progn
                (copy-region-as-kill (line-beginning-position) (line-end-position))
                (end-of-line)))
          (progn
            (copy-region-as-kill (line-beginning-position) (line-end-position))
            (end-of-line)
            (forward-char)))))))

(global-set-key (kbd "C-w") 'xah-cut-line-or-region) ; cut
(global-set-key (kbd "M-w") 'xah-copy-line-or-region) ; copy

;; Swap the u and x keys for all emacs commands
(defun swap-ctl-u-ctl-x (frame)
    "Swap the ctl-u and ctl-x keys in the newly created FRAME."
    (with-selected-frame frame
      (define-key input-decode-map [?\C-x] [?\C-u])
      (define-key input-decode-map [?\C-u] [?\C-x])))
(add-hook 'after-make-frame-functions #'swap-ctl-u-ctl-x)
(define-key key-translation-map [?\M-u] [?\M-x])

;;; keybindings.el ends here
