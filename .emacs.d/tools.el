;;; tools.el -- Larger tools configuration.
;;; Commentary:
;;; Code:

;; Git porcelain.
(use-package magit
  :custom (magit-define-global-key-bindings
           'recommended "Use magit recommended C-c keybindings."))

;; Docker support.
(use-package docker
  :bind ("C-c x d" . docker))

;; Emacs shell customization.
(use-package eshell
  :bind (("C-c s" . hs/eshell-new)
         ("C-c k s" . hs/killall-eshell))
  :hook
  ((eshell-mode . (lambda ()
                   ;; Add binding to exit eshell quickly.
                   (local-set-key (kbd "C-d") 'eshell-life-is-too-much)))
   (eshell-directory-change . hs/rename-eshell-buffer))
  :custom
  ;; Prevent renaming eshell buffers from calling eshell-mode again.
  (rename-eshell-buffer-hook nil)
  (eshell-history-append t)
  :config
  (setq hs/eshell-buffer-prefix "<eshell>:")

  (defun hs/rename-eshell-buffer ()
    "Rename the current eshell buffer based on the current directory."
    (rename-buffer
     (concat hs/eshell-buffer-prefix (abbreviate-file-name default-directory)) t))

  (defun hs/eshell-new ()
    "Open a new instance of eshell."
    (interactive)
    (eshell 'N)
    (hs/rename-eshell-buffer))

  (defun hs/killall-eshell ()
    "Kill all eshell buffers, prompting for confirmation if necessary."
    (interactive)
    (hs/killall-buffers-prefix hs/eshell-buffer-prefix)))

;; Make Emacs shell prompt pretty.
(use-package eshell-prompt-extras
  :custom (eshell-prompt-function 'epe-theme-lambda))

;; Fish-like autosuggestions in eshell!
(use-package capf-autosuggest
  :hook ((comint-mode eshell-mode) . capf-autosuggest-mode))

;; Blazingly fast terminal emulator.
(use-package vterm
  :bind ("C-c k t" . hs/killall-vterm)
  :config
  ;; Mark vterm buffer names with a prefix.
  (setq hs/vterm-buffer-prefix "<vterm>:")
  (setq vterm-buffer-name-string (concat hs/vterm-buffer-prefix "%s"))

  (defun hs/killall-vterm ()
    "Kill all vterm buffers, prompting for confirmation if necessary."
    (interactive)
    (hs/killall-buffers-prefix hs/vterm-buffer-prefix)))

;; Allow multiple vterm buffers.
(use-package multi-vterm
  :bind ("C-c t" . multi-vterm))

(use-package emacs
  :init
  (defun hs/killall-buffers-prefix (prefix)
    "Kill all buffers with names matching the prefix string, prompting the user
    to confirm if needed."
    (interactive)
    (let ((buffers (mapcar #'buffer-name (buffer-list))))
      (while buffers
        (let ((buffer (pop buffers)))
          (if (string-prefix-p prefix buffer)
              (kill-buffer buffer))))))

  (which-key-add-key-based-replacements "C-c k" "kill")
  (which-key-add-key-based-replacements "C-c x" "tools"))

;;; tools.el ends here.
