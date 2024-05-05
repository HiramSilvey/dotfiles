;;; search.el -- Minibuffer search configuration.
;;; Commentary:
;;; Code:

;; Dired using fd.
(use-package fd-dired)

;; Vertico dependency -- see use below.
(use-package crm)

;; VERTical Interactive COmpletion.
(use-package vertico
  :init (vertico-mode)
  :hook
  (minibuffer-setup . cursor-intangible-mode)
  :custom
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)
   "Do not allow the cursor in the minibuffer prompt.")
  (read-extended-command-predicate
   'command-completion-default-include-p
   (concat "Emacs 28: Hide commands in M-x which do not work in the current mode. "
           "Vertico commands are hidden in normal buffers."))
  (enable-recursive-minibuffers t)
  :config
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add 'completing-read-multiple :filter-args 'crm-indicator))

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator 'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init (savehist-mode))

;; Enable richer vertico annotations using the marginalia package.
(use-package marginalia
  :init (marginalia-mode)
  ;; Bind `marginalia-cycle' only in the minibuffer
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle)))

;;; search.el ends here
