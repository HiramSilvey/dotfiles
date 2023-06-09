;;; tools.el -- Larger tools configuration.
;;; Commentary:
;;; Code:

;; Syntax checker.
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(use-package flycheck-popup-tip
  :ensure t
  :after flycheck
  :hook (flycheck-mode . flycheck-popup-tip-mode))

;; Featureful LSP support.
(use-package lsp-mode
  :ensure t
  :commands lsp
  :init (setq lsp-keymap-prefix "C-c l")
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  ((c++-mode c++-ts-mode rust-mode rust-ts-mode) . lsp))

;; `lsp-mode' supported debugger.
(use-package dap-mode
  :ensure t
  :after lsp-mode
  :functions (dap-register-debug-template)
  ;; Support debugging Rust.
  :config (dap-register-debug-template "Rust::GDB Run Configuration"
                                       (list :type "gdb"
                                             :request "launch"
                                             :name "GDB::Run"
                                             :gdbpath "rust-gdb"
                                             :target nil
                                             :cwd nil)))

;; Git support.
(use-package magit
  :ensure t)

;; Blazingly fast terminal emulator.
(use-package vterm
  :ensure t
  :config (setq vterm-buffer-name-string "<vterm>%s"))

;; Allow multiple vterm buffers.
(use-package multi-vterm
  :ensure t
  :bind ("C-c t" . multi-vterm))

;;; tools.el ends here.
