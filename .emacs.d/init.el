;;; init.el -- Hiram's configuration
;;; Commentary:
;;; Code:

;; Enable installing packages from MELPA.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'.
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(require 'bind-key)
(setq use-package-always-ensure t)

;; Enable VERTical Interactive COmpletion.
(use-package vertico
  :ensure t
  :init (vertico-mode))

;; Optionally use the `orderless' completion style. See
;; `+orderless-dispatch' in the Consult wiki for an advanced Orderless style
;; dispatcher. Additionally enable `partial-completion' for file path
;; expansion. `partial-completion' is important for wildcard support.
;; Multiple files can be opened at once with `find-file' if you enter a
;; wildcard. You may also give the `initials' completion style a try.
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init (savehist-mode))

;; Enable richer vertico annotations using the marginalia package.
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' only in the minibuffer
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init (marginalia-mode))

;; Additional useful vertico & misc configurations.
(use-package emacs
  :init
  ;; Powerlevel10k-compatible font.
  (set-face-attribute 'default nil :font "MesloLGS NF" :height 143)
  ;; Default unicode fallback font.
  (set-fontset-font "fontset-default" 'unicode "Noto Sans Symbols 2")

  ;; `TAB' indents correctly with spaces.
  (setq-default indent-tabs-mode nil)

  ;; Toggle menu-bar off.
  (menu-bar-mode -1)

  ;; Bind "C-c o" to swap between C/C++ source and header files.
  ;; Note: Customize `ff-other-file-alist' to easily extend this to tests and/or
  ;; other languages.
  (dolist (hook '(c-ts-mode-hook
		  c++-ts-mode-hook
		  c-or-c++-ts-mode-hook))
    (add-hook hook (lambda()
		     (local-set-key (kbd "C-c o") 'ff-find-other-file))))

  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	'(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  (setq read-extended-command-predicate
	#'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; Remove extra whitespace on file save.
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;; Consolidate backup file location.
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

  ;; Load local file configurations if present.
  (if (file-readable-p "~/.emacs.d/local.el")
      (load "~/.emacs.d/local.el"))
  )

;; Highlight TODO keywords.
(use-package hl-todo
  :ensure t
  :init (hl-todo-mode))

;; Enable a snazzy modeline.
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Temporarily highlight modified regions.
(use-package goggles
  :ensure t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing

;; Extra dired colors.
(use-package diredfl
  :ensure t)

;; Dired using fd.
(use-package fd-dired
  :ensure t)

;; Highlight VC changes in the lefthand gutter.
(use-package diff-hl
  :ensure t
  :init (global-diff-hl-mode))

;; Improved, yet simple, undo + redo functionality.
(use-package undo-fu
  :ensure t
  :config
  (global-unset-key (kbd "C-/"))
  (global-unset-key (kbd "M-_"))
  (global-set-key (kbd "C-/")   'undo-fu-only-undo)
  (global-set-key (kbd "M-_")   'undo-fu-only-redo))

;; Undo + redo across emacs sessions.
(use-package undo-fu-session
  :ensure t
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  :init (undo-fu-session-global-mode))

;; Blazingly fast terminal emulator.
(use-package vterm
  :ensure t)

;; Allow multiple vterm buffers.
(use-package multi-vterm
  :ensure t
  :config
  (global-set-key (kbd "C-c t") 'multi-vterm))

;; Syntax checker.
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-popup-tip
  :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))

;; Dead-simple xref lookups.
(use-package dumb-jump
  :ensure t
  :config (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Git support.
(use-package magit
  :ensure t)

;; Org mode!
(use-package org
  :ensure t
  :config (setq org-todo-keywords
	  '((sequence "TODO(t)" "DOING(d)" "|" "DONE(D)" "CANCELED(x@)"))))

;; Highlight code parentheses.
(use-package highlight-parentheses
  :ensure t
  :config (add-hook 'prog-mode-hook 'highlight-parentheses-mode))

;; COMPlete ANYthing.
(use-package company
  :ensure t
  :config (add-hook 'after-init-hook 'global-company-mode))

;; Ensure exec-path matches the shell $PATH
(use-package exec-path-from-shell
  :ensure t
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;; Project-level interaction library.
(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :config (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Help navigate keybindings.
(use-package which-key
  :ensure t
  :init (which-key-mode))

;; Featureful LSP support.
(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :config (add-hook 'prog-mode-hook 'lsp)
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; `lsp-mode' supported debugger.
(use-package dap-mode
  :ensure t
  :after lsp-mode
  ;; Support debugging Rust.
  :config (dap-register-debug-template "Rust::GDB Run Configuration"
			     (list :type "gdb"
				   :request "launch"
				   :name "GDB::Run"
			   :gdbpath "rust-gdb"
				   :target nil
				   :cwd nil)))

;; Prefer tree-sitter enabled modes when installed.
(use-package treesit-auto
  :ensure t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;; Auto-format clang supported languages on save.
(use-package clang-format
  :ensure t
  :config (dolist (hook '(c-ts-mode-hook
			  c++-ts-mode-hook
			  c-or-c++-ts-mode-hook
			  csharp-ts-mode-hook
			  java-ts-mode-hook
			  js-ts-mode-hook
			  json-ts-mode-hook))
	    (add-hook hook (lambda ()
		      (add-hook 'before-save-hook 'clang-format-buffer nil 'local)))))

;; Add support for Rust code.
(use-package rust-mode
  :ensure t)
(use-package rust-ts-mode
  :after rust-mode
  :config (add-hook 'rust-ts-mode-hook
		    (lambda ()
		      (add-hook 'before-save-hook 'rust-format-buffer nil 'local))))

;; Add support for Cargo configuration files.
(use-package cargo-mode
  :ensure t
  :config (add-hook 'rust-ts-mode-hook 'cargo-minor-mode))

;; Add support for Markdown files.
(use-package markdown-mode
  :ensure t
  :init (setq markdown-command '("pandoc" "--from=markdown" "--to=html5")))

;; Pretty icons.
(use-package all-the-icons
  :ensure t)
(use-package all-the-icons-dired
  :ensure t
  :after all-the-icons
  :config (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Icons needed for `doom-modeline'.
(use-package nerd-icons
  :ensure t)

;; Pretty theme.
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)

  ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Automatically update pending packages on startup. Checks for updates weekly.
(use-package auto-package-update
  :ensure t
  :config (auto-package-update-maybe))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(beacon-color "#cc6666")
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(connection-local-criteria-alist
   '(((:application tramp :protocol "flatpak")
      tramp-container-connection-local-default-flatpak-profile)
     ((:application tramp)
      tramp-connection-local-default-system-profile tramp-connection-local-default-shell-profile)))
 '(connection-local-profile-alist
   '((tramp-container-connection-local-default-flatpak-profile
      (tramp-remote-path "/app/bin" tramp-default-remote-path "/bin" "/usr/bin" "/sbin" "/usr/sbin" "/usr/local/bin" "/usr/local/sbin" "/local/bin" "/local/freeware/bin" "/local/gnu/bin" "/usr/freeware/bin" "/usr/pkg/bin" "/usr/contrib/bin" "/opt/bin" "/opt/sbin" "/opt/local/bin"))
     (tramp-connection-local-darwin-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,uid,user,gid,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state=abcde" "-o" "ppid,pgid,sess,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etime,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . tramp-ps-time)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-busybox-ps-profile
      (tramp-process-attributes-ps-args "-o" "pid,user,group,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "stat=abcde" "-o" "ppid,pgid,tty,time,nice,etime,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (user . string)
       (group . string)
       (comm . 52)
       (state . 5)
       (ppid . number)
       (pgrp . number)
       (ttname . string)
       (time . tramp-ps-time)
       (nice . number)
       (etime . tramp-ps-time)
       (args)))
     (tramp-connection-local-bsd-ps-profile
      (tramp-process-attributes-ps-args "-acxww" "-o" "pid,euid,user,egid,egroup,comm=abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" "-o" "state,ppid,pgid,sid,tty,tpgid,minflt,majflt,time,pri,nice,vsz,rss,etimes,pcpu,pmem,args")
      (tramp-process-attributes-ps-format
       (pid . number)
       (euid . number)
       (user . string)
       (egid . number)
       (group . string)
       (comm . 52)
       (state . string)
       (ppid . number)
       (pgrp . number)
       (sess . number)
       (ttname . string)
       (tpgid . number)
       (minflt . number)
       (majflt . number)
       (time . tramp-ps-time)
       (pri . number)
       (nice . number)
       (vsize . number)
       (rss . number)
       (etime . number)
       (pcpu . number)
       (pmem . number)
       (args)))
     (tramp-connection-local-default-shell-profile
      (shell-file-name . "/bin/sh")
      (shell-command-switch . "-c"))
     (tramp-connection-local-default-system-profile
      (path-separator . ":")
      (null-device . "/dev/null"))))
 '(cua-global-mark-cursor-color "#7ec98f")
 '(cua-overwrite-cursor-color "#e5c06d")
 '(cua-read-only-cursor-color "#8ac6f2")
 '(custom-safe-themes
   '("1bddd01e6851f5c4336f7d16c56934513d41cc3d0233863760d1798e74809b4b" default))
 '(exwm-floating-border-color "#191b20")
 '(fci-rule-color "#2f2f2e")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(highlight-changes-colors '("#e5786d" "#834c98"))
 '(highlight-parentheses-colors '("#7ec98f" "#e5c06d" "#a4b5e6" "#834c98" "#8ac6f2"))
 '(highlight-symbol-colors
   '("#55204c0039fc" "#3f0a4e4240dc" "#5a2849c746fd" "#3fd2334a42f4" "#426a4d5455d9" "#537247613a13" "#46c549b0535c"))
 '(highlight-symbol-foreground-color "#999891")
 '(highlight-tail-colors
   '(("#2f2f2e" . 0)
     ("#3d464c" . 20)
     ("#3b473c" . 30)
     ("#41434a" . 50)
     ("#4c4536" . 60)
     ("#4b4136" . 70)
     ("#4d3936" . 85)
     ("#2f2f2e" . 100)))
 '(hl-bg-colors
   '("#4c4536" "#4b4136" "#504341" "#4d3936" "#3b313d" "#41434a" "#3b473c" "#3d464c"))
 '(hl-fg-colors
   '("#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29" "#2a2a29"))
 '(inhibit-startup-screen t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(lsp-ui-doc-border "#999891")
 '(nrepl-message-colors
   '("#ffb4ac" "#ddaa6f" "#e5c06d" "#3d464c" "#e3eaea" "#41434a" "#7ec98f" "#e5786d" "#834c98"))
 '(objed-cursor-color "#ff6c6b")
 '(package-selected-packages
   '(dap-gdb-lldb dap-lldb dap-mode lsp-mode treesit-auto multi-vterm exec-path-from-shell company highlight-parentheses doom-themes magit dumb-jump flycheck-popup-tip-mode flycheck-popup-tip flycheck vterm undo-fu-session undo-fu diff-hl fd-dired diredfl all-the-icons-dired goggles doom-modeline hl-todo orderless vertico use-package))
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#282c34"))
 '(pos-tip-background-color "#2f2f2e")
 '(pos-tip-foreground-color "#999891")
 '(rustic-ansi-faces
   ["#282c34" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(size-indication-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#8ac6f2" "#2f2f2e" 0.2))
 '(term-default-bg-color "#2a2a29")
 '(term-default-fg-color "#8d8b86")
 '(tool-bar-mode nil)
 '(tool-bar-position 'bottom)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#ffb4ac")
     (40 . "#f3a0bb068dbb")
     (60 . "#ecf0bdf57dd8")
     (80 . "#e5c06d")
     (100 . "#d1fcc2679b35")
     (120 . "#c551c35ab143")
     (140 . "#b610c464c727")
     (160 . "#a327c588dd05")
     (180 . "#8ac6f2")
     (200 . "#89fec7dad1d0")
     (220 . "#8863c85ec150")
     (240 . "#85eec8dcb0cf")
     (260 . "#82a3c956a041")
     (280 . "#7ec98f")
     (300 . "#9131c244b2d6")
     (320 . "#98acbe43c439")
     (340 . "#9f20ba15d58f")
     (360 . "#a4b5e6")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#2a2a29" "#2f2f2e" "#504341" "#ffb4ac" "#3d464c" "#8ac6f2" "#4c4536" "#e5c06d" "#41434a" "#a4b5e6" "#4d3936" "#e5786d" "#3b473c" "#7ec98f" "#8d8b86" "#74736f"))
 '(window-divider-mode nil)
 '(xterm-color-names
   ["#2f2f2e" "#ffb4ac" "#8ac6f2" "#e5c06d" "#a4b5e6" "#e5786d" "#7ec98f" "#e8e5db"])
 '(xterm-color-names-bright
   ["#2a2a29" "#ddaa6f" "#6a6a65" "#74736f" "#8d8b86" "#834c98" "#999891" "#f6f3e8"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
