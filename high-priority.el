;;; high-priority.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;;; Runs the high-priority misc commands.

;;; Code:

;; Start the server for emacsclient
(server-start)

;; Save auto-save backup files in a dedicated directory
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;;; high-priority.el ends here
