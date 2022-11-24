;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Built-in options only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Server
(ignore-errors (server-start))

;; Killing Emacs
(require 'files)
(setq confirm-kill-emacs 'y-or-n-p)

;; Performance
(setq gc-cons-threshold (* 100 1000 1000))  ; lsp
(setq read-process-output-max (* 1024 1024)) ;; 1mb, also lsp

;; Backups
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; GUI
(ignore-errors
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))

(defun paste-primary-selection ()
  (interactive)
  (insert
   (x-get-selection 'PRIMARY)))
(global-set-key (kbd "S-<insert>") 'paste-primary-selection)

(setq ring-bell-function 'ignore
      x-gtk-use-system-tooltips nil
      use-dialog-box nil)

;; modeline
(column-number-mode t)
(size-indication-mode t)

;; Parens
(require 'paren)
(setq show-paren-delay 0.5)
(show-paren-mode 1)

;; Navigation
(setq scroll-preserve-screen-position 'always)

;; Editing
(setq-default indent-tabs-mode nil)

;; VC
(require 'vc-hooks)
(setq-default vc-follow-symlinks t)

;; Windows/Frames
(winner-mode)  ; Gives you window layout undo `C-c<left>' and redo `C-c<right>'

;; recentf
(recentf-mode 1)
(setq recentf-max-saved-items 512)
(run-at-time nil (* 1 60) 'recentf-save-list)


;; Ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)


;; Dired
(require 'dired)
(setq dired-dwim-target t)

;; Save place (save last point position in a file)
(save-place-mode 1)
