;;; -*- lexical-binding: t -*-

;;Init debug helpers
;; (toggle-debug-on-quit)
;; (toggle-debug-on-error)
(defvar WITH-INTERNETS t "Whether we should consider ourselves online.")


;;; Load "layer" toggles
(setq Kserver nil
      Kbasic nil
      Kprivate nil)
(unless (load (expand-file-name "~/.emacs.d/layers.el") t)
  (message "K: Could not find the layers.el file. You are missing out!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Built-in options
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Server
(when Kserver
  (server-start))

;; Killing Emacs
(require 'files)
(setq confirm-kill-emacs 'y-or-n-p)

;; Performance
(setq gc-cons-threshold 100000000)  ; lsp
(setq read-process-output-max (* 1024 1024)) ;; 1mb, also lsp

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
(show-paren-mode  1)

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
(run-at-time nil (* 5 60) 'recentf-save-list)


;; Ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)


;; Dired
(require 'dired)
(setq dired-dwim-target t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dispatch
(when Kbasic
  (load (expand-file-name "~/.emacs.d/basic.el")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Custom-*
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
