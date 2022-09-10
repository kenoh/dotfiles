;;; -*- lexical-binding: t -*-

;;Init debug helpers
;; (toggle-debug-on-quit)
;; (toggle-debug-on-error)
(defvar WITH-INTERNETS t "Whether we should consider ourselves online.")

(setq custom-file "~/.emacs.d/custom.el")

(load "~/.emacs.d/elementary.el")
(load "~/.emacs.d/bootstrap.el")
(load "~/.emacs.d/basic.el")
(load "~/.emacs.d/better.el")
(load "~/.emacs.d/private.el")

(load custom-file)
