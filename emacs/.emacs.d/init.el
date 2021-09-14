;;; -*- lexical-binding: t -*-

(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Ensure we have repos downloaded (especially an issue the first time)
(unless package-archive-contents (package-refresh-contents))

;; Server
(server-start)

;; Killing Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Have use-package
(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))

;; Performance
(setq gc-cons-threshold 100000000)  ; lsp
(setq read-process-output-max (* 1024 1024)) ;; 1mb, also lsp

;; GUI
(ignore-errors
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))

;; modeline
(column-number-mode t)
(size-indication-mode t)

;; Parens
(setq show-paren-delay 0)
(show-paren-mode  1)

;; Navigation
(setq scroll-preserve-screen-position 'always)

;;; Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some stats to gather here:
(setq use-package-compute-statistics t)

;; delight is for :delight in use-package
(use-package delight :ensure t
  :config
  (delight '((evil-collection-unimpaired-mode nil evil-collection-unimpaire)
	     (auto-revert-mode " AR" t))))

(use-package dired :ensure nil
  :init
  (setq dired-dwim-target t))

(use-package evil :ensure t :after (persistent-scratch)
  :init
  (setq evil-want-keybinding nil) ;; otherwise we get a warning: https://github.com/emacs-evil/evil-collection/issues/60
  (setq evil-respect-visual-line-mode t) ;; so that j/k don't skip multiple lines at once
  (setq evil-undo-system 'undo-tree)
  (set-default 'evil-symbol-word-search t)  ;; because words are usually not what we want to match on '*' or '#' search
  :config
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  (progn "modeline bg color"
	 (let* ((orig "#cccccc")
		(other `((normal . ,orig)
			 (insert . "dark sea green")
			 (emacs . "medium purple")
			 (visual . "light yellow")
			 (motion . "sky blue"))))
	   (dolist (it other)
	     (let ((entry-hook (concat "evil-" (symbol-name (car it)) "-state-entry-hook"))
	     	   (exit-hook (concat "evil-" (symbol-name (car it)) "-state-exit-hook")))
		(let ((entry-fn (intern (concat "--k-" entry-hook "-f")))
		      (exit-fn (intern (concat "--k-" exit-hook "-f"))))
		  (defalias entry-fn
			  `(lambda () ,(symbol-name entry-fn) (set-face-attribute 'mode-line nil :background ,(cdr it))))
		(defalias exit-fn
			  `(lambda () ,(symbol-name exit-fn) (set-face-attribute 'mode-line nil :background ,orig)))
	     	(add-hook (intern entry-hook) entry-fn)
	     	(add-hook (intern exit-hook) exit-fn))))))
  (evil-mode 1))

(use-package evil-collection :ensure t :after (evil)
    :config
    (evil-collection-init))

(use-package evil-surround :ensure t :after (evil evil-collection)
  :config
  (global-evil-surround-mode 1))

(use-package evil-mc :ensure t :after (evil evil-collection) :delight
  :config
  (global-evil-mc-mode 1))

(use-package evil-org :ensure t
  :after (evil org evil-collection)
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-owl :ensure t :after evil :delight
  :config
  (setq evil-owl-max-string-length 500)
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  (evil-owl-mode))

(use-package expand-region :ensure t :defer t)

(use-package undo-tree :ensure t
  :delight
  :config
  (global-undo-tree-mode))

(use-package rainbow-mode :ensure t :defer t)

(use-package smartparens-config :ensure smartparens
  :config
  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode))
  
(use-package evil-smartparens :ensure t :requires (evil)
  :init (require 'evil-smartparens)
  :after (evil general smartparens evil-surround))

(use-package symex :ensure t :config :disabled t
  (global-set-key (kbd "s-;") 'symex-mode-interface)
  (dolist (mode-name symex-lisp-modes)
    (let ((mode-hook (intern (concat (symbol-name mode-name) "-hook"))))
      (add-hook mode-hook 'symex-mode))))

(use-package which-key :ensure t :delight
  :init
  (setq which-key-separator " ")
  :config
  (add-to-list 'which-key-replacement-alist '((nil . "^org-agenda") . (nil . "OA")))
  (add-to-list 'which-key-replacement-alist '((nil . "^evil") . (nil . "E")))
  (add-to-list 'which-key-replacement-alist '((nil . "^magit") . (nil . "M")))
  (add-to-list 'which-key-replacement-alist '((nil . "^org") . (nil . "O")))
  (which-key-mode 1))

(use-package magit :ensure t
  :defer t
  :commands (magit-status)
  :init
  (setq magit-display-buffer-function
	(lambda (buf) (display-buffer-same-window buf '()))
	magit-diff-refine-hunk t))

(use-package diff-hl :ensure t
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode 1))

(use-package counsel :ensure t
  :delight ivy-mode
  :delight counsel-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :config
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (define-key swiper-map [escape] 'minibuffer-keyboard-quit)
  (ivy-mode 1)
  (counsel-mode 1))

;; ivy-prescient does frequency history prioritisation
(use-package ivy-prescient :ensure t :after counsel
  :config
  (ivy-prescient-mode 1))

(use-package counsel-projectile :ensure t :after (projectile counsel)
  :config
  (counsel-projectile-mode 1))

(use-package projectile :ensure t
  :delight '(:eval (concat " <" (projectile-project-name) ">"))
  :init
  (setq projectile-require-project-root nil)
  :config
  ;; (projectile-mode 1) ; (counsel-projectile-mode) runs this for us
  (add-to-list 'projectile-project-root-files "Vagrantfile" t)
  (setq-default projectile-switch-project-action 'projectile-vc)
  )

(use-package company :ensure t
  :delight " ©"
  :init 
  (add-hook 'prog-mode-hook 'company-mode-on))

(use-package persistent-scratch :ensure t
  :config
  (persistent-scratch-setup-default))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-projects-backend 'projectile
	dashboard-startup-banner nil
	dashboard-items '((projects . 7)
			  (recents . 12)
			  (agenda . 5)
			  (bookmarks . 5)
			  (registers . 5)))
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))  ;; for new frames
	inhibit-startup-screen nil)
  :config
  (dashboard-setup-startup-hook))

(use-package idle-highlight-in-visible-buffers-mode :ensure t
  :config
  (add-hook 'prog-mode-hook 'idle-highlight-in-visible-buffers-mode))

(use-package treemacs :ensure t)


;; Org ------------------------------------------
(use-package org :ensure t
  :init
  (setq org-hide-leading-stars t
	org-startup-truncated nil)
  :config
  (add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
  (let ((f 'k-private-org))
    (if (fboundp f) (funcall f))))

(use-package org :ensure org-plus-contrib :after (org)
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

;; Languages ------------------------------------
(use-package lsp-mode :ensure t :defer t
  :hook ((rust-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-ui :ensure t
  :defer t
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs :ensure t
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

(use-package lsp-ivy :ensure t
  :commands lsp-ivy-workspace-symbol)

;;; Python
(use-package lsp-pyright :ensure t :defer t
  :init
  (setq lsp-pyright-disable-language-service nil
	lsp-pyright-disable-organize-imports nil
	lsp-pyright-auto-import-completions t
	lsp-pyright-use-library-code-for-types t)
  :hook ((python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))))

(use-package jinja2-mode :ensure t :defer t)

;; Keybindings ----------------------------------
(defun k--other-buffer () (interactive) (switch-to-buffer (other-buffer (current-buffer))))

(use-package general :ensure t :config
  (general-define-key
   :states '(normal visual insert motion emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "SPC" '(counsel-M-x :which-key "M-x")
   "v" '(er/expand-region :which-key "expand region")
   ;; quit
   "q" '(:ignore t :which-key "quit")
   "qf" '(delete-frame :which-key "delete frame")
   "qq" '(save-buffers-kill-emacs :which-key "quit")
   ;; buffer
   "TAB" '(k--other-buffer :which-key "previous buffer")
   "b" '(:ignore t :which-key "buffer")
   "bb" '(ivy-switch-buffer :which-key "switch")
   "bd" '(evil-delete-buffer :which-key "delete")
   "br" '(revert-buffer :which-key "revert")
   ;; window
   "w" '(:ignore t :which-key "window")
   "w/" '(split-window-right :which-key "split right")
   "w-" '(split-window-below :which-key "split bottom")
   "wd" '(delete-window :which-key "delete")
   "wo" '(delete-other-windows :which-key "single window")
   ;; file
   "f" '(:ignore t :which-key "file")
   "ff" '(counsel-find-file :which-key "find file")
   "fj" '(dired-jump :which-key "dired jump")
   "fs" '(save-buffer :which-key "save buffer")
   ;; toggle
   "t" '(:ignore t :which-key "toggle")
   "tt" '(toggle-truncate-lines :which-key "truncate lines")
   "tw" '(which-key-show-top-level :wk "which-key top level")
   ;; projectile
   "p" '(:ignore t :which-key "projectile")
   "pp" '(counsel-projectile-switch-project :which-key "switch project")
   "pf" '(counsel-projectile-find-file-dwim :which-key "find file")
   "ps" '(counsel-projectile-rg :wk "search project")
   ;; magit
   "g" '(:ignore t :which-key "git")
   "gb" '(magit-blame :wk "magit blame")
   "gh" '(:ignore t :wk "hunk")
   "ghj" '(diff-hl-next-hunk :wk "next")
   "ghk" '(diff-hl-previous-hunk :wh "previous")
   "ghr" '(diff-hl-revert-hunk :wk "revert")
   "gl" '(magit-log-buffer-file :wk "magit log file")
   "gs" '(magit-status :which-key "magit status")
   ;; search
   "s" '(:ignore t :which-key "search")
   "ss" '(swiper :which-key "swiper C-s")
   ;; org
   "o" '(:ignore t :which-key "org")
   "oo" '(counsel-org-files :which-key "open org files")
   "oc" '(counsel-org-capture :which-key "capture")
   "oa" '(:ignore t :which-key "agenda")
   "oaa" '(org-agenda :which-key "agenda")
   ;; org-roam
   "or" '(:ignore t :which-key "roam")
   "orb" '(org-roam-buffer-toggle :which-key "toggle buffer")
   "orf" '(org-roam-node-find :which-key "find node")
   "ori" '(org-roam-node-insert : which-key "insert node link")
   )
  (general-define-key
   :states '(normal visual)
   ;; smartparens
   "," '(:ignore t :which-key "smartparens")
   ",0" '(sp-forward-slurp-sexp :which-key "slurp forward")
   ",9" '(sp-backward-slurp-sexp :which-key "slurp backward")
   ",)" '(sp-forward-barf-sexp :which-key "barf forward")
   ",(" '(sp-backward-barf-sexp :which-key "barf backward")
   ",r" '(sp-raise-sexp :which-key "raise")
   ",s" '(sp-split-sexp :which-key "split")
   ;; lsp
   ";" '(:ignore t :which-key "LSP fu")
   ";;" '(:package lsp :keymap lsp-mode-map :which-key "lsp")
   ";'" '(:package lsp-ui :keymap lsp-ui-mode-map :which-key "lsp ui")
   )
  (general-define-key
   :states '(normal visual emacs motion insert)
   "<f2>" '(evil-window-next :which-key "next window")
   )
  )

;;; Addendum ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private config
(load (expand-file-name "~/.emacs.d/private.el") t)

;; Custom-*
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
