;;; -*- lexical-binding: t -*-

;;; Init debug helpers
;; (toggle-debug-on-quit)
;; (toggle-debug-on-error)
(defvar WITH-INTERNETS t "Whether we should consider ourselves online.")


;;; Built-in options

;; Server
(server-start)

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
(winner-mode)

;; recentf
(recentf-mode 1)
(setq recentf-max-saved-items 512)
(run-at-time nil (* 5 60) 'recentf-save-list)

;; Ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;;; Packaging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives '(
			  ("gnu" . "https://elpa.gnu.org/packages/")

			  ;;("org" . "http://orgmode.org/elpa/")  ;; deprecated since org 9.5
			  ("melpa" . "https://melpa.org/packages/")
			  ("melpa-stable" . "https://stable.melpa.org/packages/")

			  ;; alt github
			  ;; ("melpa" . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/melpa/")
			  ;; ("org"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/org/")
			  ;; ("gnu"   . "https://raw.githubusercontent.com/d12frosted/elpa-mirror/master/gnu/")

			  ;; alt gitlab
			  ;; ("melpa" . "https://gitlab.com/d12frosted/elpa-mirror/raw/master/melpa/")
			  ;; ("org"   . "https://gitlab.com/d12frosted/elpa-mirror/raw/master/org/")
			  ;; ("gnu"   . "https://gitlab.com/d12frosted/elpa-mirror/raw/master/gnu/")
			  ))
(package-initialize)
(if (version< emacs-version "27")
    ;; we are likely on an old system with outdated TLS
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;; Ensure we have repos downloaded (especially an issue the first time)
(if WITH-INTERNETS (unless package-archive-contents (package-refresh-contents)))

;; Have use-package
(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))
(require 'use-package-ensure)
(setq use-package-always-ensure WITH-INTERNETS)


;;; Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some stats to gather here:
;(setq use-package-compute-statistics t)

;; quelpa allows use-package install from git
(unless (package-installed-p 'quelpa)
  (with-temp-buffer
    (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
    (eval-buffer)
    (quelpa-self-upgrade)))

(use-package quelpa-use-package)

;; delight is for :delight in use-package
(use-package delight
  :config
  (delight '((evil-collection-unimpaired-mode nil evil-collection-unimpaired)
             (auto-revert-mode " AR" t))))

(use-package default-text-scale
  :config
  (default-text-scale-mode t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config (setq doom-modeline-buffer-file-name-style 'truncate-upto-project
                doom-modeline-minor-modes t))

(use-package minions
  :config (minions-mode 1))

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one-light t)
  (doom-themes-visual-bell-config)  ; Enable flashing mode-line on errors
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (use-package solaire-mode
    :config (solaire-global-mode +1)))

(use-package dired :ensure nil
  :init
  (setq dired-dwim-target t))

(use-package evil :after (persistent-scratch)
  :init
  (setq evil-want-keybinding nil) ;; otherwise we get a warning: https://github.com/emacs-evil/evil-collection/issues/60
  (setq evil-want-C-i-jump nil) ;; fixing TAB behaviour: https://github.com/Somelauw/evil-org-mode#common-issues
  (setq evil-respect-visual-line-mode t) ;; so that j/k don't skip multiple lines at once
  (setq evil-undo-system 'undo-tree)
  (set-default 'evil-symbol-word-search t)  ;; because words are usually not what we want to match on '*' or '#' search
  :config
  (progn
    (defun nothing() (interactive))
    (define-key evil-normal-state-map (kbd "<down-mouse-1>") 'nothing) ;otherwise this would change primary selection always when just clicking (annoying)
  )
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  (evil-mode 1))

(use-package evil-collection :after (evil)
    :config
    (evil-collection-init))

(use-package evil-surround :after (evil evil-collection)
  :config
  (global-evil-surround-mode 1))

(use-package evil-mc :after (evil evil-collection) :delight
  :config
  (global-evil-mc-mode 1))

(use-package evil-org
  :after (evil org evil-collection)
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-owl :after evil :delight
  :config
  (setq evil-owl-max-string-length 500)
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  (evil-owl-mode))

(use-package expand-region :defer t)

(use-package undo-tree
  :delight
  :config
  (global-undo-tree-mode))

(use-package rainbow-mode :defer t)

(use-package smartparens :no-require t
  :config
  (use-package smartparens-config :ensure nil  ;; the package name is 'smartparens' and we install it in the parent use-package
	       :config
	       (show-smartparens-global-mode t)
	       (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
	       (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)))

(use-package evil-smartparens :requires (evil)
  :init (require 'evil-smartparens)
  :after (evil general smartparens evil-surround))

(use-package symex :config :disabled t
  (global-set-key (kbd "s-;") 'symex-mode-interface)
  (dolist (mode-name symex-lisp-modes)
    (let ((mode-hook (intern (concat (symbol-name mode-name) "-hook"))))
      (add-hook mode-hook 'symex-mode))))

(use-package which-key :delight
  :init
  (setq which-key-separator " ")
  :config
  (add-to-list 'which-key-replacement-alist '((nil . "^org-agenda") . (nil . "OA")))
  (add-to-list 'which-key-replacement-alist '((nil . "^evil") . (nil . "E")))
  (add-to-list 'which-key-replacement-alist '((nil . "^magit") . (nil . "M")))
  (add-to-list 'which-key-replacement-alist '((nil . "^org") . (nil . "O")))
  (which-key-mode 1))

(when nil """Keysee, (which requires Sortie)"""
      ;; FIXME: the dependency between the two packages fails due to some wierd versioning issues.
  (use-package sortie
    :quelpa (sortie :fetcher url
                    :version original
                    :url "https://www.emacswiki.org/emacs/download/sortie.el"))

  (use-package keysee :after (sortie)
    :quelpa (keysee :fetcher url
                    :version original
                    :url "https://www.emacswiki.org/emacs/download/keysee.el")
    ))
(use-package magit
  :defer t
  :commands (magit-status)
  :init
  (setq magit-display-buffer-function
        (lambda (buf) (display-buffer-same-window buf '()))
        magit-diff-refine-hunk t
        magit-save-repository-buffers nil))

(use-package diff-hl
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode 1))

(use-package counsel
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
(use-package ivy-prescient :after counsel
  :config
  (ivy-prescient-mode 1))

(use-package counsel-projectile :after (projectile counsel)
  :config
  (counsel-projectile-mode 1)
  (counsel-projectile-modify-action 'counsel-projectile-switch-project-action
                                    '((default "D")))  ;; by default will open dired on project switch
  )

(use-package projectile
  :delight '(:eval (concat " <" (projectile-project-name) ">"))
  :init
  (setq projectile-require-project-root nil)
  :config
  ;; (projectile-mode 1) ; (counsel-projectile-mode) runs this for us
  (add-to-list 'projectile-project-root-files "Vagrantfile" t))

(use-package company
  :delight " ©"
  :init 
  (add-hook 'prog-mode-hook 'company-mode-on))

(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))

(use-package dashboard
  :init
  (setq dashboard-projects-backend 'projectile
        dashboard-startup-banner nil
        dashboard-items '((agenda . 5)
                          (bookmarks . 5)
                          (registers . 5)))
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))  ;; for new frames
        inhibit-startup-screen nil)
  :config
  (dashboard-setup-startup-hook))

(use-package idle-highlight-in-visible-buffers-mode
  :config
  (add-hook 'prog-mode-hook 'idle-highlight-in-visible-buffers-mode))

(use-package treemacs)

(use-package easy-kill)  ;; mine org-src-copy-block dependency

;; Org ------------------------------------------
(use-package org :after (easy-kill)
  :init
  (setq org-hide-leading-stars t
        org-startup-truncated nil)
  :config
  (add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
  (let ((f 'k-private-org))
    (if (fboundp f) (funcall f)))

  (defun org-copy-src-block ()
    """Copies contents of org's src_block."
    (interactive)
    (org-edit-src-code)
    (mark-whole-buffer)
    (easy-kill 1)
    (org-edit-src-abort)))

(use-package org  ;; :ensure org-plus-contrib
  :after (org)
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

;; Languages ------------------------------------
(use-package lsp-mode :defer t
  :hook ((rust-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (python-mode . lsp))
  :commands (lsp lsp-deferred)
  :config
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t))))

(use-package lsp-ui
  :defer t
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;;; Python
(use-package pyvenv)

;; (use-package lsp-pyright :defer t
;;   :init
;;   (setq lsp-pyright-disable-language-service nil
;;         lsp-pyright-disable-organize-imports nil
;;         lsp-pyright-auto-import-completions t
;;         lsp-pyright-use-library-code-for-types t)
;;   :hook ((python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))))

(use-package jinja2-mode :defer t)

;;; YAML/Ansible
(use-package yaml-mode)

(use-package poly-ansible)


;;; Keybindings ----------------------------------
(defun k--other-buffer () (interactive) (switch-to-buffer (other-buffer (current-buffer))))

(use-package hydra :commands defhydra
  :config
  (defhydra hydra-smartparens (:foreign-keys run :hint nil)
    "
^Slurp^    ^Barf^      ^Other^
^^^^^^^^^^---------------------------------
_0_: ->    _)_: ->     _r_aise
_9_: <-    _(_: <-     _s_plit
"
    ("0" sp-forward-slurp-sexp)
    ("9" sp-backward-slurp-sexp)
    (")" sp-forward-barf-sexp)
    ("(" sp-backward-barf-sexp)
    ("r" sp-raise-sexp)
    ("s" sp-split-sexp)
    ("q" nil "quit" :color blue)))

(use-package general :after (hydra) :config
  (general-define-key
   :states '(normal visual insert motion emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "SPC" '(counsel-M-x :wk "M-x")
   "v" '(er/expand-region :wk "expand region")
   ;; quit
   "q" '(:ignore t :wk "quit")
   "qf" '(delete-frame :wk "delete frame")
   "qq" '(save-buffers-kill-emacs :wk "quit")
   ;; buffer
   "TAB" '(k--other-buffer :wk "previous buffer")
   "b" '(:ignore t :wk "buffer")
   "bb" '(ivy-switch-buffer :wk "switch")
   "bd" '(evil-delete-buffer :wk "delete")
   "br" '(revert-buffer :wk "revert")
   ;; window
   "w" '(:ignore t :wk "window")
   "w/" '(split-window-right :wk "split right")
   "w-" '(split-window-below :wk "split bottom")
   "wd" '(delete-window :wk "delete")
   "wo" '(delete-other-windows :wk "single window")
   ;; file
   "f" '(:ignore t :wk "file")
   "ff" '(counsel-find-file :wk "find File")
   "fr" '(counsel-recentf :wk "find Recent file")
   "fj" '(dired-jump :wk "dired Jump")
   "fs" '(save-buffer :wk "Save buffer")
   ;; toggle
   "t" '(:ignore t :wk "toggle")
   "tt" '(toggle-truncate-lines :wk "truncate lines")
   "tw" '(:ignore t :wk "whitespace")
   "tww" '(whitespace-mode :wk "on/off")
   "two" '(whitespace-toggle-options :wk "options")
   "twi" '((lambda () (interactive)
             (setq indent-tabs-mode (not indent-tabs-mode)))
           :wk "indent-tabs-mode")
   "tW" '(which-key-show-top-level :wk "which-key show top level")
   "tM" '(which-key-show-major-mode :wk "which-key show major mode")
   ;; projectile
   "p" '(:ignore t :wk "projectile")
   "pp" '(counsel-projectile-switch-project :wk "switch project")
   "pf" '(counsel-projectile-find-file-dwim :wk "find file")
   "ps" '(counsel-projectile-rg :wk "search project")
   ;; magit
   "g" '(:ignore t :wk "git")
   "gb" '(magit-blame :wk "magit blame")
   "gh" '(:ignore t :wk "hunk")
   "ghj" '(diff-hl-next-hunk :wk "next")
   "ghk" '(diff-hl-previous-hunk :wh "previous")
   "ghr" '(diff-hl-revert-hunk :wk "revert")
   "gl" '(magit-log-buffer-file :wk "magit log file")
   "gs" '(magit-status :wk "magit status")
   ;; search
   "s" '(:ignore t :wk "search")
   "ss" '(swiper :wk "swiper C-s")
   "sS" '(swiper-thing-at-point :wk "swiper C-s")
   ;; org
   "o" '(:ignore t :wk "org")
   "oo" '(counsel-org-files :wk "open org files")
   "oC" '(counsel-org-capture :wk "capture")
   ;; agenda
   "a" '(org-agenda :wk "agenda")
   ;; org-roam
   "or" '(:ignore t :wk "roam")
   "orb" '(org-roam-buffer-toggle :wk "toggle buffer")
   "orf" '(org-roam-node-find :wk "find node")
   "ori" '(org-roam-node-insert :wk "insert node link")
   "oru" '(org-roam-ui :wk "UI")
   "orr" '(org-roam-refile :wk "refile")
   ;; jump
   "j" '(:ignore t :wk "jump")
   "jo" '(counsel-outline :wk "outline")
   )
  (general-define-key
   :states '(normal visual)
   ;; smartparens
   "," '(hydra-smartparens/body :wk "smartparens")
   ;; lsp
   ";" '(:ignore t :wk "LSP fu")
   ";;" '(:package lsp :keymap lsp-mode-map :wk "lsp")
   ";'" '(:package lsp-ui :keymap lsp-ui-mode-map :wk "lsp ui")
   )
  (general-define-key
   :states '(normal visual emacs motion insert)
   "<f2>" '(evil-window-next :wk "next window")
   )
  )

;;; Addendum ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Private config
(load (expand-file-name "~/.emacs.d/private.el") t)

;; Custom-*
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
