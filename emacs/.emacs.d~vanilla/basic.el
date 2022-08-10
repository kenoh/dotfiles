;;; -*- lexical-binding: t -*-

;;; Packaging ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-archives
      '(
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


;; delight is for :delight in use-package, aka better diminish.
;; (use-package delight
;;   :config
;;   (delight '((evil-collection-unimpaired-mode nil evil-collection-unimpaired)
;;              (auto-revert-mode " AR" t))))


(use-package evil :after (persistent-scratch)
  :init
  (setq evil-want-keybinding nil ;; otherwise we get a warning: https://github.com/emacs-evil/evil-collection/issues/60
        evil-want-C-i-jump nil ;; fixing TAB behaviour: https://github.com/Somelauw/evil-org-mode#common-issues
        evil-respect-visual-line-mode t ;; so that j/k don't skip multiple lines at once
        evil-undo-system 'undo-tree
        evil-collection-outline-bind-tab-p t)
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

(use-package org
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)))
  (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot))))



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
   "tT" '(toggle-truncate-lines :wk "truncate lines")
   "tt" '(treemacs :wk "treemacs")
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
(when Kprivate
  (load (expand-file-name "~/.emacs.d/better.el") t))

;; Private config
(when Kprivate
  (load (expand-file-name "~/.emacs.d/private.el") t))
