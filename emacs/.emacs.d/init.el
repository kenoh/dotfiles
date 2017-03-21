(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;;; init req-package
(unless (package-installed-p 'req-package)
  (package-refresh-contents)
  (package-install 'req-package))
(require 'req-package)

;;; init use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; general use packages
(use-package "dash"
  :ensure t)
(use-package "s"
  :ensure t)

(if (display-graphic-p)
    (progn      
      (require 'server)
      (unless (server-running-p)
        (server-mode)
        ;(desktop-save-mode 1)
        (setq confirm-kill-emacs 'y-or-n-p))))

(use-package "solarized-theme"
  :ensure t
  :config (progn
            (setq solarized-scale-org-headlines nil
                  solarized-high-contrast-mode-line t
                  solarized-use-variable-pitch nil
                  solarized-distinct-fringe-background t
		  solarized-distinct-doc-face t
		  solarized-emphasize-indicators t)
            (load-theme 'solarized-light t)))

(setq ff-mono "Liberation Mono"
      ff-sans "Liberation Sans")

(ignore-errors
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (setq cursor-type '(hbar . 3)
	use-dialog-box nil)
  (set-face-attribute 'default nil :height 100 :family ff-mono :foreground "#000000")
  (set-face-attribute 'mode-line nil :height 0.8 :family ff-sans)
  (set-face-attribute 'mode-line-inactive nil :inherit 'mode-line)
  (set-face-attribute 'speedbar-button-face nil :height 0.8 :family ff-sans)
  (set-face-attribute 'speedbar-file-face nil :height 0.8 :family ff-sans)
  (set-face-attribute 'speedbar-directory-face nil :height 0.8 :family ff-sans)
  (set-face-attribute 'speedbar-highlight-face nil :height 0.8 :family ff-sans)
  (set-face-attribute 'speedbar-selected-face nil :height 0.8 :family ff-sans)
  (set-face-attribute 'speedbar-separator-face nil :height 0.8 :family ff-sans)
  (set-face-attribute 'speedbar-tag-face nil :height 0.8 :family ff-sans))

;;; setup basic
(setf inhibit-startup-screen t)
(setf show-trailing-whitespace t)
(setq confirm-kill-emacs 'y-or-n-p)

(setq backup-by-copying t  ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs-backups"))  ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)  ; use versioned backups

(setq scroll-step 1
      scroll-margin 7
      scroll-conservatively 9999)

(setq print-level 15
      print-length 4096
      print-quoted t)

(global-set-key (kbd "<f1>") 'other-window)
(global-set-key (kbd "<f2>") 'mode-line-other-buffer)
(global-set-key (kbd "C-<mouse-4>") 'text-scale-increase)
(global-set-key (kbd "C-<mouse-5>") 'text-scale-decrease)
(global-set-key (kbd "C-M-+") 'text-scale-increase)
(global-set-key (kbd "C-M-_") 'text-scale-decrease)

(add-hook 'dired-mode-hook 'hl-line-mode)

(global-set-key [next] (lambda () (interactive)
			 (setq this-command 'next-line)
			 (next-line
			  (- (window-text-height)
			     next-screen-context-lines))))
(global-set-key [prior] (lambda () (interactive)
			  (setq this-command 'previous-line)
			  (previous-line
			   (- (window-text-height)
			      next-screen-context-lines))))

;; whitespace
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))


;; align-regexp - use spaces instead of tabs
(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))


;; emacs-lisp
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(defun k/sm-greek-lambda ()
  (font-lock-add-keywords nil `(("\\<lambda\\>"
                                 (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                                           ,(make-char 'greek-iso8859-7 107))
                                           nil))))))
(add-hook 'emacs-lisp-mode-hook 'k/sm-greek-lambda)


;; modeline
(size-indication-mode t)  ; show size of buffer in modeline



;; semantic
(require 'cedet)
(require 'semantic)  ; semantic itself
(require 'semantic/ia)  ; adds more completion options
(require 'semantic/bovine/gcc)  ; use gcc for from system headers completion

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-breadcrumbs-mode)



;; C
(setq c-default-style "k&r"
      c-basic-offset 4)


(req-package el-get ;; prepare el-get (optional)
  :force t ;; load package immediately, no dependency resolution
  :config (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get/el-get/recipes")
  (el-get 'sync))


(use-package smart-tabs-mode :ensure t :config
  (progn
    (smart-tabs-insinuate 'c 'javascript 'python)))

(use-package helm :ensure t :diminish helm-mode :config
  (progn
    (require 'helm-config)

    (use-package helm-ls-git :ensure t :config (require 'helm-ls-git))
    (use-package helm-ls-hg :ensure t :config (require 'helm-ls-hg))

    (use-package helm-ag :ensure t :config (require 'helm-ag))

    (setq helm-split-window-in-side-p t
          helm-move-to-line-cycle-in-source t
          helm-ff-search-library-in-sexp t
          helm-scroll-amount 8
          helm-ff-file-name-history-use-recentf t
	  recentf-max-saved-items 100
          helm-M-x-fuzzy-match t)

    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

    (global-set-key (kbd "C-c h o") 'helm-occur)
    (global-set-key (kbd "C-c h g") 'helm-google-suggest)

    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)

    (use-package helm-projectile :ensure t :config (helm-projectile-on))
    (use-package helm-swoop :ensure t :config
      (progn
	(global-set-key (kbd "M-i") 'helm-swoop)
	(setq helm-swoop-split-with-multiple-windows t
	      helm-swoop-split-direction 'split-window-vertically
	      helm-swoop-use-line-number-face t)))

    (helm-mode 1)))

(use-package which-key :ensure t :diminish which-key-mode :config (which-key-mode))


(use-package auto-complete :ensure t :config (ac-config-default))

(use-package undo-tree :ensure t :diminish undo-tree-mode :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package highlight-thing :ensure t :diminish hi-lock-mode :config
  (progn
    (add-hook 'prog-mode-hook 'highlight-thing-mode)
    (add-hook 'text-mode-hook 'highlight-thing-mode)))

(use-package highlight-symbol :ensure t)

(use-package rainbow-delimiters :ensure t :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))

(use-package rainbow-identifiers :ensure t)

;; (use-package highlight-parentheses :ensure t :config
;;   (progn
;;     (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
;;     (add-hook 'text-mode-hook 'highlight-parentheses-mode)))

(use-package visual-regexp :ensure t)

(use-package diff-hl :ensure t :config (add-hook 'dired-mode-hook 'diff-hl-dired-mode))

(use-package ediff :ensure t :config
  (setq ediff-split-window-function 'split-window-horizontally
	ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package projectile :ensure t :config
  (progn
    (add-to-list 'projectile-project-root-files "Vagrantfile" t)
    (setq projectile-switch-project-action 'projectile-dired
          projectile-use-git-grep t
	  projectile-project-name-function (lambda (project-root)
					     (->> project-root
						  (replace-regexp-in-string (concat "^" (getenv "HOME") "/?") "~/")
						  (replace-regexp-in-string "/*$" "")))
	  projectile-mode-line '(:propertize
				 (:eval (if (file-remote-p default-directory)
					    " Pr"
					  (format " %s (%s)"
						  (projectile-project-name)
						  (let ((fn tags-file-name))
						    (if (stringp fn)
							(file-relative-name fn)
						      "?")))))
				 face modeline-buffer-id))
    (use-package ibuffer-projectile :ensure t)
    ;; (use-package projectile-speedbar :ensure t :config
    ;;   (progn
    ;;     (global-set-key (kbd "M-<f2>") 'projectile-speedbar-toggle)
    ;;     (setq projectile-speedbar-projectile-speedbar-enable nil)))
    (projectile-cleanup-known-projects)
    (projectile-global-mode)))

(use-package magit :ensure t :diminish magit-mode :config
  (progn
    (global-set-key (kbd "C-c g") 'magit-status)))

(use-package ggtags :ensure t :config
  (progn
    (add-hook 'c-mode-common-hook
	      (lambda () (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
		      (ggtags-mode 1))))))

(use-package git-gutter-fringe :ensure t :diminish git-gutter-mode :config (global-git-gutter-mode t))

(use-package paredit :ensure t :diminish paredit-mode :config (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package eldoc :diminish eldoc-mode :config (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package macrostep :ensure t)

(use-package nameless :ensure t :config
  (progn
    (setq nameless-global-aliases '(("πσ" . "parsec")))
    (add-hook 'emacs-lisp-mode-hook 'nameless-mode)))

(use-package clojure-mode :ensure t :config
  (progn
    (add-to-list 'exec-path (concat (getenv "HOME") "/bin"))
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (use-package clojure-mode-extra-font-locking :ensure t)
    (use-package cider :ensure t)))

(use-package org :ensure t :defer t :config 
  (progn
    (add-hook 'org-mode-hook 'org-indent-mode)
    ;; active Babel languages
    (require 'ob-shell)
    (require 'ob-python)
    (require 'ob-clojure)
    (require 'cider)
    (setq org-babel-clojure-backend 'cider)
    (org-babel-do-load-languages 'org-babel-load-languages
				 '((sh . t)
				   (emacs-lisp . t)
				   (python . t)
				   (gnuplot . t)
				   (clojure . t)))
    ;; do not bother when C-c C-c
    (defun kenoh/org-confirm-babel-evaluate (lang body)
      (not (member lang (list "emacs-lisp" "clojure"))))
    (setq org-confirm-babel-evaluate 'kenoh/org-confirm-babel-evaluate)))

(use-package yaml-mode :ensure t :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
    (use-package ansible :ensure t :config
      (progn
	(add-hook 'yaml-mode-hook 'ansible)
	(use-package company-ansible :ensure t :config
	  (add-hook 'ansible-hook 'company-mode))
	(use-package ansible-doc :ensure t :config
	  (add-hook 'ansible-hook 'ansible-doc-mode))))))

(el-get-bundle zweifisch/ob-ansible
  (require 'ob-ansible))

(use-package rpm-spec-mode :ensure t :config
  (progn
    (autoload 'rpm-spec-mode "rpm-spec-mode.el" "RPM spec mode." t)
    (setq auto-mode-alist (append '(("\\.spec" . rpm-spec-mode))
                                  auto-mode-alist))))

(use-package lua-mode :ensure t)

(use-package slime :ensure t :config
  (progn
    (setq inferior-lisp-program "sbcl"
	  slime-contribs '(slime-fancy))))

(use-package docker :ensure t)
(use-package dockerfile-mode :ensure t)

(use-package graphviz-dot-mode :ensure t)

;; load private settings
(load "~/.emacs.d/private.el" t)

;; ditch the customize feature
(setq custom-file "~/.emacs.d/custom.el")
;(load custom-file 'noerror)

(require 'ob-ansible)
(req-package-finish)
