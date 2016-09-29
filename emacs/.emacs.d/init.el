;;; install
(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	))
(package-initialize)
;;; init use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; general use packages
(use-package "dash"
  :ensure t)
(use-package "s"
  :ensure t)

;;; setup basic
(setf inhibit-startup-screen t)
(setf show-trailing-whitespace t)
(menu-bar-mode -1)
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(use-package "solarized-theme"
  :ensure t
  :config (progn
	    (setq solarized-scale-org-headlines nil
		  solarized-high-contrast-mode-line t
		  solarized-use-variable-pitch nil
		  solarized-distinct-fringe-background t)
	    (load-theme 'solarized-light t)))
(if (display-graphic-p)
    (progn
      (set-face-attribute 'default nil :height 110 :family "Inconsolata")
      (require 'server)
      (unless (server-running-p)
        (server-mode)
        (desktop-save-mode 1)
        (setq confirm-kill-emacs 'y-or-n-p))))

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

(setq indent-tabs-mode t
      tab-width 4)
(add-hook 'lisp-mode-hook (lambda () (setq indent-tabs-mode nil)))

(setq print-level 15
	  print-length 100
	  print-quoted t)

(defun k/previous-window ()
  (interactive)
  (call-interactively 'other-window))
(global-set-key (kbd "<f1>") 'k/previous-window)

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
(require 'semantic)  ; semantic itself
(require 'semantic/ia)  ; adds more completion options
(require 'semantic/bovine/gcc)  ; use gcc for from system headers completion

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)



;; C
(setq c-default-style "k&r"
      c-basic-offset 4)


;; my quick access
(require 'dash)
(bind-keys
 ("C-x <f7>" . (lambda () (interactive) (find-file "~/notes.org" t)))
 ("C-x <f8>" . (lambda () (interactive) (find-file "~/todo.org" t)))
 ("C-x <f6>" . (lambda () (interactive) (notmuch))))


;;; packages
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

    (helm-mode 1)))


(use-package undo-tree :ensure t :diminish undo-tree-mode :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package which-key :ensure t :diminish which-key-mode :config
  (which-key-mode))

(use-package projectile :ensure t :diminish projectile-mode :config
  (progn
	(use-package helm-projectile :ensure t :config (require 'helm-projectile))
	(projectile-global-mode)
	(setq projectile-completion-system 'helm)
	(helm-projectile-on)))

(use-package org :ensure t :config
  (progn
    ;; custom <-ish templates (use like `<s' and TAB, or `<se' and TAB)
    (add-to-list 'org-structure-template-alist
		 '("s" "#+NAME: ?\n#+BEGIN_SRC \n\n#+END_SRC"))
    (add-to-list 'org-structure-template-alist
		 '("se" "#+NAME: ?\n#+BEGIN_SRC emacs-lisp\n\n#+END_SRC"))
    ;; active Babel languages
    (require 'ob-sh)
    (require 'ob-python)
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((sh . t)
       (emacs-lisp . t)
       (python . t)
       (gnuplot . t)))
    ;; do not ask for evaluation
    (defun k/org-confirm-babel-evaluate (lang body)
      (not (string= lang "emacs-lisp")))
    (setq org-confirm-babel-evaluate 'k/org-confirm-babel-evaluate)
    (add-hook 'org-mode-hook 'org-indent-mode)))


(use-package magit  :ensure t :diminish magit-mode :config
  (progn
    (add-hook 'prog-mode 'magit-mode)))

(use-package git-gutter-fringe :ensure t :diminish git-gutter-mode :config
  (global-git-gutter-mode t))

(use-package rpm-spec-mode :ensure t :config
  (progn
    (autoload 'rpm-spec-mode "rpm-spec-mode.el" "RPM spec mode." t)
    (setq auto-mode-alist (append '(("\\.spec" . rpm-spec-mode))
				  auto-mode-alist))))

(use-package company :ensure t
  :config
  (progn
    (setq company-tooltip-limit 20
	  company-idle-delay .3
	  company-echo-delay 0
	  company-begin-commands '(self-insert-command))))

(use-package outshine :ensure t :config
  (add-hook 'outline-minor-mode-hook 'outshine-hook-function))

(use-package paredit :ensure t :diminish paredit-mode :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package eldoc :diminish eldoc-mode :config
  (add-hook 'emacs-lisp-mode-hook 'eldoc-mode))

(use-package cider :ensure t)

(use-package f :ensure t)

(use-package highlight-thing :ensure t :diminish highlight-thing-mode :config
  (highlight-thing-mode))

(use-package highlight-symbol :ensure t)

(use-package highlight-parentheses :ensure t :diminish highlight-parentheses-mode :config
  (progn
    (global-highlight-parentheses-mode)
    (setq hl-paren-background-colors '("red" "green" "blue" "magenta" "cyan")
          hl-paren-colors nil)))

(use-package "gnuplot" :ensure t :config
  (require 'ob-gnuplot))

(use-package vdiff :ensure t)

(use-package visual-regexp :ensure t)


;; load private settings
(load "~/.emacs.d/private.el" t)
