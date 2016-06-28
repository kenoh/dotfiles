;;; install
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;; setup basic
(setf inhibit-startup-screen t)
(setf show-trailing-whitespace t)
(set-face-inverse-video 'region t)
(global-hl-line-mode 1)
(menu-bar-mode -1)
(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (set-face-attribute 'default nil :height 130 :family "Hack")
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


;; semantic
(require 'semantic)  ; semantic itself
(require 'semantic/ia)  ; adds more completion options
(require 'semantic/bovine/gcc)  ; use gcc for from system headers completion

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)

(semantic-mode 1)


;; my quick access
(require 'dash)
(bind-keys
 ("C-x <f7>" . (lambda () (interactive) (find-file "~/notes.org" t)))
 ("C-x <f8>" . (lambda () (interactive) (find-file "~/todo.org" t))))



;;; init use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; packages
(use-package "helm"
  :ensure t
  :bind (("M-x"     . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-x b"   . helm-buffers-list)
	 ("M-y"     . helm-show-kill-ring))
  :init (progn
	  (use-package helm-projectile)
	  (use-package helm-mode)
	  (use-package helm-buffers)
	  (use-package helm-files)
	  (use-package helm-locate)
	  (use-package helm-misc)
	  (use-package helm-match-plugin)
	  (use-package helm-ring)))


(use-package undo-tree
  :ensure t
  :init (progn
	  (global-undo-tree-mode)
	  (setq undo-tree-visualizer-timestamps t)
	  (setq undo-tree-visualizer-diff t)))


(use-package "which-key"
  :ensure t
  :config (which-key-mode))


(use-package "projectile"
  :ensure t)


(use-package "org"
  :ensure t
  :config (progn
	    (add-to-list 'org-structure-template-alist
			 '("s" "#+NAME: ?\n#+BEGIN_SRC \n\n#+END_SRC"))
	    (add-to-list 'org-structure-template-alist
			 '("se" "#+NAME: ?\n#+BEGIN_SRC emacs-lisp\n\n#+END_SRC"))))


(use-package "magit"
  :ensure t
  :config (progn
	    (add-hook 'prog-mode 'magit-mode)))


(use-package "git-gutter-fringe"
  :ensure t
  :config (global-git-gutter-mode t))


(use-package "rpm-spec-mode"
  :ensure t
  :config (progn
	    (autoload 'rpm-spec-mode "rpm-spec-mode.el" "RPM spec mode." t)
	    (setq auto-mode-alist (append '(("\\.spec" . rpm-spec-mode))
					  auto-mode-alist))))


(use-package "company"
  :ensure t
  :commands global-company-mode
  :init (global-company-mode)
  :config (progn
	    (setq company-tooltip-limit 20
		  company-idle-delay .3
		  company-echo-delay 0
		  company-begin-commands '(self-insert-command))))


(use-package "outshine"
  :ensure t
  :config (add-hook 'outline-minor-mode-hook 'outshine-hook-function))


(use-package "paredit"
  :ensure t
  :config (progn
	    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
	    (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)))


;; load private settings
(load "~/.emacs.d/private.el")
