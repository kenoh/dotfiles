(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; Ensure we have repos downloaded (especially an issue the first time)
(unless package-archive-contents (package-refresh-contents))

;; Have use-package
(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))

;; Private config
(load-file (expand-file-name "~/.emacs.d/private.el"))
(let ((f 'k--private--before))
  (if (fboundp f) (funcall f)))

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


;;; Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; some stats to gather here:
(setq use-package-compute-statistics t)

;; delight is for :delight in use-package
(use-package delight :ensure t)

(use-package evil :ensure t
  :init
  (setq evil-want-keybinding nil) ;; otherwise we get a warning: https://github.com/emacs-evil/evil-collection/issues/60
  :config
  (evil-mode 1))

(use-package evil-collection :ensure t :after evil
    :config
    (evil-collection-init))

(use-package expand-region :ensure t :defer t)

(use-package undo-tree :ensure t
  :delight)

(use-package smartparens-config :ensure smartparens
  :config
  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode))
  
(use-package evil-smartparens :ensure t
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
  (which-key-mode 1))

(use-package magit :ensure t)

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
  :delight '(:eval (concat " " (projectile-project-name)))
  :init
  (setq projectile-require-project-root nil)
  :config
  ;; (projectile-mode 1) ; (counsel-projectile-mode) runs this for us
  )

(use-package company :ensure t
  :init 
  (add-hook 'prog-mode-hook 'company-mode-on))

(use-package persistent-scratch :ensure t
  :config
  (persistent-scratch-setup-default))

;; Org ------------------------------------------
(use-package org :ensure t
  :init
  (setq org-hide-leading-stars t)
  :config
  (add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
  (k-private-org))

(use-package org-roam :ensure t :after org :delight
  :init
  (setq org-roam-directory "~/org/roam")
  (add-hook 'after-init-hook 'org-roam-mode))

;; Languages ------------------------------------
(use-package lsp-mode :ensure t)
(use-package lsp-ui :ensure t)


;; Keybindings ----------------------------------
(use-package general :ensure t :config
  (general-define-key
   :states '(normal visual insert emacs)
   :keymaps 'override
   :prefix "SPC"
   :non-normal-prefix "M-SPC"
   "SPC" '(counsel-M-x :which-key "M-x")
   "v" '(er/expand-region :which-key "expand region")
   ;; quit
   "q" '(:ignore t :which-key "quit")
   "qf" '(delete-frame :which-key "delete frame")
   "qq" '(kill-emacs :which-key "quit")
   ;; buffer
   "TAB" '(evil-switch-to-windows-last-buffer :which-key "previous buffer")
   "b" '(:ignore t :which-key "buffer")
   "bb" '(ivy-switch-buffer :which-key "switch buffer")
   "bd" '(evil-delete-buffer :which-key "delete buffer")
   ;; window
   "w" '(:ignore t :which-key "window")
   "w/" '(split-window-right :which-key "split right")
   "w-" '(split-window-below :which-key "split bottom")
   "wd" '(delete-window :which-key "delete window")
   "wo" '(delete-other-windows :which-key "single window")
   ;; file
   "f" '(:ignore t :which-key "file")
   "fs" '(save-buffer :which-key "save buffer")
   "ff" '(counsel-find-file :which-key "find file")
   ;; projectile
   "p" '(:ignore t :which-key "projectile")
   "pp" '(counsel-projectile-switch-project :which-key "switch project")
   "pf" '(counsel-projectile-find-file-dwim :which-key "find file")
   ;; magit
   "g" '(:ignore t :which-key "git")
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
   "orr" '(org-roam :which-key "side bar")
   "orf" '(org-roam-find-file :which-key "find file")
   "ori" '(org-roam-insert : which-key "insert link"))
  (general-define-key
   :states '(normal visual)
   "<f1>" '(evil-window-next :which-key "next window")
   ;; smartparens
   "," '(:ignore t :which-key "smartparens")
   ",0" '(sp-forward-slurp-sexp :which-key "slurp forward")
   ",9" '(sp-backward-slurp-sexp :which-key "slurp backward")
   ",)" '(sp-forward-barf-sexp :which-key "barf forward")
   ",(" '(sp-backward-barf-sexp :which-key "barf backward")
   ",r" '(sp-raise-sexp :which-key "raise"))
  )

;;; Addendum ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let ((f 'k--private--after))
  (if (fboundp f) (funcall f)))

;; Custom-*
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
