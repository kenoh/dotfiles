;;; -*- lexical-binding: t -*-


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


(use-package expand-region :defer t
  :init
  (which-key-add-keymap-based-replacements k--leader-map
    "v" '("expand region" . er/expand-region)))


(use-package undo-tree
  :delight
  :config
  (global-undo-tree-mode))


(use-package rainbow-mode :defer t)


(use-package smartparens :no-require t
  :config
  (use-package smartparens-config :ensure nil  ;; the package name is 'smartparens' and we install it in the parent use-package
    :init
    (which-key-add-keymap-based-replacements k--leader-map
      "v" '("expand region" . er/expand-region)
      ",0" '("slurp>" . sp-forward-slurp-sexp)
      ",9" '("slurp<" . sp-backward-slurp-sexp)
      ",)" '("barf>" . sp-forward-barf-sexp)
      ",(" '("barf<" . sp-backward-barf-sexp)
      ",r" '("raise" . sp-raise-sexp)
      ",s" '("split" . sp-split-sexp))
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
  (spc-def "tW" 'which-key-show-top-level :wk "WK show top level")
  (spc-def "tM" 'which-key-show-major-mode :wk "WK show major mode")
  (setq which-key-separator " ")
  :config
  (add-to-list 'which-key-replacement-alist '((nil . "^org-agenda") . (nil . "OA")))
  (add-to-list 'which-key-replacement-alist '((nil . "^evil") . (nil . "E")))
  (add-to-list 'which-key-replacement-alist '((nil . "^magit") . (nil . "M")))
  (add-to-list 'which-key-replacement-alist '((nil . "^org") . (nil . "O")))
  (which-key-mode 1))


(use-package magit
  :defer t
  :commands (magit-status)
  :init
  (spc-def "gb" 'magit-blame :wk "magit blame")
  (spc-def "gl" 'magit-log-buffer-file :wk "magit log file")
  (spc-def "gs" 'magit-status :wk "magit status")
  :init
  (setq magit-display-buffer-function
        (lambda (buf) (display-buffer-same-window buf '()))
        magit-diff-refine-hunk t
        magit-save-repository-buffers nil))


(use-package diff-hl
  :init
  (spc-def "gh" nil :wk "hunk")
  (spc-def "ghj" 'diff-hl-next-hunk :wk "next")
  (spc-def "ghk" 'diff-hl-previous-hunk :wh "previous")
  (spc-def "ghr" 'diff-hl-revert-hunk :wk "revert")
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode 1))


(use-package swiper
  :init
  (spc-def "ss" 'swiper :wk "swiper C-s")
  (spc-def "sS" 'swiper-thing-at-point :wk "swiper C-s"))


(use-package counsel
  :delight ivy-mode
  :delight counsel-mode
  :init
  (spc-def "SPC" 'counsel-M-x :wk "M-x")
  (spc-def "bb" 'ivy-switch-buffer :wk "switch")
  (spc-def "ff" 'counsel-find-file :wk "find File")
  (spc-def "fr" 'counsel-recentf :wk "find Recent file")
  (spc-def "oo" 'counsel-org-files :wk "open org files")
  (spc-def "oC" 'counsel-org-capture :wk "capture")
  (spc-def "jo" 'counsel-outline :wk "outline")
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :config
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (define-key swiper-map [escape] 'minibuffer-keyboard-quit)
  (ivy-mode 1)
  (counsel-mode 1))


;; ivy-prescient does frequency history prioritisation
(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1))


(use-package counsel-projectile
  :after (projectile counsel)
  :init
  (spc-def "pp" 'counsel-projectile-switch-project :wk "switch project")
  (spc-def "pf" 'counsel-projectile-find-file-dwim :wk "find file")
  (spc-def "ps" 'counsel-projectile-rg :wk "search project")
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


(use-package treemacs
  :init
  (spc-def "tt" 'treemacs :wk "treemacs")
  )


(use-package easy-kill)  ;; mine org-src-copy-block dependency


;; Org ------------------------------------------
(use-package org :after (easy-kill)
  :init
  (spc-def "a" 'org-agenda :wk "agenda")
  (setq org-hide-leading-stars t
        org-startup-truncated nil
        org-export-initial-scope 'subtree)
  :config
  (add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
  (let ((f 'k-private-org))
    (if (fboundp f) (funcall f)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)))
  (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))
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
