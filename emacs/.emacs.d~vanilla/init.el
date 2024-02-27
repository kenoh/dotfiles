;; -*- lexical-binding: t -*-

;;; PREFACE

;; Init debug helpers
;; (toggle-debug-on-quit)
;; (toggle-debug-on-error)
(defvar WITH-INTERNETS t "Whether we should consider ourselves online.")
;; (defvar WITH-EVIL nil)

(setq custom-file "~/.emacs.d/custom.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ELEMENTARY (Built-in options only)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Server
(ignore-errors (server-start))

;; Killing Emacs
(require 'files)
(setq confirm-kill-emacs 'y-or-n-p)

;; Performance
(setq gc-cons-threshold (* 100 1000 1000))  ; lsp
(setq read-process-output-max (* 1024 1024)) ;; 1mb, also lsp

;; Backups
(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; GUI
(ignore-errors
  (menu-bar-mode 0)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))

(setq frame-resize-pixelwise t)

;; Primary selection keybinding
(progn
 (defun k--paste-primary-selection ()
   (interactive)
   (insert
    (x-get-selection 'PRIMARY)))
 (global-set-key (kbd "S-<insert>") 'k--paste-primary-selection))

;; UI annoyances
(setq ring-bell-function 'ignore
      x-gtk-use-system-tooltips nil
      use-dialog-box nil)

;; Modeline
(column-number-mode t)
(size-indication-mode t)

;; Parens
(require 'paren)
(setq show-paren-delay 0.5)
(show-paren-mode 1)

;; Autoscroll
(setq scroll-preserve-screen-position 'always
      scroll-conservatively 101
      scroll-margin 7)

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
(defun k--recentf-save-list ()
  (let ((save-silently t) (inhibit-message t)) (recentf-save-list)))
(run-at-time nil (* 1 60) 'k--recentf-save-list)

;; Help
(defun k/describe-keymap (arg)
  (interactive "P")
  (pcase arg
    ('nil (call-interactively 'describe-keymap))
    ('(4) (describe-keymap (help-fns--most-relevant-active-keymap)))
    ('(16) (message (help-fns--most-relevant-active-keymap)))))
(global-set-key (kbd "C-h M-k") 'k/describe-keymap)

;; Ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

(ffap-bindings)

;; Dired
(require 'dired)
(setq dired-dwim-target t)
;; (progn
;;   (defun k//mild-revert-buffer (&rest args)
;;     (with-demoted-errors "k//mild-revert-buffer: %S"
;;       (apply 'revert-buffer args)))
;;   (defun k//dired-revert-buffer-hook ()
;;     (setq-local window-selection-change-functions
;;                 (cl-adjoin #'k//mild-revert-buffer window-selection-change-functions)))
;;   (add-hook 'dired-mode-hook #'k//dired-revert-buffer-hook))

;; Save place (save last point position in a file)
(save-place-mode 1)

;; suppres warnings
(setq byte-compile-warnings '(not cl-functions obsolete docstrings))


;; treesitter
(unless (version< emacs-version "29.1")
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          ;; (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          ;; (make "https://github.com/alemuller/tree-sitter-make")
          ;; (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          ;; (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          )
        major-mode-remap-alist
        '(;; (yaml-mode . yaml-ts-mode)
          ;; (bash-mode . bash-ts-mode)
          ;; (js2-mode . js-ts-mode)
          ;; (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          ;; (css-mode . css-ts-mode)
          ;; (python-mode . python-ts-mode)
          )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BOOTSTRAP (packaging and basic packages setup)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Packaging
(require 'package)
(setq package-archives
      '(
	    ("gnu" . "https://elpa.gnu.org/packages/")
	    ("nongnu" . "https://elpa.nongnu.org/nongnu/")

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
;(if WITH-INTERNETS (unless package-archive-contents (package-refresh-contents)))

;; Have use-package
(dolist (package '(use-package))
  (unless (package-installed-p package)
    (package-install package)))
(require 'use-package-ensure)
(setq use-package-always-ensure WITH-INTERNETS)
;; some stats to gather here:
;; (setq use-package-compute-statistics t)


;; ;; quelpa allows use-package install from git
;; (unless (package-installed-p 'quelpa)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
;;     (eval-buffer)
;;     (quelpa-self-upgrade)))

;; (use-package quelpa-use-package)


;;;; Org-Mode early install
(use-package org :pin gnu)
(use-package org-contrib :pin nongnu)

;;;; Which-key
(use-package which-key
  :init
  (setq which-key-show-early-on-C-h t
	which-key-idle-delay 1
	which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode 1))

;;;; General keybindings

(defun k/go-to-init () (interactive) (find-file (concat user-emacs-directory "/init.el")))
(use-package evil-leader
  :config
  (defun my-leader (key symbol &rest rest)
    (evil-leader/set-key
      key
      (if (eq :wk (car rest))
          (cons (cadr rest) symbol)
        symbol)))
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode 1))

(defun k/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(my-leader "b" '(keymap) :wk "buffer")
(my-leader "bk" 'kill-current-buffer :wk "kill")
(my-leader "bs" 'k/switch-to-scratch :wk "scratch")
(my-leader "f" '(keymap) :wk "file")
(my-leader "ff" 'find-file)
(my-leader "fi" 'k/go-to-init)
(my-leader "fs" 'save-buffer)
(my-leader "fj" 'dired-jump)
(my-leader "p" '(keymap) :wk "project")
(my-leader "q" '(keymap) :wk "quit")
(my-leader "qf" 'delete-frame)
(my-leader "qq" 'save-buffers-kill-emacs)
(my-leader "s" '(keymap) :wk "search")
(my-leader "t" '(keymap) :wk "toggle")
(my-leader "tT" 'toggle-truncate-lines)
(my-leader "tw" '(keymap) :wk "whitespace")
(my-leader "twi" 'indent-tabs-mode :wk "indent tabs")
(my-leader "two" 'whitespace-toggle-options :wk "ws options")
(my-leader "tww" 'whitespace-mode :wk "ws mode")
(my-leader "w" '(keymap) :wk "window")
(my-leader "wd" 'delete-window :wk "delete")
(my-leader "wo" 'delete-other-windows :wk "delete other")
(my-leader "w-" 'split-window-below :wk "split below")
(my-leader "w/" 'split-window-right :wk "split right")


;;;; Evil
(use-package evil
  :after (evil-leader)
  :init
  (setq
   ;; otherwise we get a warning [https://github.com/emacs-evil/evil-collection/issues/60]:
   evil-want-keybinding nil
   ;; fixing TAB behaviour in VTEs [https://github.com/Somelauw/evil-org-mode#common-issues]:
   evil-want-C-i-jump nil
   ;; So that j/k don't skip multiple lines at once:
   evil-respect-visual-line-mode t
   evil-undo-system 'undo-redo
   evil-collection-outline-bind-tab-p t)

  ;; Words are usually not what we want to match on '*' or '#' search:
  (set-default 'evil-symbol-word-search t)

  :config
  ;; disable clicking changing the primary selection (clipboard)
  (progn
    (defun nothing() (interactive))
    (define-key evil-normal-state-map (kbd "<down-mouse-1>") 'nothing))

  ;; TODO: why when we have evil-respect-visual-line-mode t?
  (define-key evil-motion-state-map "j" 'evil-next-visual-line)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)

  (global-set-key [f2] 'evil-window-next)
  (my-leader "`" 'evil-switch-to-windows-last-buffer :wk "last buffer")
  (evil-mode 1))

(use-package evil-collection
  :requires (evil)
  :init
  (setq evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))

(use-package evil-org
  :requires (evil org))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BASIC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil-surround :after (evil evil-collection)
  :config
  (global-evil-surround-mode 1))


;; evil-mc: Evil multiple cursors.
;; TODO: review usage
(use-package evil-mc :after (evil evil-collection) :delight
  :config
  (global-evil-mc-mode 1))


;; evil-org: Better org-mode keybings in evil.
(use-package evil-org
  :after (evil org evil-collection)
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


;; evil-owl: Preview marks and registers before using them.
;; This binds to e.g. backtick, `m', ...
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
  (my-leader "v" 'er/expand-region :wk "expand region"))


(use-package rainbow-mode :defer t)


(use-package smartparens :no-require t
  :config
  (use-package smartparens-config :ensure nil  ;; the package name is 'smartparens' and we install it in the parent use-package
    :init
    (my-leader ",0" 'sp-forward-slurp-sexp :wk "slurp )")
    (my-leader ",9" 'sp-backward-slurp-sexp :wk "( slurp")
    (my-leader ",)" 'sp-forward-barf-sexp :wk "barf )")
    (my-leader ",(" 'sp-backward-barf-sexp :wk "( barf")
    (my-leader ",r" 'sp-raise-sexp :wk "raise")
    (my-leader ",s" 'sp-split-sexp :wk "split")
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
  (my-leader "tW" 'which-key-show-top-level :wk "WK show top level")
  (my-leader "tM" 'which-key-show-major-mode :wk "WK show major mode")
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
  (my-leader "gb" 'magit-blame)
  (my-leader "gl" 'magit-log-buffer-file)
  (my-leader "gs" 'magit-status)
  (my-leader "gt" 'magit-toggle-buffer-lock)  ;; Allows for a given buffer not be re-used by magit.
  :init
  (setq magit-display-buffer-function
        (lambda (buf) (display-buffer-same-window buf '()))
        magit-diff-refine-hunk t
        magit-save-repository-buffers nil)
  (with-eval-after-load 'magit  ;; Might be useful for when checking out another branch while having a dirty worktree.
    (transient-append-suffix 'magit-branch "-r"
      '("-m" "Merge local modifications" "--merge"))))


(use-package diff-hl
  :init
  (my-leader "ghj" 'diff-hl-next-hunk)
  (my-leader "ghk" 'diff-hl-previous-hunk)
  (my-leader "ghr" 'diff-hl-revert-hunk)
  (my-leader "ghs" 'diff-hl-show-hunk)
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode 1))


(use-package swiper
  :init
  (my-leader "ss" 'swiper :wk "swiper C-s")
  (my-leader "sS" 'swiper-thing-at-point :wk "swiper C-s"))


(use-package counsel
  :delight ivy-mode
  :delight counsel-mode
  :init
  (my-leader "SPC" 'counsel-M-x :wk "M-x")
  (my-leader "bb" 'ivy-switch-buffer :wk "switch")
  (my-leader "ff" 'counsel-find-file :wk "find File")
  (my-leader "fr" 'counsel-recentf :wk "find Recent file")
  (my-leader "oo" 'counsel-org-files :wk "open org files")
  (my-leader "oC" 'counsel-org-capture :wk "capture")
  (my-leader "jo" 'counsel-outline :wk "outline")
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :config
  (setq counsel-rg-base-command (append counsel-rg-base-command '("--hidden")))  ;; may need fixing since it adds after the search pattern
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (define-key swiper-map [escape] 'minibuffer-keyboard-quit)
  (ivy-mode 1)
  (counsel-mode 1))

(use-package marginalia)

(use-package outshine)

(use-package consult)

;; ivy-prescient does frequency history prioritisation
(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode 1))


(use-package counsel-projectile
  :after (projectile counsel)
  :init
  (my-leader "pp" 'counsel-projectile-switch-project :wk "switch project")
  (my-leader "pf" 'counsel-projectile-find-file :wk "find file")
  (my-leader "pF" 'counsel-projectile-find-file-dwim :wk "find file DWIM")
  (my-leader "ps" 'counsel-projectile-rg :wk "search project")
  :config
  (counsel-projectile-mode 1)
  (counsel-projectile-modify-action 'counsel-projectile-switch-project-action
                                    '((default "D")))  ;; by default will open dired on project switch
  )


(use-package projectile
  ;; :delight '(:eval (concat " <" (projectile-project-name) ">"))
  :init
  ;; (setq projectile-require-project-root nil
  ;;       projectile-switch-project-action #'projectile-dired)
  :config
  ;; (projectile-mode 1) ; (counsel-projectile-mode) runs this for us
  (add-to-list 'projectile-project-root-files "Vagrantfile" t)

  (when t  ;; buffer name tweaks
    (defun k--proj-relative-name (name)
      (rename-buffer
       (ignore-errors
         (concat (projectile-project-name)
                 "|"
                 (file-relative-name name
                                     (projectile-project-root))))))
    (defun k--proj-relative-buf-name ()
      (k--proj-relative-name buffer-file-name))
    (defun k--proj-relative-dired-name ()
      (k--proj-relative-name dired-directory))
    (add-hook 'find-file-hook #'k--proj-relative-buf-name)
    (add-hook 'dired-mode-hook #'k--proj-relative-dired-name))
  )


(use-package company
  :delight " ©"
  :init
  (add-hook 'prog-mode-hook 'company-mode-on))


(use-package persistent-scratch
  :config
  (persistent-scratch-setup-default))


;; (use-package idle-highlight-in-visible-buffers-mode
;;   :config
;;   (remove-hook 'prog-mode-hook 'idle-highlight-in-visible-buffers-mode))


(use-package treemacs
  :init
  (my-leader "tt" 'treemacs)
  (setq treemacs-project-follow-mode t)
  :config
  (use-package treemacs-evil)
  (use-package treemacs-projectile)
  (use-package treemacs-magit)
  ;; Fixes: Clicking near top/bottom of the treemacs window with my default non-zero
  ;; scroll-margin causes unintended drag-n-drop messing up my filesystem.
  (add-hook 'treemacs-mode-hook
            (defun k--set-local-scroll-margin ()
              (setq-local scroll-margin 0))))



(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))



;;;; Perspective
;; (use-package perspective
;;   :config
;;   (use-package persp-projectile)
;;   (use-package treemacs-perspective)
;;   (progn
;;     (my-leader "pP" perspective-map)
;;     (setq persp-suppress-no-prefix-key-warning t))
;;   (persp-mode 1))


;;;; Org
(use-package org :after (easy-kill)
  :init
  (my-leader "a" 'org-agenda :wk "agenda")
  (setq org-hide-leading-stars t
        org-startup-truncated nil
        org-export-initial-scope 'subtree
        org-catch-invisible-edits 'error)
  (set-face-attribute 'link nil :weight 'normal)
  :config
  (add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
  (let ((f 'k-private-org))
    (if (fboundp f) (funcall f)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (python . t)))
  (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))
  (use-package easy-kill)  ;; mine org-src-copy-block dependency
  (defun k/org-copy-src-block ()
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
  (ox-extras-activate '(ignore-headlines))  ;; FIXME: use org from repos
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; BETTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Look'n'feel

(let ((face (car (seq-intersection
                  '("JetBrains Mono NL"
                    "DejaVu Sans Mono"
                    "Monospace")
                  (cons "Monospace" (font-family-list))))))
  (set-face-attribute 'default nil :family face :height 120)
  (set-face-attribute 'variable-pitch nil :family face :height 1.0)
  (set-face-attribute 'fixed-pitch nil :family face :height 1.0))

(setq modus-themes-mixed-fonts t)
(load-theme 'modus-operandi)
(my-leader "tC" 'modus-themes-toggle :wk "toggle theme")


;; Minions hide minor modes into a menu
(use-package minions
  :config
  (add-to-list 'minions-prominent-modes 'projectile-mode)
  (minions-mode 1))


(use-package default-text-scale
  ;; Gives C-M-{-,=,0}
  :config
  (default-text-scale-mode t))


;; FIXME: solaire/dimmer ????
;; dim inactive buffers
(use-package solaire-mode
  :config (solaire-global-mode +1))


;; dim inactive buffers
(use-package dimmer
  :config
  (setq dimmer-adjustment-mode :both)
  (dimmer-configure-which-key)
  (dimmer-mode t))


(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character ?|))


(use-package doom-modeline  
  :hook (after-init . doom-modeline-mode)
  :init (setq doom-modeline-buffer-file-name-style 'truncate-except-project
              doom-modeline-icon nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Auto complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :config
  (setq-default company-backends (cl-remove 'company-dabbrev company-backends)
                company-global-modes '(prog-mode org-mode)
                completion-ignore-case t)
  (global-company-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun k--pull-systemd-env-var (name)
  (with-temp-buffer
    (call-process-shell-command (format "systemctl --user show-environment | sh -c '. /dev/stdin; echo -n $%s'" name)
                                nil '(t nil) nil)
    (setenv name (buffer-string))))
(defun k/import-env ()
  (interactive)
  ;; (keychain-refresh-environment)
  (k--pull-systemd-env-var "KITTY_LISTEN_ON")
  (k--pull-systemd-env-var "KITTY_PUBLIC_KEY"))

(use-package breadcrumb
  :config
  (breadcrumb-mode 1))

(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#00FF00")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("GOTCHA" . "#FF4500")
          ("STUB"   . "#1E90FF")))
  (global-hl-todo-mode 1))

(use-package git-timemachine
  :init
  (my-leader "gT" 'git-timemachine-toggle :wk "timemachine")
  :config
  ;; https://emacs.stackexchange.com/questions/9842/disable-evil-mode-when-git-timemachine-mode-is-activated
  (eval-after-load 'git-timemachine
    '(progn
       (evil-make-overriding-map git-timemachine-mode-map 'normal)
       ;; force update evil keymaps after git-timemachine-mode loaded
       (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))))


;;;; Open new terminal shortcut
(progn
  (defun k/new-term-with-cwd ()
    (interactive)
    (let ((cwd (expand-file-name default-directory)))
      (with-temp-buffer ;"*k/new-term-with-cwd*"
        (cl-case 2
          (1 (call-process "terminator" nil 0 nil
                           "--new-tab"
                           (concat "--working-directory="
                                   cwd)))
          (2 (call-process "kitten" nil t nil
                           "@"
                           "--to" (getenv "KITTY_LISTEN_ON")
                           "--password=emacs"
                           "launch"
                           "--type=tab"
                           (concat "--cwd=" cwd)))))))
  (my-leader "jt" 'k/new-term-with-cwd :wk "new term tab"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PROG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; LSP
(use-package lsp-mode :defer t
  :hook ((rust-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (python-mode . lsp))
  :commands (lsp lsp-deferred)
  ;; :init
  ;; (my-leader ";" nil :wk "LSP fu")
  ;; (my-leader ";;" nil :wk "lsp" :keymap 'lsp-mode-map)
  :config
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t))))


(use-package lsp-ui
  :defer t
  ;; :init
  ;; (my-leader ";'" nil :wk "lsp ui" :keymap 'lsp-ui-mode-map)
  :hook (lsp-mode . lsp-ui-mode))


(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))


(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)


;;;; Python

(use-package pyvenv)

;; (use-package lsp-pyright :defer t
;;   :init
;;   (setq lsp-pyright-disable-language-service nil
;;         lsp-pyright-disable-organize-imports nil
;;         lsp-pyright-auto-import-completions t
;;         lsp-pyright-use-library-code-for-types t)
;;   :hook ((python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))))

(use-package jinja2-mode :defer t)

;;;; YAML and Ansible

;; (use-package yaml-pro)  ;; for interesting stuff we need treesitter, hence emacs29.
(use-package yaml-tomato)
(use-package yaml-mode)

(use-package poly-ansible)


;;;; Utils

;; (use-package helm-dash
;;   ;; UPSTREAM UNMAINTAINED PACKAGE!!!
;;   :config
;;   (setq helm-dash-common-docsets
;;         (let* ((rexp "\\.docset$")
;;                (files (directory-files (concat helm-dash-docsets-path) nil rexp))
;;                (names (cl-mapcar (lambda (s) (replace-regexp-in-string rexp "" s)) files)))
;;           names)))

(use-package devdocs)



;;; PRIVATE
(load "~/.emacs.d/private.el")



;;; ADDENDUM
(load custom-file)
