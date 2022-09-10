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
;(if WITH-INTERNETS (unless package-archive-contents (package-refresh-contents)))

;; Have use-package
(dolist (package '(use-package))
   (unless (package-installed-p package)
       (package-install package)))
(require 'use-package-ensure)
;(setq use-package-always-ensure WITH-INTERNETS)


;;; Packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; some stats to gather here:
;; (setq use-package-compute-statistics t)


;; ;; quelpa allows use-package install from git
;; (unless (package-installed-p 'quelpa)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
;;     (eval-buffer)
;;     (quelpa-self-upgrade)))

;; (use-package quelpa-use-package)

(defun k--other-buffer () (interactive) (switch-to-buffer (other-buffer (current-buffer))))

(use-package which-key
  :init
  (setq which-key-show-early-on-C-h t
	which-key-idle-delay 1
	which-key-idle-secondary-delay 0.05)
  :config
  (which-key-mode 1))

(use-package evil
  :init
  (setq evil-want-keybinding nil  ;; otherwise we get a warning: https://github.com/emacs-evil/evil-collection/issues/60
        evil-want-C-i-jump nil  ;; fixing TAB behaviour: https://github.com/Somelauw/evil-org-mode#common-issues
        evil-respect-visual-line-mode t  ;; so that j/k don't skip multiple lines at once
        ;evil-undo-system 'undo-tree  ;; TBD
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

(use-package evil-collection
  :config
  (evil-collection-init))

(progn
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'emacs (kbd "M-SPC"))

  (defvar k--leader-map (make-sparse-keymap))
  (define-key evil-normal-state-map (kbd "SPC") k--leader-map)
  (defmacro spc-def (keys fun &rest rest)
    `(which-key-add-keymap-based-replacements k--leader-map
       ,keys (cons ,(cadr rest) ,(if (null fun) `(make-sparse-keymap) fun)))
    )

  (which-key-add-keymap-based-replacements
    k--leader-map
    "b" `("buffer" . ,(make-sparse-keymap))
    "bb" '("switch" . switch-to-buffer)
    "bd" '("delete" . evil-delete-buffer)
    "br" '("revert" . revert-buffer)
    "bs" `("scratch" . ,(lambda () (interactive) (switch-to-buffer "*scratch*")))
    "f" `("file" . ,(make-sparse-keymap))
    "ff" '("find" . find-file)
    "fj" '("jump" . dired-jump)
    "fr" '("recent" . recentf-open-files)
    "fs" '("save" . save-buffer)
    "q" `("quit" . ,(make-sparse-keymap))
    "qf" '("frame" . delete-frame)
    "qq" '("quit" . save-buffers-kill-emacs)
    "t" `("toggle" . ,(make-sparse-keymap))
    "tT" '("truncate-lines" . toggle-truncate-lines)
    "tw" `("whitespace" . ,(make-sparse-keymap))
    "twi" '("indent-tabs" . indent-tabs-mode)
    "two" '("ws options" . whitespace-toggle-options)
    "tww" '("ws mode" . whitespace-mode)
    "w" `("window" . ,(make-sparse-keymap))
    "w/" '("vsplit" . split-window-right)
    "w-" '("split" . split-window-below)
    "wd" '("delete" . delete-window)
    "wo" '("only this" . delete-other-windows)
    "TAB" '("other buffer" . k--other-buffer)
     )

  ;; non-leader
  (evil-define-key 'normal 'global (kbd "<f2>") 'evil-window-next)
  )

