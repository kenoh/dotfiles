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
(setq use-package-always-ensure WITH-INTERNETS)


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

(when t
  "General setup"
  (use-package general
	       :defer nil
	       :demand t
    :config
    (defun spc-def (keys &rest rest)
      (general-define-key
        :states '(normal visual insert motion)
        :keymap 'override
        :prefix "SPC"
        :non-normal-prefix "M-SPC"
        keys rest))
    (require 'general)
    (general-override-mode 1)
    (general-define-key
     :states '(normal visual insert motion)
     :keymap 'override
     :prefix "SPC"
     :non-normal-prefix "M-SPC"
     "" '(nil :wk "my lieutanant general prefix")
  
    "b" '(nil :wk "buffer")
    "bb" '(switch-to-buffer :wk "switch")
    "bd" `(,(lambda () (interactive) (kill-buffer (current-buffer))) :wk "delete")
    "br" '(revert-buffer :wk "revert")
    "bs" `(,(lambda () (interactive) (switch-to-buffer "*scratch*")) :wk "scratch")

    "f" '(nil :wk "file")
    "ff" '(find-file :wk "find")
    "fj" '(dired-jump :wk "jump")
    "fr" '(recentf-open-files :wk "recent")
    "fs" '(save-buffer :wk "save")

    "g" '(nil :wk "git")
    "j" '(nil :wk "jump")
    "o" '(nil :wk "org")
    "p" '(nil :wk "project")

    "q" '(nil :wk "quit")
    "qf" '(delete-frame :wk "frame")
    "qq" '(save-buffers-kill-emacs :wk "quit")

    "s" '(nil :wk "search")

    "t" '(nil :wk "toggle")
    "tT" '(toggle-truncate-lines :wk "truncate-lines")

    "tw" '(nil :wk "whitespace")
    "twi" '(indent-tabs-mode :wk "indent-tabs")
    "two" '(whitespace-toggle-options :wk "ws options")
    "tww" '(whitespace-mode :wk "ws mode")

    "w" '(nil :wk "window")
    "w/" '(split-window-right :wk "vsplit")
    "w-" '(split-window-below :wk "split")
    "wd" '(delete-window :wk "delete")
    "wo" '(delete-other-windows :wk "only this")
    "TAB" '(k--other-buffer :wk "other buffer")
    )
    (general-define-key
     :states '(normal visual insert motion)
     :keymap 'override
     "<f2>" '(evil-window-next :wk "next window"))
    )
  )
