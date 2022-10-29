;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Look'n'feel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config (setq doom-modeline-buffer-file-name-style 'truncate-upto-project
                doom-modeline-minor-modes t))


;; Minions hide minor modes into a menu
(use-package minions
  :config (minions-mode 1))


(use-package doom-themes
  :config
  (dolist (v '(("L" "theme light" doom-one-light) ("D" "theme dark" doom-one)))
    (spc-def (concat "t" (car v)) (lambda () (interactive) (load-theme (caddr v))) :wk (cadr v)))

  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t  ; if nil, italics is universally disabled
        doom-one-brighter-comments t
        doom-one-light-brighter-comments t
        )
  (load-theme 'doom-one-light t)
  (doom-themes-visual-bell-config)  ; Enable flashing mode-line on errors
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (use-package solaire-mode
    :config (solaire-global-mode +1)))


(use-package default-text-scale
  ;; Gives C-M-{-,=,0}
  :config
  (default-text-scale-mode t))


;; dim inactive buffers
(use-package dimmer
  :config
  (setq dimmer-adjustment-mode :both)
  (dimmer-configure-which-key)
  (dimmer-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :config
  (setq-default company-backends (cl-remove 'company-dabbrev company-backends)
                company-global-modes '(prog-mode org-mode))
  (global-company-mode 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package git-timemachine
  :init
  (spc-def "gT" 'git-timemachine-toggle :wk "timemachine")
  :config
  ;; https://emacs.stackexchange.com/questions/9842/disable-evil-mode-when-git-timemachine-mode-is-activated
  (eval-after-load 'git-timemachine
  '(progn
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
     ;; force update evil keymaps after git-timemachine-mode loaded
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))))


(progn
  (defun k-new-term-with-cwd ()
    (interactive)
    (let ((cwd (expand-file-name default-directory)))
      (call-process "terminator" nil 0 nil
                    "--new-tab"
                    (concat "--working-directory="
                            cwd))))
  (spc-def "jt" 'k-new-term-with-cwd :wk "new term tab"))
