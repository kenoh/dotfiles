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
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one-light t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)  ; Enable flashing mode-line on errors
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (use-package solaire-mode
    :config (solaire-global-mode +1)))


(use-package default-text-scale
  :config
  (default-text-scale-mode t))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package lsp-mode :defer t
  :hook ((rust-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (python-mode . lsp))
  :commands (lsp lsp-deferred)
  :init
  (spc-def ";" nil :wk "LSP fu")
  (spc-def ";;" 'lsp-mode-map :wk "lsp")
  :config
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t))))


(use-package lsp-ui
  :defer t
  :init
  (spc-def ";'" 'lsp-ui-mode-map :wk "lsp ui")
  :hook (lsp-mode . lsp-ui-mode))


(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :config
  (lsp-treemacs-sync-mode 1))


(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)


;;; Python
(use-package pyvenv)

;; (use-package lsp-pyright :defer t
;;   :init
;;   (setq lsp-pyright-disable-language-service nil
;;         lsp-pyright-disable-organize-imports nil
;;         lsp-pyright-auto-import-completions t
;;         lsp-pyright-use-library-code-for-types t)
;;   :hook ((python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))))

(use-package jinja2-mode :defer t)

;;; YAML/Ansible
(use-package yaml-mode)

(use-package poly-ansible)
