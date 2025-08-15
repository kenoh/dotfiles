
;;;; LSP
(use-package lsp-mode :defer t
  :hook ((rust-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration)
         (python-mode . lsp)
         (terraform-mode . lsp))
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

