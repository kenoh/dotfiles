;;;; Org-Mode early install
(use-package org :pin gnu)
(use-package org-contrib :pin nongnu)


;; evil-org: Better org-mode keybings in evil.
(use-package evil-org
  :after (evil org evil-collection)
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(use-package org
  :init
  (my-leader "a" 'org-agenda :wk "agenda")
  (setq org-hide-leading-stars t
        org-startup-truncated nil
        org-export-initial-scope 'subtree
        org-catch-invisible-edits 'error
        org-M-RET-may-split-line nil)
  (add-to-list 'org-export-backends 'md)
  (set-face-attribute 'link nil :weight 'normal)
  :config
  (add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
  (let ((f 'k-private-org))
    (if (fboundp f) (funcall f)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((dot . t)
     (python . t)
     (emacs-lisp . t)
     (shell . t)))
  (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))
  )


(use-package org  ;; :ensure org-plus-contrib
  :after (org)
  :config
  (require 'ox-extra)
  ;; NOTE: I use ignore-headlines in my invoices.
  (ox-extras-activate '(ignore-headlines))  ;; FIXME: use org from repos
  )


(use-package easy-kill)
(use-package emacs :ensure nil :after (org easy-kill)
  :config
  (defun k/org-copy-src-block ()
    "Copies contents of org's src_block."
    (interactive)
    (org-edit-src-code)
    (mark-whole-buffer)
    (easy-kill 1)
    (org-edit-src-abort)))
