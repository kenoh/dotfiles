(defun k/go-to-init () (interactive) (find-file (concat user-emacs-directory "/init.el")))
(use-package evil-leader
  :init
  (setq
   ;; This one is funny... The option is a workaround for evil-collection [https://github.com/emacs-evil/evil-collection/issues/60] but...
   ;; we cannot set it in evil package's :init as suggested because global-evil-leader-mode loads evil but...
   ;; it has to run first (as per note its docs).
   ;; Told ya its funny.
   evil-want-keybinding nil)
  :config
  (defun my-leader (key symbol &rest rest)
    (evil-leader/set-key
      key
      (if (eq :wk (car rest))
          (cons (cadr rest) symbol)
        symbol)))
  (evil-leader/set-leader "<SPC>")
  (global-evil-leader-mode 1))


(use-package evil
  :after (evil-leader)
  :init
  (setq
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

