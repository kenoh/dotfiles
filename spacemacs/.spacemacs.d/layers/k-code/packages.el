(defconst k-code-packages
  '(
    diff-hl
    helm-unicode
    magit
    persistent-scratch
    projectile
    rainbow-mode
    evil
    ))


(defun k-code/post-init-diff-hl ()
  (add-hook 'dired-mode-hook #'diff-hl-dired-mode))


(defun k-code/post-init-magit ()
  (setq-default
   magit-process-password-prompt-regexps (quote ("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
                                                 "^\\(Enter \\)?[Pp]assword\\( for '\\(https?://\\)?\\(?99:.*\\)'\\)?: ?$"
                                                 "^.*'s password: ?$"
                                                 "^Yubikey for .*: ?$"
                                                 "^Enter PIN for .*: ?$"
                                                 "^Enter passphrase for .*"))
   magit-repository-directories '(("~/src/" . 5))
   magit-diff-refine-hunk t
   magit-diff-refine-ignore-whitespace nil
   magit-display-buffer-function (lambda (buffer)
                                   (display-buffer buffer '(display-buffer-same-window)))))


(defun k-code/post-init-persistent-scratch ()
  (setq persistent-scratch-save-file (expand-file-name "~/.emacs.persistent-scratch"))
  (persistent-scratch-setup-default))


(defun k-code/post-init-projectile ()
  (with-eval-after-load "projectile"
    (add-to-list 'projectile-project-root-files "Vagrantfile" t)
    (setq-default projectile-switch-project-action 'projectile-vc)))

(defun k/term-set-cursor-to-block ()
  (send-string-to-terminal "\033[0 q"))
(defun k/term-set-cursor-to-bar ()
  (send-string-to-terminal "\033[5 q"))
(defun k-code/post-init-evil ()
  (progn "Have vertical bar as a cursor in terminal."
         ;; So, there is a package for it, evil-terminal-cursor-changer,
         ;; but this below just seems to be enough for now.
         (when (not (display-graphic-p))
           (add-hook 'evil-insert-state-entry-hook 'k/term-set-cursor-to-bar)
           (add-hook 'evil-normal-state-entry-hook 'k/term-set-cursor-to-block))))

;; These no-op functions are to silence warnings from https://github.com/syl20bnr/spacemacs/blob/c7a103a772d808101d7635ec10f292ab9202d9ee/core/core-configuration-layer.el#L658
(defun k-code/post-init-helm-unicode ()
  t)
(defun k-code/post-init-rainbow-mode ()
  t)
