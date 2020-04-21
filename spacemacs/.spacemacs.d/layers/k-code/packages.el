(defconst k-code-packages
  '(
    diff-hl
    helm-unicode
    magit
    persistent-scratch
    projectile
    rainbow-mode
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
