(defconst k-desk-packages
  '(
    org
    (org-bullets :excluded t)
    ))


(defun k-desk/post-init-org ()
  (setq org-startup-indented t))
