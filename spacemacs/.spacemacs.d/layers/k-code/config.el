(global-set-key (kbd "<f2>") 'other-window)
(global-set-key (kbd "<f3>") 'mode-line-other-buffer)

;; FIXME: this does not work (even the spaceline-toggle-* does not)
;; (with-eval-after-load "spaceline-config"
;;   (setq spacemacs-spaceline-additional-segments '((projectile-root)))
;;   (spaceline-compile))

(progn "ediff"
       (progn "workaround for issues when highlighting stuff in ediff-regions-*"
              "source: https://github.com/syl20bnr/spacemacs/issues/12498#issuecomment-522240840"
              (defun bm-adv-without-purpose (fn &rest args)
                (without-purpose (apply fn args)))
              (advice-add #'ediff-clone-buffer-for-region-comparison :around #'bm-adv-without-purpose)))
