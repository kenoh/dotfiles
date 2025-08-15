;;;; Packaging
(require 'package)
(setq package-archives
      '(
	    ("gnu" . "https://elpa.gnu.org/packages/")
	    ("nongnu" . "https://elpa.nongnu.org/nongnu/")

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
;; some stats to gather here:
;; (setq use-package-compute-statistics t)


;; ;; quelpa allows use-package install from git
;; (unless (package-installed-p 'quelpa)
;;   (with-temp-buffer
;;     (url-insert-file-contents "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
;;     (eval-buffer)
;;     (quelpa-self-upgrade)))

;; (use-package quelpa-use-package)


;; Update elpa gpg keys to avoid going out-of-date.
(use-package gnu-elpa-keyring-update)
