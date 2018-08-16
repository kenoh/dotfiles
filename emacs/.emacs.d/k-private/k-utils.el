(defun mod-plist (key fn plist)
  "Modifies a value of `plist` at `key` with a `fn` taking the value as a prameter"
  (let ((pair (-partition-after-item key plist)))
    (pcase pair
      ('() plist)
      (`(,l) l)
      (`(,l1 (,v . ,l2)) (append l1 (list (apply fn (list v))) l2)))))

(require 'ert-expectations)
(expectations
  (expect '() (mod-plist :a (-partial '+ 10) '()))
  (expect '(:a 11) (mod-plist :a (-partial '+ 10) '(:a 1)))
  (expect '(:a 1) (mod-plist :b (-partial '+ 10) '(:a 1)))
  (expect '(:a 11 :b 2) (mod-plist :a (-partial '+ 10) '(:a 1 :b 2)))
  (expect '(:a 1 :b 12) (mod-plist :b (-partial '+ 10) '(:a 1 :b 2)))
  (expect '(:a 1 :b 2) (mod-plist :c (-partial '+ 10) '(:a 1 :b 2)))
  (expect '(:a 1 :b 2 :c 3) (mod-plist :d (-partial '+ 10) '(:a 1 :b 2 :c 3)))
  (expect '(:a 11 :b 2 :c 3) (mod-plist :a (-partial '+ 10) '(:a 1 :b 2 :c 3)))
  (expect '(:a 1 :b 12 :c 3) (mod-plist :b (-partial '+ 10) '(:a 1 :b 2 :c 3)))
  (expect '(:a 1 :b 2 :c 13) (mod-plist :c (-partial '+ 10) '(:a 1 :b 2 :c 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun next-or-first (current lst)
  (let ((finding (--drop-while (not (equal current it)) lst)))
    (or (cadr finding) (car lst))))

(expectations
  (expect nil (next-or-first 1 '()))
  (expect 1 (next-or-first 2 '(1)))
  (expect 1 (next-or-first 1 '(1)))
  (expect 1 (next-or-first 2 '(1 2)))
  (expect 2 (next-or-first 1 '(1 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro gsk (key doc &rest body)
  "Set global `key` binding (with a which-key description being `doc`) to a lambda function with `body`"
  `(progn
     (which-key-add-key-based-replacements ,key ,doc)
     (global-set-key (kbd ,key)
		     (lambda () ,doc (interactive) ,@body))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'k-utils)
