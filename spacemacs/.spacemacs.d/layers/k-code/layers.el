(configuration-layer/declare-layers
 '(
   ansible
   (auto-completion :variables
                    auto-completion-return-key-behavior nil
                    auto-completion-tab-key-behavior 'complete)
   better-defaults
   emacs-lisp
   git
   helm
   json
   markdown
   multiple-cursors
   (python :variables
           python-shell-interpreter "python"
           python-test-runner 'pytest)
   syntax-checking
   systemd
   treemacs
   version-control
   yaml
   ))
