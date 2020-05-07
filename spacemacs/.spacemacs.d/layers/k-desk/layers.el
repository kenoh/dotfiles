(configuration-layer/declare-layer
 '(
   k-code
   ;;
   docker
   (haskell :variables
            haskell-completion-backend 'ghci
            haskell-process-type 'stack-ghci)
   (org :variables
        org-adapt-indentation nil)
   xclipboard
   (shell :variables
          shell-default-height 30
          shell-default-position 'bottom
          shell-default-term-shell "/usr/bin/zsh")

   ))
