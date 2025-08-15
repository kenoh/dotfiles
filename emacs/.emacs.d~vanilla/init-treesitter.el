;; treesitter
(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (bitbake "https://github.com/tree-sitter-grammars/tree-sitter-bitbake")
        ;; (cmake "https://github.com/uyha/tree-sitter-cmake")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (kdl "https://github.com/tree-sitter-grammars/tree-sitter-kdl")
        ;; (make "https://github.com/alemuller/tree-sitter-make")
        ;; (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")
        )
      major-mode-remap-alist
      '((yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (sh-mode . bash-ts-mode)
        ;; (js2-mode . js-ts-mode)
        ;; (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        ;; (css-mode . css-ts-mode)
        ;; (python-mode . python-ts-mode)
        ))

'(  ;; FIXME: Meta-broken, i.e. you canot install straight with use-package, thus cannot currently install kdl-ts-mode.
  (use-package straight)

  (use-package kdl-ts-mode    ;; used by Niri wayland compositor
    :after (straight)
    :straight '(kdl-ts-mode
                :type git
                :host github
                :repo "dataphract/kdl-ts-mode"))
  )

(use-package bitbake-ts-mode)
