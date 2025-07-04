;;; setup-prog.el --- Setup programming-related packages

;;; Commentary:

;; * Tree-Sitter
;;
;; Tree-sitter is a fast, incremental parsing library that gives your editor
;; structured syntax trees — in real time — as you type. It’s used to understand
;; the grammar of programming languages way more accurately than traditional
;; regex-based highlighting.
;;
;; https://github.com/tree-sitter/tree-sitter
;;
;; * Eglot
;;
;; Eglot (Emacs Polyglot) is the Emacs client for LSP, it provides commands for
;; enriching the source code editing features, such as automatic code
;; completion, go-to definition of function/class/module, doc of symbol
;; at-point, refactoring, on-the-fly diagnostics, etc.
;;
;; https://www.gnu.org/software/emacs/manual/html_node/eglot/index.html

;;; Code:

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package elixir-ts-mode
  :ensure t
  :after treesit-auto
  :mode "\\.exs?\\'"
  :hook ((elixir-ts-mode . eglot-ensure)
	 (before-save . eglot-format))
)
  :hook
  ((elixir-ts-mode . eglot-ensure))
)

(use-package eglot
  :ensure nil
  :config
  (add-to-list 'eglot-server-programs
               '(elixir-ts-mode . ("~/.lexical-lsp/_build/dev/package/lexical/bin/start_lexical.sh"))))

(provide 'setup-prog)
;; setup-prog.el ends here
