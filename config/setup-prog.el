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
  :init
  (setq major-mode-remap-alist
	'((elixir-mode . elixir-ts-mode)))
  :config
  (dolist (lang-source
           '((elixir "https://github.com/elixir-lang/tree-sitter-elixir")
             (heex   "https://github.com/phoenixframework/tree-sitter-heex")))
    (add-to-list 'treesit-language-source-alist lang-source))

  (dolist (lang '(elixir heex))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang)))
)

(provide 'setup-prog)
;; setup-prog.el ends here
