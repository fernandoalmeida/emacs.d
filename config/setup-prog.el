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
  (treesit-auto-install t)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package treesit-fold
  :ensure (treesit-fold
	   :host github
	   :repo "emacs-tree-sitter/treesit-fold")
  :init
  (global-treesit-fold-mode)
  :bind
  ("C-c f f" . treesit-fold-toggle))

(use-package elixir-ts-mode
  :ensure t
  :after treesit-auto
  :mode "\\.exs?\\'"
)

(use-package exunit
  :ensure t
  :hook
  (elixir-ts-mode . exunit-mode)
  )

(use-package eglot
  :ensure nil
  :custom
  (eglot-server-programs
   '(((elixir-ts-mode) . ("~/.lexical-lsp/_build/dev/package/lexical/bin/start_lexical.sh"))
     ((js-ts-mode typescript-ts-mode tsx-ts-mode) . ("typescript-language-server" "--stdio"))))
  :hook
  ((elixir-ts-mode . eglot-ensure)
   (typescript-ts-mode . eglot-ensure)
   (before-save . eglot-format))
  )

(use-package corfu
  :ensure t

  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert)
  (corfu-preselect 'first)
  (corfu-on-exact-match nil)
  (corfu-min-width 30)
  (corfu-max-width 30)

  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode)
  )

(use-package po-mode
  :ensure t
  :mode
  ("\\.po\\'" . po-mode)
)

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc"))

(provide 'setup-prog)
;; setup-prog.el ends here
