;;; setup-emacs.el --- Setup Emacs builtin configs

;;; Commentary:

;; Setup Emacs builtin features.

;;; Code:

(use-package emacs
  :ensure nil
  :config
  (delete-selection-mode 1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (global-hl-line-mode t)
  (global-display-line-numbers-mode)
  (menu-bar-mode -1)
  (setq
   auto-save-default nil
   column-number-mode t
   create-lockfiles nil
   inhibit-splash-screen t
   inhibit-startup-echo-area-message t
   inhibit-startup-message t
   initial-scratch-message ""
   make-backup-files nil
   redisplay-dont-pause t
   ring-bell-function #'ignore
   default-frame-alist '((fullscreen . maximized))
   )
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (electric-indent-mode -1)
  (electric-pair-mode 1)
  (cua-mode t)
  (setq cua-enable-cua-keys nil)
  :hook
  (before-save . delete-trailing-whitespace)
  :custom
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  :bind
  ("M-p" . (lambda () (interactive) (previous-line 5)))
  ("M-n" . (lambda () (interactive) (next-line 5)))
)


(provide 'setup-emacs)
;; setup-emacs.el ends here
