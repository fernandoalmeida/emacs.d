;;; setup-emacs.el --- Setup Emacs builtin configs

;;; Commentary:

;; Setup Emacs builtin features.

;;; Code:

(use-package emacs
  :ensure nil
  :config 
  (fset 'yes-or-no-p 'y-or-n-p)
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
   ring-bell-function #'ignore)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (global-display-line-numbers-mode))

(provide 'setup-emacs)
;; setup-emacs.el ends here
