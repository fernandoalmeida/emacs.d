;;; setup-emacs.el --- Setup Emacs builtin configs

;;; Commentary:

;; Setup Emacs builtin features.

;;; Code:

(use-package emacs
  :ensure nil
  :config 
  (setq 
   ring-bell-function #'ignore
   inhibit-splash-screen t
   inhibit-startup-message t
   column-number-mode t
   redisplay-dont-pause t)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

(provide 'setup-emacs)
;; setup-emacs.el ends here
