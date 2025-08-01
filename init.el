;;; init.el --- Startup entry point

;;; Commentary:

;; This is the entry point of the setup.
;; The config is splitted into multiple files.

;;; Code:

;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
;; (setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

(require 'setup-elpaca)
(require 'setup-emacs)
(require 'setup-theme)
(require 'setup-git)
(require 'setup-nav)
(require 'setup-prog)
(require 'setup-ai)
(require 'setup-term)

(provide 'init)
;;; init.el ends here
