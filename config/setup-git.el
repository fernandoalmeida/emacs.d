;;; setup-git.el --- Setup git related packages

;;; Commentary:

;; * Magit
;; 
;; Magit is an interface to the version control system Git, implemented as an
;; Emacs package. It is complete enough to allow users to perform almost all
;; of their daily version control tasks directly from within Emacs.
;;
;; https://github.com/magit/magit

;;; Code:

(use-package transient
  :ensure t)

(use-package magit
  :after transient
  :ensure t)

(use-package magit-todos
  :after magit
  :ensure t
  :config (magit-todos-mode))

(provide 'setup-git)
;; setup-git.el ends here
