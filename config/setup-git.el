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

(use-package git-timemachine
  :ensure t
  :bind
  (("C-c v t" . git-timemachine))
)

(use-package blamer
  :ensure t
  :bind
  (("C-c v i" . blamer-show-posframe-commit-info)
   ("C-c v w" . blamer-kill-ring-commit-hash))
)

(provide 'setup-git)
;; setup-git.el ends here
