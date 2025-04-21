;;; setup-projectile.el --- Setup projectile package

;;; Commentary:

;; Projectile is a project interaction library for Emacs.
;; It provides a powerful set of features operating at the project level,
;; as well as simple heuristics to identify projects.
;;
;; https://github.com/bbatsov/projectile/

;;; Code:

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(provide 'setup-projectile)
;; setup-projectile.el ends here
