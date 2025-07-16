;;; setup-term.el --- Setup terminal-related packages

(use-package eat
  :ensure t
  :config
  (setq eat-shell "/usr/bin/fish")
  :bind
  (("C-c t" . eat))
  )

(provide 'setup-term)
;;; setup-term.el ends here
