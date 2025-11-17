;;; setup-ai.el --- Setup AI-related packages

(use-package claude-code
  :ensure (claude-code :type git
                       :host github
                       :repo "stevemolitor/claude-code.el")
  :config
  (claude-code-mode)
  (setq claude-code-terminal-backend 'eat)
  (setq claude-code-no-delete-other-windows t)
  (add-to-list 'display-buffer-alist
                 '("^\\*claude"
                   (display-buffer-in-side-window)
                   (side . right)
                   (window-width . 80)))

  :bind-keymap
  ("C-c c" . claude-code-command-map)
  )

(provide 'setup-ai)
;; setup-ai.el ends here
