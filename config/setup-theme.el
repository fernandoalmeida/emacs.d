;;; setup-theme.el --- Setup theme-related packages

;;; Commentary:

;; * Zenburn
;; 
;; A higher contrast version of the popular zenburn theme created by Bozhidar Batsov.
;;
;; https://github.com/edran/hc-zenburn-emacs

;;; Code:

(use-package hc-zenburn-theme
  :ensure t
  :config
  (load-theme 'hc-zenburn t)
  (add-to-list 'default-frame-alist '(font . "DejaVuSansMono 12"))
  (set-face-background 'isearch "brown")
)

(use-package highlight-indentation
  :ensure t
  :config
  (set-face-background 'highlight-indentation-face "#e3e3e3")
  (set-face-background 'highlight-indentation-current-column-face "#393939")
  :hook
  (prog-mode . 'highlight-indentation-current-column-mode)
)

(use-package rainbow-mode
  :ensure t
  :hook
  (prog-mode . 'rainbow-mode)
)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'setup-theme)
;; setup-theme.el ends here
