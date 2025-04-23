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
  (set-face-background 'highlight-indentation-current-column-face "#393939")
  :hook
  (prog-mode . (lambda ()
                 (highlight-indentation-current-column-mode 1)
                 (setq highlight-indentation-blank-lines t)))
)

(use-package rainbow-mode
  :ensure t
  :hook prog-mode
)

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (custom-set-faces '(rainbow-delimiters-depth-1-face ((t (:foreground "DeepSkyBlue1")))))
  (custom-set-faces '(rainbow-delimiters-depth-2-face ((t (:foreground "YellowGreen")))))
  (custom-set-faces '(rainbow-delimiters-depth-3-face ((t (:foreground "gold1")))))
  (custom-set-faces '(rainbow-delimiters-depth-4-face ((t (:foreground "tomato")))))
  (custom-set-faces '(rainbow-delimiters-depth-5-face ((t (:foreground "DeepPink")))))
  (custom-set-faces '(rainbow-delimiters-depth-6-face ((t (:foreground "white")))))
)

(provide 'setup-theme)
;; setup-theme.el ends here
