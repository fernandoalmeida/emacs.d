;;; setup-theme.el --- Setup theme-related packages

;;; Commentary:

;; * Zenburn
;; 
;; A higher contrast version of the popular zenburn theme created by Bozhidar Batsov.
;;
;; https://github.com/edran/hc-zenburn-emacs

;;; Code:

(use-package zenburn-theme
  :ensure t
  :after (vertico magit)
  :init
  (setq current-line-background "dark slate gray")
  (setq selected-region-background "#60594c")
  :config
  (load-theme 'zenburn t)
  (add-to-list 'default-frame-alist '(font . "DejaVuSansMono 12"))
  (set-face-background 'hl-line current-line-background)
  (set-face-background 'vertico-current current-line-background)
  (set-face-background 'magit-section-highlight current-line-background)
  (set-face-background 'magit-diff-file-heading-highlight current-line-background)
  (set-face-background 'region selected-region-background)
)

(use-package highlight-indentation
  :ensure t
  :config
  (set-face-background 'highlight-indentation-current-column-face "gray22")
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
