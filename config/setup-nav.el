;;; setup-nav.el --- Setup navigation-related packages

;;; Commentary:

;; * Vertico
;; Vertico provides a performant and minimalistic vertical completion UI
;; based on the default completion system.
;; https://github.com/minad/vertico
;;
;; * Consult
;; 

;;; Code:

(use-package vertico
  :ensure t
  :custom
  (vertico-count 30)
  (vertico-cycle t)
  :init
  (vertico-mode 1)
  (vertico-buffer-mode 1)
  :config
  (setq vertico-buffer-display-action '(display-buffer-in-side-window
					(side . bottom)
					(window-height . 0.3)))
  )

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :ensure t
  :after vertico
  :init (marginalia-mode))

(use-package consult
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-line :initial (thing-at-point 'symbol)
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep consult-man :initial (thing-at-point 'symbol)
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")
  :bind
  ("C-s" . consult-line)
  ("C-x b" . consult-buffer)
  ("M-y" . consult-yank-from-kill-ring)
  ("C-c g" . consult-ripgrep)
)

(use-package embark
  :ensure t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind
  (("C-;" . embark-act))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(provide 'setup-nav)
;; setup-nav.el ends here
