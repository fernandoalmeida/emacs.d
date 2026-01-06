;;; setup-nav.el --- Setup navigation-related packages

;;; Commentary:

;; * Projectile
;; Projectile is a project interaction library for Emacs.
;; It provides a powerful set of features operating at the project level,
;; as well as simple heuristics to identify projects.
;; https://github.com/bbatsov/projectile/
;; 
;; * Vertico
;; Vertico provides a performant and minimalistic vertical completion UI
;; based on the default completion system.
;; https://github.com/minad/vertico
;;
;; * Consult
;; 

;;; Code:

(use-package consult
  :ensure t

  :hook
  (completion-list-mode . consult-preview-at-point-mode)

  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5)
  (setq xref-show-xrefs-function #'consult-xref xref-show-definitions-function #'consult-xref)

  :config
  (defun custom/consult-line ()
    "Run `consult-line` using the active region as initial input."
    (interactive)
    (let ((initial (when (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end)))))
      (consult-line initial)))
  (defun custom/consult-ripgrep ()
    "Grep selected region as initial input."
    (interactive)
    (let ((dir (or (consult--project-root) default-directory))
	  (initial
	   (when (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end)))))
      (consult--grep "Ripgrep" #'consult--ripgrep-make-builder dir initial)))
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-git-grep consult-grep consult-man
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<")

  :bind
  ("C-s" . custom/consult-line)
  ("C-c g" . custom/consult-ripgrep)
  ("C-x b" . consult-buffer)
  ("M-y" . consult-yank-from-kill-ring)
  )

(use-package embark
  :ensure t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind
  (("C-;" . embark-act)
   ("M-." . embark-dwim)
   ("C-," . embark-previous-symbol)
   ("C-." . embark-next-symbol))
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :ensure t
  :after vertico
  :init (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package savehist
  :init
  (savehist-mode))

(use-package vertico
  :ensure t
  :custom
  (vertico-count 30)
  (vertico-cycle t)
  :init
  (vertico-mode 1)
  (vertico-buffer-mode 1)
  :config
  (setq vertico-buffer-display-action
	'(display-buffer-in-side-window
	  (side . bottom)
	  (window-height . 0.3)))
  )

(provide 'setup-nav)
;; setup-nav.el ends here
