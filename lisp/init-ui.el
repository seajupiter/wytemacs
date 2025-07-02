;; -*- lexical-binding: t; -*-

(set-face-attribute 'default nil :family "JuliaMono" :height 160)

(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-nord t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(use-package grid
  :straight (:type git :host github :repo "ichernyshovvv/grid.el"))

(defface my/enlight-content-face
  '((t :family "Academy Engraved LET" :height 2.0))
  "Custom variable-pitch face for enlight splash screen.")

(use-package enlight
  :straight t
  :custom
  (enlight-content
   (propertize
    "We know the past,
but cannot control it.
We control the future,
but cannot know it.
-- Claude Elwood Shannon\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n"
    'face 'my/enlight-content-face
    )))

(setq-default left-margin-width 0)
(setq-default right-margin-width 0)

(provide 'init-ui)
