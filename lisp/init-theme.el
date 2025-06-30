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

(setq-default left-margin-width 2)
(setq-default right-margin-width 2)

(provide 'init-theme)