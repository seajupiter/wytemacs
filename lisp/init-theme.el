;; -*- lexical-binding: t; -*-

;; (use-package dracula-theme
;; 	:straight t)
;; (load-theme 'dracula t)

;; (add-to-list 'load-path "/Users/wyt/Repository/nano-emacs/")
;; (setq nano-font-family-monospaced "JuliaMono")
;; (setq nano-font-family-proportional "Fira Sans")
;; (setq nano-font-size 16)
;; (require 'nano-faces)
;; (require 'nano-theme)
;; (require 'nano-theme-light)
;; (require 'nano-theme-dark)
;; (require 'nano-layout)
;; (require 'nano-defaults)
;; (require 'nano-modeline)
;; (require 'nano-bindings)
;; (require 'nano-help)
;; (nano-faces)
;; (nano-theme-set-dark)
;; (nano-refresh-theme)
;; (set-face-attribute 'nano-face-critical nil
;;                     :foreground "#FFFFFF"
;;                     :background "#0096FF")

;; (load-theme 'modus-vivendi t)
(set-face-attribute 'default nil
                    :family "JuliaMono" :height 160)

(use-package doom-themes
  :straight t
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; for treemacs users
  ;; (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  (load-theme 'doom-nord t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(setq-default left-margin-width 2)
(setq-default right-margin-width 2)

(provide 'init-theme)
