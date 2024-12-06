;; (use-package gruvbox-theme :ensure t
;;   :config
;;   (load-theme 'gruvbox-dark-medium t))

;; (use-package solo-jazz-theme
;;   :straight t
;;   :config
;;   (load-theme 'solo-jazz t))

(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t))

(custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "ETBembo" :height 200)))))

(provide 'init-theme)
