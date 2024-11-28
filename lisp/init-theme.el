;; (use-package gruvbox-theme :ensure t
;;   :config
;;   (load-theme 'gruvbox-dark-medium t))

(use-package solo-jazz-theme
  :straight '(solo-jazz-theme
              :type git
              :host github
              :repo "cstby/solo-jazz-emacs-theme")
  :config
  (load-theme 'solo-jazz t))

(provide 'init-theme)
