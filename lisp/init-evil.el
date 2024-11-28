;; evil mode
(straight-use-package '(evil :host github :repo "emacs-evil/evil"))

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :custom
  (evil-cross-lines t)
  :config (evil-mode t))

(use-package evil-collection
  :after (evil)
  :straight (evil-collection :type git
                             :host github
                             :repo "emacs-evil/evil-collection")
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-outline-bind-tab-p t)
  :config
  (evil-collection-init))

(use-package evil-escape :ensure t
  :init
  (setq-default evil-escape-key-sequence "jk")
  (setq evil-escape-excluded-states '(visual motion))
  (setq evil-escape-excluded-major-modes '(ibuffer-mode))
  :config
  (evil-escape-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))
  

(provide 'init-evil)
