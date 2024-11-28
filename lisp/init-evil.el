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

(defun my-evil-escape-inhibit-condition () (interactive)
  (eq evil-state 'visual))

(use-package evil-escape :ensure t
  :init
  (setq-default evil-escape-key-sequence "jk")
  (setq evil-escape-inhibit-functions
        '(my-evil-escape-inhibit-condition))
  :config
  (evil-escape-mode))
  

(provide 'init-evil)
