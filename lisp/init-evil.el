;; evil mode
(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-C-u-scroll t)
  :custom
  (evil-cross-lines t)
  :config (evil-mode t))

(use-package evil-collection
  :after (evil)
  :straight t
  :custom
  (evil-collection-setup-minibuffer t)
  (evil-collection-outline-bind-tab-p t)
  :config
  (evil-collection-init))

(use-package evil-escape
  :straight t
  :init
  (setq-default evil-escape-key-sequence "jk")
  (setq evil-escape-excluded-states '(visual motion))
  (setq evil-escape-excluded-major-modes '(ibuffer-mode))
  :config
  (evil-escape-mode))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))
  

(provide 'init-evil)
