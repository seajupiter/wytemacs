;; -*- lexical-binding: t; -*-

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-redo)
  (setq evil-want-C-u-scroll nil)
  (setq evil-cross-lines t)
  :config
  (evil-mode 1)
  (use-package evil-escape
    :straight t
    :config
    (setq-default evil-escape-key-sequence "jk")
    (setq evil-escape-inhibit-functions '(my/evil-escape-inhibit))
    (defun my/evil-escape-inhibit ()
      (or (evil-visual-state-p)
          (evil-motion-state-p)
          (derived-mode-p 'magit-mode)
          (derived-mode-p 'ibuffer-mode)
          (derived-mode-p 'treemacs-mode)))
    (evil-escape-mode)))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :straight t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-mc
  :straight t
  :after evil
  :config
  (global-evil-mc-mode 1))

(provide 'init-evil)