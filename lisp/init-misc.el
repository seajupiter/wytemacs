;; hiding titlebar
(use-package ns-auto-titlebar
  :straight t
  :config (when (eq system-type 'darwin) (ns-auto-titlebar-mode)))

(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize))

(use-package visual-fill-column
  :straight t
  :init
  (setq visual-fill-column-width 100)
  (global-visual-fill-column-mode 1))

(use-package writeroom-mode
  :straight t
  :init
  (setq writeroom-width 100))

(use-package neotree
  :straight t
  :init
  (setq neo-autorefresh t))

(provide 'init-misc)
