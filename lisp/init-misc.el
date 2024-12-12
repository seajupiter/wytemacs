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

(use-package ultra-scroll-mac
  :straight (ultra-scroll-mac :type git :host github :repo "jdtsmith/ultra-scroll-mac")
  :if (eq window-system 'mac)
  :init
  (setq scroll-conservatively 101 ; important!
        scroll-margin 0) 
  :config
  (ultra-scroll-mac-mode 1))

(provide 'init-misc)
