;; hiding titlebar
(straight-use-package '(ns-auto-titlebar :host github :repo "purcell/ns-auto-titlebar"))

(use-package ns-auto-titlebar
  :config (when (eq system-type 'darwin) (ns-auto-titlebar-mode)))

(provide 'init-misc)
