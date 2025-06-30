;; -*- lexical-binding: t; -*-

(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize))

;; (use-package visual-fill-column
;;   :straight t
;;   :hook (visual-line-mode . visual-fill-column-mode)
;;   :custom
;;   (visual-fill-column-width 120)
;;   (visual-fill-column-center-text t))

(use-package writeroom-mode
  :straight t
  :custom
  (writeroom-width 100)
  :init
  (defun wyt/toggle-zen () (interactive)
         (writeroom-mode 'toggle)))

(use-package magit
  :straight t
  :after transient)

(use-package consult
  :straight t)

(use-package projectile
  :straight t
  :config
  (projectile-mode 1))

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))

(use-package beacon
  :straight t
  :config
  (beacon-mode 1))

(use-package vterm
  :straight t
  :bind
  (("C-`" . vterm)
   :map vterm-mode-map
   ("C-`" . switch-to-prev-buffer))
  :custom
  (vterm-shell "/opt/homebrew/bin/fish"))

(use-package nerd-icons
  :straight t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font"))

(use-package neotree
  :straight t
  :hook
  (neo-after-create . (lambda (&rest _)
                        (setq truncate-lines t)
                        (setq word-wrap nil)))
  :init
  (defun my/neotree-find-project-root ()
    (interactive)
    (let ((buffer (current-buffer)))
      (neotree-find (projectile-project-root))
      (set-buffer buffer)))
  :bind
  (([f8] . neotree-toggle))
  :config
  (setq neo-theme (if (display-graphic-p) 'nerd-icons 'arrow))
  (setq neo-window-fixed-size nil))

(use-package avy
  :straight t
  :bind
  (("C-;" . avy-goto-char)
   ("C-'" . avy-goto-char-2)))

(use-package ledger-mode
  :straight t
  :mode ("\\.journal\\'" . ledger-mode)
  :bind
  (:map ledger-mode-map
        ("C-c C-x" . ledger-post-fill))
  :config
  (use-package flycheck-ledger
    :straight t
    :after (flycheck ledger-mode)
    :demand t))

(use-package posframe
  :straight t)

(provide 'init-misc)
