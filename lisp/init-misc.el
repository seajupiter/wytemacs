;; -*- lexical-binding: t; -*-

;; hiding titlebar
;; (use-package ns-auto-titlebar
;;   :straight t
;;   :config (when (eq system-type 'darwin) (ns-auto-titlebar-mode)))

;; get shell's PATH envvar
(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize))

;; Zen writing 
(use-package visual-fill-column
  :straight t
  :init
  (setq-default visual-fill-column-center-text t)
  (setq visual-fill-column-width 120))

(use-package writeroom-mode
  :straight t
  :init
  (setq writeroom-width 100))

;; git integration
(use-package transient
	:straight t)

(use-package magit
  :straight t)

;; Telescope
(use-package consult :straight t)

(use-package projectile :straight t
  :config (projectile-mode 1))

;; AI
(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))

;; Help me find cursor position
(use-package beacon
  :straight t
  :config
  (beacon-mode 1))

;; General keybindings definer
(use-package general
  :straight (general :type git
                     :host github
                     :repo "noctuid/general.el")
  :init
  (general-create-definer my/leader-def
    :prefix "SPC"))

;; faster terminal
(use-package vterm
  :straight t
  :bind
  (("C-`" . vterm)
   :map vterm-mode-map
   ("C-`" . switch-to-prev-buffer))
  :custom
  (vterm-shell "/opt/homebrew/bin/fish"))

;; nerd icons
(use-package nerd-icons
  :straight t
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font")
  )

;; neotree
(use-package neotree
  :straight t
  :init
  (defun my/neotree-find-project-root () (interactive)
         (let ((buffer (current-buffer))) 
           (neotree-find (projectile-project-root))
           (set-buffer buffer)))
  :bind
  (([f8] . neotree-toggle))
  :config
  (setq neo-theme (if (display-graphic-p) 'nerd-icons 'arrow))
  (setq neo-window-fixed-size nil))

;; easy motion
(use-package avy
  :straight t
  :bind
  (("C-;" . avy-goto-char)
   ("C-'" . avy-goto-char-2)))

;; ledger -- PTA tool
(use-package ledger-mode
	:straight t
	:mode ("\\.journal\\'" "\\.dat\\'" "\\.ledger\\'")
	:hook (ledger-mode . flycheck-mode)
	:bind
	(("C-c C-x" . ledger-post-fill)))

(use-package flycheck-ledger
	:straight t
	:after (flycheck ledger-mode)
	:demand t)

(provide 'init-misc)
