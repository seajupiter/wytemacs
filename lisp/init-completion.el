;; -*- lexical-binding: t; -*-

(use-package savehist
  :straight nil ; built-in
  :init
  (savehist-mode 1))

(use-package vertico
  :straight t
  :init
  (vertico-mode 1))

(use-package vertico-posframe
  :straight t
  :after posframe
  :custom
  (vertico-multiform-commands
   '((consult-line (:not posframe))
     (consult-ripgrep (:not posframe))
     (t posframe)))
  :config
  (vertico-posframe-mode 1))

(use-package marginalia
  :straight t
  :after vertico
  :config
  (marginalia-mode 1))

(use-package corfu
  :straight t
  :hook (prog-mode . corfu-mode)
  :bind (:map corfu-map
              ("C-e" . corfu-quit)
              ("C-l" . corfu-info-location)
              ("C-h" . corfu-info-documentation))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.05)
  (corfu-auto-prefix 2)
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1)
  (add-to-list 'savehist-minibuffer-history-variables 'corfu-history)
  :config
  (use-package nerd-icons-corfu
    :straight t
    :config
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

(use-package cape
  :straight t
  :bind ("M-<tab>" . cape-prefix-map)
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-ignore-case t))

(provide 'init-completion)
