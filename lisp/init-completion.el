;; -*- lexical-binding: t; -*-

(use-package savehist
  :init
  (savehist-mode))

;; Minibuffer Completion
(use-package vertico
  :straight t
  :init
  (vertico-mode 1))

(use-package posframe
	:straight t)

(use-package vertico-posframe
	:straight t
	:init
	(vertico-posframe-mode 1))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :straight t
  ;; The :init section is always executed.
  :init
  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

;; Completion at point
(use-package corfu
  :straight t
	:bind (:map corfu-map
							("C-e" . corfu-quit)
							("C-l" . corfu-info-location)
							("C-h" . corfu-info-documentation))
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
	(corfu-auto-delay 0.05)					 
	(corfu-auto-prefix 2)
  :init
  (global-corfu-mode 1)
	(corfu-popupinfo-mode 1)
	(corfu-history-mode 1)
	(add-to-list 'savehist-minibuffer-history-variables 'corfu-history))

(use-package nerd-icons-corfu
	:straight t
	:config
	(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
	:straight t
	:bind ("M-<tab>" . cape-prefix-map)
	:init
	(setq-default completion-at-point-functions
								(append (default-value 'completion-at-point-functions)
												(list #'cape-dabbrev #'cape-file #'cape-abbrev))))

(use-package orderless
  :straight t
  :custom
  (completion-styles '(orderless basic))
	(completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
	(completion-ignore-case t))

(provide 'init-completion)
