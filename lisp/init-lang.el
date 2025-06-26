;; -*- lexical-binding: t; -*-

;; lsp-mode
(use-package lsp-mode
	:straight t
	:init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  ;; :hook ((TeX-mode . lsp)
	;; 			 (LaTeX-mode . lsp)
  ;;        ;; if you want which-key integration
  ;;        (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui
	:straight t
	:commands lsp-ui-mode)

;; fly-check
(use-package flycheck
	:straight t)

;; lisp
(load (expand-file-name "~/.quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")

;; Markdown
(use-package markdown-mode
  :straight t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

(provide 'init-lang)
