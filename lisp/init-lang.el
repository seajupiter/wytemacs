;; -*- lexical-binding: t; -*-

(use-package treesit)

(use-package lsp-mode
  :straight t
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l"))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package flycheck
  :straight t)

;; lisp
(load (expand-file-name "~/.quicklisp/slime-helper.el") 'noerror 'nomessage)
(setq inferior-lisp-program "sbcl")

;; Markdown
(use-package markdown-mode
  :straight t
  :mode ("\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command "pandoc")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; lua
(use-package lua-ts-mode)

(provide 'init-lang)
