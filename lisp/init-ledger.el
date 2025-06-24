;; -*- lexical-binding: t; -*-

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

(provide 'init-ledger)
