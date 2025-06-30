;; -*- lexical-binding: t; -*-

(use-package auctex
  :straight t
  :defer t
  :config
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-view-program-list '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -g -b %n %o %b")))
  (setq TeX-view-program-selection '((output-pdf "Skim")))
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)
  (setq TeX-master 'dwim)
  (setq TeX-auto-save t)
  (setq TeX-PDF-mode t)
  (setq-default TeX-output-dir "build"))

(use-package auctex-cont-latexmk
  :straight t
  :after auctex
  :bind
  (:map LaTeX-mode-map
        ("C-c k" . auctex-cont-latexmk-toggle)))

(use-package flymake
  :straight t
  :custom
  (flymake-show-diagnostics-at-end-of-line t)
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)))

(use-package preview-auto
  :straight t
  :hook (LaTeX-mode . preview-auto-setup))

(use-package cdlatex
  :straight t
  :hook (LaTeX-mode . turn-on-cdlatex)
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab)))

(use-package evil-tex
  :straight t
  :hook ((LaTeX-mode . evil-tex-mode)
         (org-mode . evil-tex-mode)))

(provide 'init-tex)