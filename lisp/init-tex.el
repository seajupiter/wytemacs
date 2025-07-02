;; -*- lexical-binding: t; -*-

(use-package pdf-tools
  :straight t
  :hook
  (pdf-view-mode . (lambda ()
                     (auto-revert-mode -1)))
  :custom
  (pdf-view-use-scaling t)
  :config
  (pdf-tools-install))

(use-package auctex
  :straight t
  :hook
  (TeX-mode . TeX-source-correlate-mode)
  (TeX-mode . prettify-symbols-mode)
  :custom
  (TeX-source-correlate-method 'synctex)
  (TeX-view-program-list '(("Sioyek"
                            ("/Applications/sioyek.app/Contents/MacOS/sioyek %o --forward-search-file \"%b\" --forward-search-line %n --inverse-search \"emacsclient +%2 %1\""))))
  (TeX-view-program-selection '((output-pdf "Sioyek")))
  (TeX-parse-self t)
  (TeX-save-query nil)
  (TeX-master 'dwim)
  (TeX-auto-save t)
  (TeX-PDF-mode t)
  (TeX-output-dir "build")
  (preview-scale-function 1.3)
  (preview-locating-previews-message nil)
  (preview-protect-point t)
  (preview-leave-open-previews-visible t)
  :config
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

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
