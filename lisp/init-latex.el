(use-package auctex
  :straight t)

(use-package latex
  :ensure auctex
  :hook ((LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . my/latex-buffer-setup)
         (LaTeX-mode . turn-on-reftex))
  :init
  (setq TeX-source-correlate-method 'synctex
        TeX-view-program-list   ;; Use Skim, it's awesome
        '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -g -b %n %o %b"))
        TeX-view-program-selection '((output-pdf "Skim"))
        ;; TeX-view-program-list
        ;; '(("Sioyek" "/Applications/sioyek.app/Contents/MacOS/sioyek %o --forward-search-file %b --forward-search-line %n --inverse-search \"emacsclient --no-wait +%2:%3 %1\""))
        ;; TeX-view-program-selection '((output-pdf "Sioyek"))
        TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-master 'dwim
        TeX-PDF-mode nil
        preview-pdf-color-adjust-method t
        preview-image-type 'dvipng
        preview-scale-function 1.0)
  :config
  (defun my/latex-buffer-setup ()
    (TeX-source-correlate-mode)
    (TeX-PDF-mode)))

;; CDLatex settings
(use-package cdlatex
  :straight t
  :hook (LaTeX-mode . turn-on-cdlatex)
  :bind (:map cdlatex-mode-map 
              ("<tab>" . cdlatex-tab)))

;; evil-tex
(use-package evil-tex
  :straight t
  :hook (LaTeX-mode . evil-tex-mode)
  :hook (org-mode . evil-tex-mode))

;; xenops
(use-package xenops
  :straight t
  :init
  (setq xenops-math-image-scale-factor 2.0))

(provide 'init-latex)
