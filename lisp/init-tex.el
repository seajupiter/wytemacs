;; -*- lexical-binding: t; -*-

(use-package auctex
  :straight t)

(use-package latex
  :ensure auctex
  :hook ((LaTeX-mode . prettify-symbols-mode)
         (LaTeX-mode . my/latex-buffer-setup)
         (LaTeX-mode . turn-on-reftex)
		 (LaTeX-mode . (lambda () (setq TeX-command-default "LaTeXMk"))))
		 ;; (LaTeX-mode . (lambda () (consult-theme 'modus-operandi))))
  :init
  (setq TeX-source-correlate-method 'synctex
		TeX-view-program-list   ;; Use Skim, it's awesome
		'(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -g -b %n %o %b"))
		TeX-view-program-selection '((output-pdf "Skim"))
		TeX-parse-self t
		TeX-save-query nil
		TeX-master 'dwim
		TeX-auto-save t
		TeX-PDF-mode nil
		preview-image-type 'dvipng
		preview-pdf-color-adjust-method t
		preview-scale-function 1.0
		preview-locating-previews-message nil
		preview-protect-point t
		preview-leave-open-previews-visible t)
	(setq-default TeX-output-dir "build")
  :config
  (defun my/latex-buffer-setup ()
    (TeX-source-correlate-mode)
    (TeX-PDF-mode)))

(use-package auctex-cont-latexmk
  :straight t
  :after latex
  :bind
  (:map LaTeX-mode-map
        ("C-c k" . auctex-cont-latexmk-toggle)))

(use-package flymake
  :custom
  (flymake-show-diagnostics-at-end-of-line t)
  :bind
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error)))

(use-package preview-auto
  :straight t
  :hook (LaTeX-mode . preview-auto-setup))

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
;; (use-package xenops
;;   :straight t
;;   :init
;;   (setq xenops-math-image-scale-factor 2.0))

(provide 'init-tex)
