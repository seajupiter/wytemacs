;; -*- lexical-binding: t; -*-

(use-package yasnippet
  :straight t
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :init
  (setq yas-triggers-in-field t)
  (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
  :config
  (yas-global-mode 1))

(use-package cdlatex
  :straight t
  :hook (LaTeX-mode . cdlatex-mode)
  :config
  (defun cdlatex-in-yas-field ()
    (when-let* ((_ (overlayp yas--active-field-overlay))
                (end (overlay-end yas--active-field-overlay)))
      (if (>= (point) end)
          (let ((s (thing-at-point 'sexp)))
            (unless (and s (assoc (substring-no-properties s)
                                  cdlatex-command-alist-comb))
              (yas-next-field-or-maybe-expand)
              t))
        (let (cdlatex-tab-hook minp)
          (setq minp
                (min (save-excursion (cdlatex-tab)
                                     (point))
                     (overlay-end yas--active-field-overlay)))
          (goto-char minp) t))))

  (defun yas-next-field-or-cdlatex ()
    (interactive)
    (if (or (bound-and-true-p cdlatex-mode)
            (bound-and-true-p org-cdlatex-mode))
        (cdlatex-tab)
      (yas-next-field-or-maybe-expand)))

  (define-key yas-keymap (kbd "<tab>") #'yas-next-field-or-cdlatex)
  (define-key yas-keymap (kbd "TAB") #'yas-next-field-or-cdlatex))

(use-package aas
  :straight t
  :hook ((LaTeX-mode . aas-activate-for-major-mode)
         (org-mode . aas-activate-for-major-mode))
  :config
  (aas-global-mode))

(use-package laas
  :straight t
  :hook ((LaTeX-mode . laas-mode)
         (org-mode . laas-mode))
  :config
  (aas-set-snippets 'laas-mode
    "mk" (lambda () (interactive) (yas-expand-snippet "\\$$1\\$$0"))
    "dm" (lambda () (interactive) (yas-expand-snippet "\\[\n  $1\n\\]\n$0"))
    :cond #'texmathp
    "supp" "\\supp"
    "On" "O(n)"
    "O1" "O(1)"
    "Olog" "O(\\log n)"
    "Olon" "O(n \\log n)"
    "max" nil
    "Sum" (lambda () (interactive) (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
    "Span" (lambda () (interactive) (yas-expand-snippet "\\Span($1)$0"))
    "sq" (lambda () (interactive) (yas-expand-snippet "\\sqrt{$1}$0"))
    "norm" (lambda() (interactive) (yas-expand-snippet "\\left\\lVert $1 \\right\\rVert"))
    "ZZ" (lambda () (interactive) (yas-expand-snippet "\\mathbb{Z}$0"))
    "ooo" (lambda () (interactive) (yas-expand-snippet "\\infty"))
    "xx" (lambda () (interactive) (yas-expand-snippet "\\times"))
    :cond #'laas-object-on-left-condition
    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))
    ",." (lambda () (interactive) (laas-wrap-previous-object "boldsymbol"))))

(provide 'init-snippet)
