(defun wrap-with-delimiters (beg end delimiter)
  "Wrap the selected region with DELIMITER in Org mode, or remove it if already present."
  (interactive "r\nsEnter delimiter: ")
  (if (use-region-p)
      (let ((region-text (buffer-substring-no-properties beg end)))
        (if (and (string-prefix-p delimiter region-text)
                 (string-suffix-p delimiter region-text))
            ;; If text is already surrounded by the delimiter, remove it
            (progn
              (goto-char beg)
              (delete-char (length delimiter))  ; Delete the leading delimiter
              (goto-char (- end (length delimiter)))  ; Adjust for the length of the delimiter
              (delete-char (- (length delimiter))))  ; Delete the trailing delimiter
          ;; Otherwise, add the delimiter around the selected text
          (progn
            (goto-char end)
            (insert delimiter)
            (goto-char beg)
            (insert delimiter))))
    (message "No region selected")))

(defun org-mode-setup () (interactive)
    "start up hook for org-mode"
    (org-cdlatex-mode)
    (visual-line-mode)
    (variable-pitch-mode)
    (add-to-list 'evil-surround-pairs-alist '(?/ . ("/" . "/")))
    (add-to-list 'evil-surround-pairs-alist '(?* . ("*" . "*")))
    (add-to-list 'evil-surround-pairs-alist '(?_ . ("_" . "_")))
    (setq visual-fill-column-width 80))

(use-package org
  :straight t
  :hook
  (org-mode . org-mode-setup)
  :general
  (:keymaps 'org-mode-map
            "s-b" (lambda () (interactive)
                    (wrap-with-delimiters (region-beginning) (region-end) "*"))
            "s-i" (lambda () (interactive)
                    (wrap-with-delimiters (region-beginning) (region-end) "/"))
            "s-_" (lambda () (interactive)
                    (wrap-with-delimiters (region-beginning) (region-end) "_")))
  :init
  (setq org-startup-truncated nil)
  ;; (setq org-hide-emphasis-markers t)
  (setq org-highlight-latex-and-related '(latex script entities))
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.6))
  (setq org-latex-create-formula-image-program 'dvisvgm)

  (custom-theme-set-faces
   'user
   '(org-level-8 ((t (:family "Fira Sans"))))
   '(org-level-7 ((t (:family "Fira Sans"))))
   '(org-level-6 ((t (:family "Fira Sans"))))
   '(org-level-5 ((t (:family "Fira Sans"))))
   '(org-level-4 ((t (:family "Fira Sans" :height 1.3))))
   '(org-level-3 ((t (:family "Fira Sans" :height 1.4))))
   '(org-level-2 ((t (:family "Fira Sans" :height 1.5))))
   '(org-level-1 ((t (:family "Fira Sans" :height 1.6))))
   '(org-document-title ((t (:family "Fira Sans" :height 1.7 :underline nil))))
   '(variable-pitch ((t (:family "Fira Sans" :height 1.2)))))
  )

(use-package org-bullets
  :straight t
  :hook
  (org-mode . org-bullets-mode))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-directory (file-truename "~/org-roam"))
  :config
  (org-roam-db-autosync-mode))

(provide 'init-org)
