;; -*- lexical-binding: t; -*-

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
    (org-indent-mode)
    (visual-line-mode)
    ;; (variable-pitch-mode)
    (add-to-list 'evil-surround-pairs-alist '(?/ . ("/" . "/")))
    (add-to-list 'evil-surround-pairs-alist '(?* . ("*" . "*")))
    (add-to-list 'evil-surround-pairs-alist '(?_ . ("_" . "_")))
    (setq visual-fill-column-width 80))

(use-package org
  :straight t
  :hook
  (org-mode . org-mode-setup)
  :bind
  (("C-c a" . 'org-agenda-list)
   :map org-mode-map
        ("s-b" . (lambda () (interactive)
                  (wrap-with-delimiters (region-beginning) (region-end) "*")))
        ("s-i" . (lambda () (interactive)
                  (wrap-with-delimiters (region-beginning) (region-end) "/")))
        ("s-_" . (lambda () (interactive)
                  (wrap-with-delimiters (region-beginning) (region-end) "_"))))
  :init
  (setq org-startup-truncated nil)
  (setq org-hide-emphasis-markers t)
  (setq org-highlight-latex-and-related '(latex script entities))
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (setq org-latex-create-formula-image-program 'dvisvgm)
  (setq org-image-actual-width nil)

	:config
	(plist-put org-format-latex-options :background "Transparent")
  (plist-put org-format-latex-options :foreground "White")
  (plist-put org-format-latex-options :scale 2.0)

  )

(use-package org-bullets
  :straight t
  :hook
  (org-mode . org-bullets-mode))

(use-package org-roam
  :straight t
  :init
  (setq org-roam-directory (file-truename "~/org"))
  (setq org-roam-db-location (file-truename "~/org/org-roam.db"))
  (setq org-roam-dailies-directory "journal/")
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
           :unnarrowed t)
          ("b" "book notes" plain (file "~/RoamNotes/Templates/BookNoteTemplate.org")
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ))
  (setq org-roam-node-display-template
      (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (defun my/org-roam-capture-inbox ()
    (interactive)
    (org-roam-capture- :node (org-roam-node-create)
                       :templates '(("i" "inbox" plain "* %?"
                                     :target (file+head "tasks/Inbox.org" "#+title: Inbox\n")))))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert)
   ("C-c n b" . my/org-roam-capture-inbox))
  :config
  (require 'org-roam-export)
  (org-roam-db-autosync-enable))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package org-appear
  :straight t
  :hook (org-mode . org-appear-mode)
  :init
  (setq org-appear-autolinks t))

(use-package org-fragtog
  :straight t
  :hook (org-mode . org-fragtog-mode))

(use-package org-download
  :straight t
  :general
  ("C-c p p" 'org-download-clipboard)
  :init
  (setq org-download-method 'directory))

(use-package evil-org
  :straight t
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(provide 'init-org)
