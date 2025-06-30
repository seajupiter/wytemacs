;; -*- lexical-binding: t; -*-

(defun wrap-with-delimiters (beg end delimiter)
  "Wrap the selected region with DELIMITER in Org mode, or remove it if already present."
  (interactive "r\nsEnter delimiter: ")
  (if (use-region-p)
      (let ((region-text (buffer-substring-no-properties beg end)))
        (if (and (string-prefix-p delimiter region-text)
                 (string-suffix-p delimiter region-text))
            (progn
              (goto-char beg)
              (delete-char (length delimiter))
              (goto-char (- end (length delimiter)))
              (delete-char (- (length delimiter))))
          (progn
            (goto-char end)
            (insert delimiter)
            (goto-char beg)
            (insert delimiter))))
    (message "No region selected")))

(defun org-mode-setup ()
  (interactive)
  (org-cdlatex-mode)
  (org-indent-mode)
  (visual-line-mode)
  (add-to-list 'evil-surround-pairs-alist '(?/ . ("/" . "/")))
  (add-to-list 'evil-surround-pairs-alist '(?* . ("*" . "*")))
  (add-to-list 'evil-surround-pairs-alist '(?_ . ("_" . "_"))))

(use-package org
  :straight t
  :hook (org-mode . org-mode-setup)
  :bind
  (("C-c a" . 'org-agenda-list)
   :map org-mode-map
   ("s-b" . (lambda () (interactive) (wrap-with-delimiters (region-beginning) (region-end) "*")))
   ("s-i" . (lambda () (interactive) (wrap-with-delimiters (region-beginning) (region-end) "/")))
   ("s-_" . (lambda () (interactive) (wrap-with-delimiters (region-beginning) (region-end) "_"))))
  :custom
  (org-startup-truncated nil)
  (org-hide-emphasis-markers t)
  (org-highlight-latex-and-related '(latex script entities))
  (org-latex-create-formula-image-program 'dvisvgm)
  (org-image-actual-width nil)
  (org-clock-sound "~/Music/ding.wav")
  (org-clock-clocked-in-display 'frame-title)
  (org-pretty-entities t)
  :config
  (plist-put org-format-latex-options :background "Transparent")
  (plist-put org-format-latex-options :foreground "White")
  (plist-put org-format-latex-options :scale 2.0)

  (defvar my/org-prettify-symbols nil
    "Alist for `prettify-symbols-mode'.")

  (defun my/org-prettify--set-prettify-symbols-alist ()
    (dolist (entity (append org-entities-user org-entities))
      (when (listp entity)            ; `org-entities' has strings too
        (when-let* ((match-for (car entity))
                    (replace-with (car (last entity)))
                    (_ (length= replace-with 1)))
          (add-to-list 'my/org-prettify-symbols
                       (cons (concat "\\" match-for) replace-with))))))

  (my/org-prettify--set-prettify-symbols-alist)

  (defun my/org-prettify--predicate (_start end _match)
    ;; There's no need the check the character before the entity match
    ;; since all of them start with \. The characters that are
    ;; acceptable after the match are mathematical operators and some
    ;; special characters.
    (seq-contains-p '(?\C-j ?} ?{ ?\\ ?_ ?- ?+ ?^ ?\( ?\) ?$ ?  ?/ ?| ?. ?, ?\;)
                    (char-after end)))

  (define-minor-mode my/org-prettify-mode
    "Use `prettify-symbols-mode' to prettify org entities."
    :lighter " ℘"
    (if my/org-prettify-mode
        (progn
          (setq-local prettify-symbols-alist my/org-prettify-symbols
                      prettify-symbols-unprettify-at-point 'right-edge
                      prettify-symbols-compose-predicate #'my/org-prettify--predicate)
          (prettify-symbols-mode 1))
      (setq-local prettify-symbols-alist nil
                  prettify-symbols-unprettify-at-point nil
                  prettify-symbols-compose-predicate #'prettify-symbols-default-compose-p)
      (prettify-symbols-mode -1)))

  (defvar my/org-startup-prettified nil
    "Prettify org entites when non-nil.")

  (add-to-list 'org-startup-options '("prettify"   my/org-startup-prettified t))

                                        ; utilities to import org-clock data from mobile phone
  (defun my/org-import-mobile-time-log ()
    "Parse mobile time log and add entries to a target Org file."
    (interactive)
    (let* ((target-org-file (expand-file-name "~/org/mobile.org")) ; Or your main agenda file
           (log-content (current-kill 0))
           (lines (when log-content (split-string log-content "\n" t))))
      (if (not lines)
          (message "No available information in system clipboard")
        (with-current-buffer (find-file-noselect target-org-file)
          (dolist (line lines)
            (when (string-match "\\[\\(.*\\)\\]--\\[\\(.*\\)\\] | \\[\\(.*\\)\\]" line)
              (let* ((start-time-str (match-string 1 line))
                     (end-time-str (match-string 2 line))
                     (task-name (match-string 3 line))
                     (org-start-time (format-time-string "[%Y-%m-%d %a %H:%M]" (org-time-string-to-seconds start-time-str)))
                     (org-end-time (format-time-string "[%Y-%m-%d %a %H:%M]" (org-time-string-to-seconds end-time-str))))
                (message "heading: %s" task-name)
                ;; Find or create the heading
                (goto-char (point-min))
                (let ((heading-found (search-forward (concat "* " task-name) nil t)))
                  (unless heading-found
                    (goto-char (point-max))
                    (insert (format "\n* %s\n:LOGBOOK:\n:END:" task-name))))
                ;; For now, we'll just put all clocks under the top-level heading
                ;; A more complex version could create sub-headings.
                (goto-char (point-min))
                (search-forward (concat "* " task-name))
                (search-forward ":END:")
                (previous-line 1)
                (end-of-line)
                (insert (format "\nCLOCK: %s--%s" org-start-time org-end-time))
                (message (format "Logged: %s" (match-string 3 line))))))
          (save-buffer)))
      (message "Mobile log import complete.")))
  )


(use-package org-download
  :straight t
  :after org
  :bind
  (:map org-mode-map
        ("C-c p p" . org-download-clipboard))
  :init
  (setq org-download-method 'directory))

(use-package evil-org
  :straight t
  :after org
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-modern
  :straight t
  :after org
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

(provide 'init-org)
