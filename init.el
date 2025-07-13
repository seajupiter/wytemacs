;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-
;; Author: Yuetian Wu <yuetian-wu@outlook.com>
;; Created: 2025-07-04

;; Commentary:
;; This is the main configuration file for Emacs.
;; It bootstraps package management, load packages, and sets user preferences.

;;; Code:

;;;; Bootstrap elpaca package manager

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;;;; Sane defaults

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
(setq warning-minimum-level :error)
(setq inhibit-startup-screen t)         ; No startup screen
(setq ring-bell-function 'ignore)       ; No audible bell
(blink-cursor-mode 1)                   ; No blinking cursor
(column-number-mode 1)                  ; Show column number
(global-visual-line-mode 1)             ; Wrap lines visually
(tool-bar-mode -1)
(setq-default left-margin-width 0)
(setq-default right-margin-width 0)
(setq-default line-spacing 0.2)         ; Add some space between lines
(setq frame-inhibit-implied-resize t)
(setq frame-resize-pixelwise t)         ; fine-grained window resizing
(setopt initial-scratch-message ";; In the beginning there was darkness...\n\n")
(setq scroll-conservatively 101)
;; Only enable pixel-scroll in GUI frames
(when (display-graphic-p)
  (pixel-scroll-precision-mode 1))
(setq make-backup-files nil)             ; No backup files ~
(setq create-lockfiles nil)              ; No lockfiles #
(setq delete-by-moving-to-trash t)       ; Move deleted files to trash
(global-auto-revert-mode 1)             ; Revert buffers when files change on disk
(setq global-auto-revert-non-file-buffers t)
(setq confirm-nonexistent-file-or-buffer nil)
(setq vc-follow-symlinks t)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(fset 'yes-or-no-p 'y-or-n-p)            ; y/n instead of yes/no
(electric-pair-mode 1)                  ; Auto-pair delimiters
(recentf-mode 1)                        ; Keep track of recent files
(set-face-attribute 'default nil :family "JuliaMono" :height 150)
(set-fontset-font t 'han "Pingfang SC")
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; on macos, fix "This Emacs binary lacks sound support" 
;; - https://github.com/leoliu/play-sound-osx/blob/master/play-sound.el
;; - update according to https://github.com/leoliu/play-sound-osx/issues/2#issuecomment-1088360638
(when (eq system-type 'darwin)
  (unless (and (fboundp 'play-sound-internal)
               (subrp (symbol-function 'play-sound-internal)))
    (defun play-sound-internal (sound)
      "Internal function for `play-sound' (which see)."
      (or (eq (car-safe sound) 'sound)
          (signal 'wrong-type-argument (list sound)))
      
      (cl-destructuring-bind (&key file data volume device)
          (cdr sound)
        
        (and (or data device)
             (error "DATA and DEVICE arg not supported"))
        
        (apply #'start-process "afplay" nil
               "afplay" (append (and volume (list "-v" volume))
                                (list (expand-file-name file data-directory))))))))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\*"))

(use-package which-key
  :config
  (which-key-mode))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

;;;; Keymap 

(use-package general :ensure (:wait t) :demand t)

(defun my/scroll-half-page (direction)
  "Scroll half a page up or down depending on DIRECTION.
If DIRECTION is 'up, scroll up; if 'down, scroll down."
  (interactive
   (list (if current-prefix-arg 'up 'down))) ; with C-u, scroll up
  (let ((n (/ (window-body-height) 2)))
    (cond
     ((eq direction 'up) (scroll-down n))
     ((eq direction 'down) (scroll-up n)))))

;;;; Evil

(use-package evil
  :ensure t
  :custom
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-tree)
  (evil-want-C-u-scroll nil)
  (evil-cross-lines t)
  :config
  (defun my/evil-shift-left-visual ()
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (defun my/evil-shift-right-visual ()
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))

  (general-swap-key nil 'motion ";" ":")
  (general-def :states '(normal motion visual)
    "H" 'evil-first-non-blank
    "L" 'evil-end-of-line
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)
  (general-def :states 'normal
    "SPC o c" (lambda () (interactive) (find-file user-init-file))
    "SPC b d" 'kill-current-buffer
    "SPC b i" 'ibuffer
    "SPC x f" 'find-file
    "SPC x e" 'eval-last-sexp
    "SPC x p f" 'project-find-file
    "SPC x p g" 'project-find-regexp)
  (general-def :states 'visual
    ">" 'my/evil-shift-right-visual
    "<" 'my/evil-shift-left-visual) 

  (evil-mode 1))

(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode))

(use-package evil-escape
  :ensure t
  :config
  (setq-default evil-escape-key-sequence "jk")
  (defun my/evil-escape-inhibit ()
    (or (evil-visual-state-p)
        (evil-motion-state-p)
        (derived-mode-p 'magit-mode)
        (derived-mode-p 'ibuffer-mode)
        (derived-mode-p 'treemacs-mode)))
  (setq evil-escape-inhibit-functions '(my/evil-escape-inhibit))
  (evil-escape-mode))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-mc
  :ensure t
  :after evil
  :config
  (global-evil-mc-mode 1))

(use-package standard-themes
  :ensure t
  :custom
  (standard-themes-bold-constructs t)
  (standard-themes-italic-constructs t)
  (standard-themes-disable-other-themes t)
  (standard-themes-mixed-fonts t)
  (standard-themes-variable-pitch-ui t)
  (standard-themes-prompts '(extrabold italic))
  (standard-themes-to-toggle '(standard-dark standard-light-tinted))
  (standard-themes-to-rotate '(standard-light standard-light-tinted standard-dark standard-dark-tinted))
  ;; more complex alist to set weight, height, and optional
  ;; `variable-pitch' per heading level (t is for any level not
  ;; specified):
  (standard-themes-headings
   '((0 . (variable-pitch light 1.9))
     (1 . (variable-pitch light 1.8))
     (2 . (variable-pitch light 1.7))
     (3 . (variable-pitch semilight 1.6))
     (4 . (variable-pitch semilight 1.5))
     (5 . (variable-pitch 1.4))
     (6 . (variable-pitch 1.3))
     (7 . (variable-pitch 1.2))
     (agenda-date . (1.3))
     (agenda-structure . (variable-pitch light 1.8))
     (t . (variable-pitch 1.1))))
  :config
  (load-theme 'standard-dark t)
  (general-def :states 'normal
    "SPC t t" 'standard-themes-toggle))

;;;; Completion

(use-package savehist
  :init
  (savehist-mode 1))

(use-package vertico
  :ensure t
  :init
  (vertico-mode 1))

(use-package marginalia
  :ensure t
  :after vertico
  :config
  (marginalia-mode 1))

(use-package corfu
  :ensure t
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.05)
  (corfu-auto-prefix 2)
  :init
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1)
  (add-to-list 'savehist-minibuffer-history-variables 'corfu-history)
  :general-config
  (general-def :keymaps 'corfu-map
    "C-e" 'corfu-quit
    "C-l" 'corfu-info-location
    "C-h" 'corfu-info-documentation))

(use-package nerd-icons-corfu
  :ensure t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package cape
  :ensure t
  :bind ("M-<tab>" . cape-prefix-map)
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-ignore-case t))

;;;; Tex

(use-package auctex
  :ensure t
  :hook
  (TeX-mode . TeX-source-correlate-mode)
  (TeX-mode . prettify-symbols-mode)
  :custom
  (TeX-source-correlate-method 'synctex)
  (TeX-view-program-list   ;; Use Skim, it's awesome
   '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -g -b %n %o %b")))
  (TeX-view-program-selection '((output-pdf "Skim")))
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
  :general-config
  (general-def :keymaps 'LaTeX-mode-map
    "s-B" 'TeX-command-run-all)
  :config
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer))

(use-package flymake
  :ensure t
  :custom
  (flymake-show-diagnostics-at-end-of-line t)
  :general-config
  (general-def :keymaps 'flymake-mode-map
    "M-n" 'flymake-goto-next-error
    "M-p" 'flymake-goto-prev-error))

(use-package preview-auto
  :ensure t
  :hook (LaTeX-mode . preview-auto-setup))

(use-package cdlatex
  :ensure t
  :after (yasnippet)
  :hook (LaTeX-mode . turn-on-cdlatex)
  :general-config
  (general-def :keymaps 'cdlatex-mode-map
    "<tab>" 'cdlatex-tab)
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
  (general-def :keymaps 'yas-keymap
    "<tab>" 'yas-next-field-or-cdlatex
    "TAB" 'yas-next-field-or-cdlatex))

(use-package reftex
  :custom
  (reftex-plug-into-AUCTeX t)
  :hook (LaTeX-mode . turn-on-reftex))

(use-package evil-tex
  :ensure t
  :hook
  ((LaTeX-mode . evil-tex-mode)
   (org-mode . evil-tex-mode)))

;;;; Org

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
  (visual-line-mode))

(use-package org
  :ensure t
  :hook (org-mode . org-mode-setup)
  :custom
  (org-startup-truncated nil)
  (org-hide-emphasis-markers t)
  (org-highlight-latex-and-related '(latex script entities))
  (org-latex-create-formula-image-program 'dvisvgm)
  (org-image-actual-width nil)
  (org-clock-sound "~/Music/ding.wav")
  (org-clock-clocked-in-display 'frame-title)
  (org-pretty-entities t)
  :general-config
  (general-def "C-c a" 'org-agenda-list)
  (general-def :keymaps 'org-mode-map
    "s-b" (lambda () (interactive) (wrap-with-delimiters (region-beginning) (region-end) "*"))
    "s-i" (lambda () (interactive) (wrap-with-delimiters (region-beginning) (region-end) "/"))
    "s-_" (lambda () (interactive) (wrap-with-delimiters (region-beginning) (region-end) "_")))
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
  :ensure t
  :after org
  :general
  (general-def :keymaps 'org-mode-map
    "C-c p p" 'org-download-clipboard)
  :init
  (setq org-download-method 'directory))

(use-package org-modern
  :ensure t
  :after org
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))


;;;; Snippet
(use-package yasnippet
  :ensure t
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :init
  (setq yas-triggers-in-field t)
  (setq yas-snippet-dirs (list (concat user-emacs-directory "snippets")))
  :config
  (yas-global-mode 1))

(use-package aas
  :ensure t
  :hook ((LaTeX-mode . aas-activate-for-major-mode)
         (org-mode . aas-activate-for-major-mode))
  :config
  (aas-global-mode))

(use-package laas
  :ensure t
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


;;;; ProgLang

(use-package treesit)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l"))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package flycheck
  :ensure t)

;; elisp 
(use-package elisp-mode
  :hook
  (emacs-lisp-mode . (lambda ()
		       (outline-minor-mode)
		       (define-key outline-minor-mode-map (kbd "C-c C-n") 'outline-next-visible-heading)
		       (define-key outline-minor-mode-map (kbd "C-c C-p") 'outline-previous-visible-heading)
		       (define-key outline-minor-mode-map (kbd "C-c C-f") 'outline-forward-same-level)
		       (define-key outline-minor-mode-map (kbd "C-c C-b") 'outline-backward-same-level)
		       (define-key outline-minor-mode-map (kbd "C-c C-u") 'outline-up-heading)
		       (define-key outline-minor-mode-map (kbd "C-c C-t") 'outline-toggle-children))))

;; Markdown
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command "pandoc"))

;; lua
(use-package lua-ts-mode
  :hook
  (lua-ts-mode . lsp))

;; Nushell
(use-package nushell-mode :ensure t)

;; configuration files
(use-package conf-mode
  :mode ("\\.skhdrc\\'" . conf-mode))


;;;; Misc

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package writeroom-mode
  :ensure t
  :custom
  (writeroom-width 100)
  :init
  (defun wyt/toggle-zen () (interactive)
         (writeroom-mode 'toggle)))

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
  :after transient)

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (general-def :states 'normal
    "SPC v =" 'diff-hl-diff-goto-hunk
    "SPC v n" 'diff-hl-next-hunk
    "SPC v p" 'diff-hl-previous-hunk
    "SPC v s" 'diff-hl-show-hunk
    "SPC v S" 'diff-hl-stage-dwim))

(use-package consult
  :ensure t
  :general-config
  (general-def :states 'normal
    "SPC ." 'consult-fd
    "SPC f l" 'consult-line
    "SPC f w" 'consult-ripgrep
    "SPC /" 'consult-ripgrep
    "SPC f t" 'consult-theme
    "SPC ," 'consult-buffer
    "SPC f r" 'consult-recent-file))

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1))

(use-package copilot
  :ensure t)

(use-package beacon
  :ensure t
  :config
  (beacon-mode 1))

(use-package vterm
  :ensure t
  :custom
  (vterm-shell "/opt/homebrew/bin/fish")
  :general
  (general-def "C-`" 'vterm)
  (general-def :keymaps 'vterm-mode-map
    "C-`" 'switch-to-prev-buffer))

(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font"))

(use-package neotree
  :ensure t
  :hook
  (neo-after-create . (lambda (&rest _)
			(setq truncate-lines t)
			(setq word-wrap nil)))
  :init
  (defun my/neotree-find-project-root ()
    (interactive)
    (let ((buffer (current-buffer)))
      (neotree-find (projectile-project-root))
      (set-buffer buffer)))
  :general
  (general-def :states 'normal "SPC TAB" 'neotree-toggle)
  (general-def [f8] 'neotree-toggle))
:config
(setq neo-theme (if (display-graphic-p) 'nerd-icons 'arrow))
(setq neo-window-fixed-size nil)

(use-package avy
  :ensure t
  :general
  (general-def
    "C-;" 'avy-goto-char-2)
  (general-def :states '(normal visual)
    "SPC z" 'avy-goto-char-2
    "s" 'avy-goto-char-timer))

(use-package ledger-mode
  :ensure t
  :mode ("\\.journal\\'" . ledger-mode))

(use-package flycheck-ledger
  :ensure t
  :after (flycheck ledger-mode))

;;; init.el ends here

