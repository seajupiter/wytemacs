;;; init.el --- Emacs configuration entry point -*- lexical-binding: t; -*-
;; Author: Yuetian Wu <ytwu@posteo.net>
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

;;;; Helper functions

(defun is-nw-p ()
  (or (and (not (daemonp))
	   (not (display-graphic-p)))
      (and (daemonp)
	   (string= (daemonp) "nw"))))

(defun is-gui-p ()
  (or (and (not (daemonp))
	   (display-graphic-p))
      (and (daemonp)
	   (string= (daemonp) "gui"))))

;;;; Sane defaults

(modify-all-frames-parameters '((vertical-scroll-bars)))
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
(add-hook 'minibuffer-setup-hook (lambda () (electric-pair-local-mode -1)))
(recentf-mode 1)                        ; Keep track of recent files
(set-face-attribute 'default nil :family "IBM Plex Mono" :height 150)
(set-face-attribute 'variable-pitch nil :family "IBM Plex Sans" :height 150)
(set-fontset-font t 'han "Pingfang SC")
(setq display-line-numbers-type 'relative)
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

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
  :custom
  (undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree" user-emacs-directory))))
  :config
  (global-undo-tree-mode))

;; Configure outline-minor-mode for navigation
(use-package outline
  :ensure nil  ; built-in package
  :hook ((emacs-lisp-mode . outline-minor-mode)
         (lisp-interaction-mode . outline-minor-mode))
  :config
  ;; Set up outline heading regexp for Emacs Lisp comments
  (setq-default outline-regexp ";;;\\(;+\\| \\)")
  ;; Show all headings by default
  (add-hook 'outline-minor-mode-hook 'outline-show-all))

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
  (evil-set-leader nil (kbd ",") t)
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
    "C-f" (lambda () (interactive) (evil-scroll-down 0))
    "C-b" (lambda () (interactive) (evil-scroll-up 0))
    "SPC o c" (lambda () (interactive) (find-file user-init-file))
    "SPC b d" 'kill-current-buffer
    "SPC b i" 'ibuffer
    "SPC x f" 'find-file
    "SPC x e" 'eval-last-sexp
    "SPC x p f" 'project-find-file
    "SPC x p g" 'project-find-regexp
    "SPC t n" 'display-line-numbers-mode)
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
  (setq evil-escape-inhibit-functions '((lambda () (interactive)
					  (or (evil-visual-state-p)
					      (evil-motion-state-p)
					      (derived-mode-p 'magit-mode)
					      (derived-mode-p 'ibuffer-mode)
					      (derived-mode-p 'treemacs-mode)))))
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

;;;; Themes

(use-package everforest
  :ensure (:type git :repo "https://github.com/seajupiter/everforest-emacs.git")
  ;; :load-path "~/Repository/everforest-emacs/"
  :config
  (load-theme 'everforest-hard-dark t))

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
  (corfu-auto-delay 0.02)
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
  (TeX-electric-escape t)
  (TeX-source-correlate-method 'synctex)
  (TeX-view-program-list   ;; Use Skim, it's awesome
   '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -g -b %n %o %b")
     ("eaf" eaf-pdf-synctex-forward-view)))
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
  :config
  (defvar latex-symbol-history nil
    "History of previously used TeX macros and LaTeX math symbols.")
  
  ;; Add to savehist for persistence across sessions
  (add-to-list 'savehist-additional-variables 'latex-symbol-history)
  
  (defun my/smart-insert-latex-macro ()
    "Insert a TeX macro or LaTeX math symbol interactively.
For TeX macros, only mandatory arguments are prompted.
For LaTeX math symbols, insert them directly."
    (interactive)
    (let* ((tex-symbols (mapcar (lambda (sym)
                                  (cons (concat "macro: " (car sym)) (car sym)))
                                (TeX-symbol-list)))
           (math-symbols (delq nil
                               (mapcar (lambda (item)
                                         (when (and (listp item) (stringp (cadr item)))
                                           (cons (concat "math: " (cadr item)) item)))
                                       (append LaTeX-math-list LaTeX-math-default))))
           (all-symbols (append tex-symbols math-symbols))
           ;; Put history items first, then remaining items
           (history-items (delq nil (mapcar (lambda (h) 
                                              (assoc h all-symbols))
                                            latex-symbol-history)))
           (remaining-items (seq-filter (lambda (item) 
                                          (not (member (car item) latex-symbol-history)))
                                        all-symbols))
           (all-symbols-with-history (append history-items remaining-items))
           (completion-extra-properties
            (list :annotation-function
                  (lambda (s)
                    (cond
                     ((string-prefix-p "macro: " s) " [TeX macro]")
                     ((member s latex-symbol-history) " [Recent]")
                     (t " [Math symbol]")))))
           (choice (completing-read "TeX macro or math symbol: " 
                                    all-symbols-with-history
                                    nil t nil 'latex-symbol-history))
           (entry (cdr (assoc choice all-symbols-with-history))))

      (when choice
        ;; Add to history (remove if exists, then add to front)
        (setq latex-symbol-history 
              (cons choice (delete choice latex-symbol-history)))
        ;; Keep history reasonable size
        (when (> (length latex-symbol-history) 20)
          (setq latex-symbol-history (butlast latex-symbol-history)))
        
        (cond
         ;; Handle TeX macro
         ((string-prefix-p "macro: " choice)
          (let ((current-prefix-arg '(4))) ; Set prefix arg (C-u) to insert only mandatory args
            (TeX-insert-macro entry)))

         ;; Handle LaTeX math symbol
         (t
          ;; entry is a list with format (key type symbol ...)
          (let ((symbol (nth 1 entry)))
            (cond ((stringp symbol)
                   (insert (format "\\%s" symbol)))
                  (t (message "Unknown symbol format: %S" symbol))))))))
  (general-def :keymaps 'LaTeX-mode-map
    "s-b" 'TeX-command-run-all)
  (general-def :states 'normal :keymaps 'LaTeX-mode-map
    ", l l" 'TeX-command-run-all
    ", l v" 'TeX-view)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)))

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
  :config
  (general-def :keymaps 'cdlatex-mode-map
    "<tab>" 'cdlatex-tab
    "`" nil)
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
   (org-mode . evil-tex-mode))
  :config
  (general-unbind :keymaps 'evil-tex-mode-map "M-n"))

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

  (add-to-list 'org-startup-options '("prettify"   my/org-startup-prettified t)))


(use-package org-download
  :ensure t
  :after org
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
  (org-download-timestamp "%Y%m%d-%H%M%S")
  (org-download-screenshot-method "pngpaste %s")
  :general
  (general-def :keymaps 'org-mode-map
    "C-c p p" 'org-download-clipboard))

(use-package org-modern
  :ensure t
  :after org
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))


;;;; Reference managing
(use-package ebib
  :ensure t
  :custom
  (ebib-index-columns '(("Entry Key" 12 t) ("Title" 50 t) ("Year" 6 t) ("Author/Editor" 20 nil)))
  (ebib-layout 'full)
  (ebib-index-window-size 45)
  (ebib-file-associations '(("pdf" . "open") ("ps" . "open")))
  (ebib-citation-commands '((LaTeX-mode
			     (("cite" "\\cite%<[%A]%>[%A]{%(%K%,)}")
			      ("paren" "\\parencite%<[%A]%>[%A]{%(%K%,)}")
			      ("foot" "\\footcite%<[%A]%>[%A]{%(%K%,)}")
			      ("text" "\\textcite%<[%A]%>[%A]{%(%K%,)}")
			      ("smart" "\\smartcite%<[%A]%>[%A]{%(%K%,)}")
			      ("super" "\\supercite{%K}")
			      ("auto" "\\autocite%<[%A]%>[%A]{%(%K%,)}")
			      ("cites" "\\cites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
			      ("parens" "\\parencites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
			      ("foots" "\\footcites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
			      ("texts" "\\textcites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
			      ("smarts" "\\smartcites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
			      ("supers" "\\supercites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
			      ("autos" "\\autoscites%<(%A)%>(%A)%(%<[%A]%>[%A]{%K}%)")
			      ("author" "\\citeauthor%<[%A]%>[%A]{%(%K%,)}")
			      ("title" "\\citetitle%<[%A]%>[%A]{%(%K%,)}")
			      ("year" "\\citeyear%<[%A]%>[%A][%A]{%K}")
			      ("date" "\\citedate%<[%A]%>[%A]{%(%K%,)}")
			      ("full" "\\fullcite%<[%A]%>[%A]{%(%K%,)}")))
			    (org-mode (("ebib" "[[ebib:%K][%D]]")))
			    (markdown-mode
			     (("text" "@%K%< [%A]%>") ("paren" "[%(%<%A %>@%K%<, %A%>%; )]")
			      ("year" "[-@%K%< %A%>]")))))
  :config
  (general-def :keymaps 'LaTeX-mode-map
    "C-c b" 'ebib-insert-citation))


;;;; Snippet
(use-package yasnippet
  :ensure t
  :demand t
  :init
  (setq yas-triggers-in-field t)
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

(use-package evil-textobj-tree-sitter
  :ensure t
  :config
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "parameter.outer"))
  (define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "parameter.inner"))
  (define-key evil-normal-state-map (kbd "]f")
	      (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer")))
  (define-key evil-normal-state-map (kbd "[f")
	      (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
  (define-key evil-normal-state-map (kbd "]F")
              (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
  (define-key evil-normal-state-map (kbd "[F")
              (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)))
  (define-key evil-normal-state-map (kbd "]a")
	      (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "parameter.outer")))
  (define-key evil-normal-state-map (kbd "[a")
	      (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "parameter.outer" t)))
  (define-key evil-normal-state-map (kbd "]A")
              (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "parameter.outer" nil t)))
  (define-key evil-normal-state-map (kbd "[A")
              (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "parameter.outer" t t))))

(use-package flycheck
  :ensure t)

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode)
  :demand t
  :init
  (setq markdown-command "pandoc"))

;; lua
(use-package lua-ts-mode
  :mode ("\\.lua\\'" . lua-ts-mode))

;; rust
(use-package rust-ts-mode
  :mode ("\\.rs\\'" . rust-ts-mode))

;; haskell
(use-package haskell-ts-mode
  :ensure t
  :mode ("\\.hs\\'" . haskell-ts-mode)
  :custom
  (haskell-ts-font-lock-level 4)
  (haskell-ts-use-indent t)
  (haskell-ts-ghci "ghci")
  (haskell-ts-use-indent t)
  :config
  (add-to-list 'treesit-language-source-alist
	       '(haskell . ("https://github.com/tree-sitter/tree-sitter-haskell" "v0.23.1")))
  (unless (treesit-grammar-location 'haskell)
    (treesit-install-language-grammar 'haskell)))

;; configuration files
(use-package conf-mode
  :mode ("\\.skhdrc\\'" . conf-mode))

;; lsp
(use-package eglot
  :hook
  (lua-ts-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (haskell-ts-mode . eglot-ensure))

(use-package eldoc-box
  :ensure t)

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
  :hook
  (magit-post-refresh . diff-hl-magit-post-refresh)
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
    "SPC f r" 'consult-recent-file
    "SPC f i" 'consult-imenu
    "SPC f o" 'consult-outline))

(use-package projectile
  :ensure t
  :config
  (projectile-mode 1))

(use-package copilot
  :ensure t
  :config
  (general-def :keymaps 'copilot-completion-map
    "TAB" 'copilot-accept-completion))

(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'claude-3.7-sonnet
	gptel-backend (gptel-make-gh-copilot "Copilot")))

(use-package eshell
  :general
  (general-def "C-`" 'eshell)
  (general-def :keymaps 'eshell-mode-map
    "C-`" 'switch-to-prev-buffer))

(use-package eat
  :ensure t
  :hook
  (eshell-load . eat-eshell-mode))

(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font"))

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

(use-package elfeed
  :ensure t)

(use-package gt
  :ensure (:type git :repo "https://github.com/lorniu/gt.el.git")
  :custom
  (gt-langs '(en zh))
  (gt-default-translator (gt-translator :engines (gt-google-engine))))

;;;; custom
(setq custom-file (concat user-emacs-directory "custom.el"))
(load-file custom-file)


;;; init.el ends here
