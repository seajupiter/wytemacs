;; -*- lexical-binding: t; -*-

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(setq warning-minimum-level :error)

;;; Basic UI Customization
(setq inhibit-startup-screen t)         ; No startup screen
(setq ring-bell-function 'ignore)       ; No audible bell
(setq pop-up-windows nil)               ; No popup windows
(blink-cursor-mode 1)                   ; No blinking cursor
(column-number-mode 1)                  ; Show column number
(global-visual-line-mode 1)             ; Wrap lines visually
(setq-default line-spacing 0.2)         ; Add some space between lines
(setq frame-resize-pixelwise t)         ; fine-grained window resizing

;;; Scrolling
(setq scroll-conservatively 101)
(pixel-scroll-precision-mode 1)

;;; File Handling
(setq make-backup-files nil)             ; No backup files ~
(setq create-lockfiles nil)              ; No lockfiles #
(setq delete-by-moving-to-trash t)       ; Move deleted files to trash
(global-auto-revert-mode 1)             ; Revert buffers when files change on disk
(setq global-auto-revert-non-file-buffers t)
(setq confirm-nonexistent-file-or-buffer nil)
(setq vc-follow-symlinks t)

;;; Encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

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

;;; Other Defaults
(fset 'yes-or-no-p 'y-or-n-p)            ; y/n instead of yes/no
(electric-pair-mode 1)                  ; Auto-pair delimiters
(recentf-mode 1)                        ; Keep track of recent files

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\*"))

(use-package which-key
  :config
  (which-key-mode))

;; Font for Chinese
(set-fontset-font t 'han "Pingfang SC")

(provide 'init-sane-defaults)
