;; -*- lexical-binding: t; -*-

(defun my/edit-configuration ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(defun my/setup-new-frame (frame)
  "Custom function to set up a newly created FRAME"
  (with-selected-frame frame
    (set-frame-height (selected-frame) 50)
    (set-frame-width (selected-frame) 100)))

(add-hook 'after-make-frame-functions #'my/setup-new-frame)

;; straight package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq-default tab-width 4
      indent-tabs-mode  nil
      tab-always-indent t)

(straight-use-package 'use-package)


(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

(progn
  (require 'init-misc)
  (require 'init-completion)
  (require 'init-tex)
  (require 'init-org)
  (require 'init-snippet)
  (require 'init-lang)
  (require 'init-sane-defaults)
  (require 'init-ui))
