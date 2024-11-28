(defun my-edit-configuration ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(defun my-setup-new-frame (frame)
  "Custom function to set up a newly created FRAME"
  (with-selected-frame frame
    (set-frame-height (selected-frame) 45)
    (set-frame-width (selected-frame) 100)))

(add-hook 'after-make-frame-functions #'my-setup-new-frame)

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
      indent-tabs-mode  t
      tab-always-indent t)

(straight-use-package 'use-package)

;; Font
(add-to-list 'default-frame-alist
             '(font . "RobotoMono Nerd Font-14"))

(add-to-list 'load-path "/Users/wyt/.emacs.d/lisp/")

;; (require 'init-nano)
(require 'init-theme)
(require 'init-tweak)
(require 'init-evil)
(require 'init-misc)
(require 'init-completion-at-point)
(require 'init-completion-minibuffer)
(require 'init-telescope)

(require 'init-general)
