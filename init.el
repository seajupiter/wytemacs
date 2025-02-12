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
      indent-tabs-mode  t
      tab-always-indent t)

(straight-use-package 'use-package)
(straight-use-package 'org)

;; Font
(add-to-list 'default-frame-alist
             '(font . "UbuntuMono Nerd Font-18:regular"))

(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

(require 'init-sane-defaults)
(require 'init-misc)
(require 'init-theme)
(require 'init-evil)
(require 'init-completion)
(require 'init-latex)
(require 'init-org)
(require 'init-snippet)
(require 'init-mappings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("4ade6b630ba8cbab10703b27fd05bb43aaf8a3e5ba8c2dc1ea4a2de5f8d45882"
     "2771ec93656faf267521dce9ffe1a6ad88cd0bea87aa0e8c4fc80bf355c58c1d"
     "df6dfd55673f40364b1970440f0b0cb8ba7149282cf415b81aaad2d98b0f0290"
     "2078837f21ac3b0cc84167306fa1058e3199bbd12b6d5b56e3777a4125ff6851"
     "ff24d14f5f7d355f47d53fd016565ed128bf3af30eb7ce8cae307ee4fe7f3fd0"
     "e8bd9bbf6506afca133125b0be48b1f033b1c8647c628652ab7a2fe065c10ef0"
     "2b501400e19b1dd09d8b3708cefcb5227fda580754051a24e8abf3aff0601f87"
     "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14"
     "2e7dc2838b7941ab9cabaa3b6793286e5134f583c04bde2fba2f4e20f2617cf7"
     "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
     "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710"
     "1930427eae3d4d830a43fd79fbda76021138b929c243a4e8606cf4f0531ea17c"
     "f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7"
     "b1a691bb67bd8bd85b76998caf2386c9a7b2ac98a116534071364ed6489b695d"
     default)))
(put 'TeX-narrow-to-group 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)
