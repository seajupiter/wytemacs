(defun my/edit-configuration ()
  "Open the init file."
  (interactive)
  (find-file user-init-file))

(defun my/setup-new-frame (frame)
  "Custom function to set up a newly created FRAME"
  (with-selected-frame frame
    (set-frame-height (selected-frame) 45)
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

;; Font
(add-to-list 'default-frame-alist
             '(font . "RobotoMono Nerd Font-14"))

(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

(require 'init-theme)
(require 'init-tweak)
(require 'init-evil)
(require 'init-misc)
(require 'init-completion-at-point)
(require 'init-completion-minibuffer)
(require 'init-telescope)
(require 'init-latex)
(require 'init-cite)
(require 'init-org)
(require 'init-snippet)
(require 'init-general)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2e7dc2838b7941ab9cabaa3b6793286e5134f583c04bde2fba2f4e20f2617cf7" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710" "1930427eae3d4d830a43fd79fbda76021138b929c243a4e8606cf4f0531ea17c" "f366d4bc6d14dcac2963d45df51956b2409a15b770ec2f6d730e73ce0ca5c8a7" "b1a691bb67bd8bd85b76998caf2386c9a7b2ac98a116534071364ed6489b695d" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-document-title ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "ETBembo" :height 2.0 :underline nil))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "ETBembo" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "ETBembo" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "ETBembo" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "ETBembo" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "ETBembo"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "ETBembo"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "ETBembo"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "ETBembo"))))
 '(variable-pitch ((t (:family "ETBembo" :height 200)))))
(put 'TeX-narrow-to-group 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)
