;; ------------- nano-emacs ----------------------

(straight-use-package
 '(nano-emacs :type git :host github :repo "rougier/nano-emacs"))

(require 'nano-layout)
;; Theming Command line options (this will cancel warning messages)
(add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
(add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
(add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
(add-to-list 'command-switch-alist '("-compact" . (lambda (args))))


;; Customize support for 'emacs -q' (Optional)
;; You can enable customizations by creating the nano-custom.el file
;; with e.g. `touch nano-custom.el` in the folder containing this file.
(let* ((this-file  (or load-file-name (buffer-file-name)))
       (this-dir  (file-name-directory this-file))
       (custom-path  (concat this-dir "nano-custom.el")))
  (when (and (eq nil user-init-file)
             (eq nil custom-file)
             (file-exists-p custom-path))
    (setq user-init-file this-file)
    (setq custom-file custom-path)
    (load custom-file)))

;; Theme
(require 'nano-faces)
(require 'nano-theme)
(require 'nano-theme-dark)
(require 'nano-theme-light)

(nano-theme-set-light)
(call-interactively 'nano-refresh-theme)

;; Nano session saving (optional)
(require 'nano-session)

;; Nano header & mode lines (optional)
;; (require 'nano-modeline)

;; Help (optional)
(unless (member "-no-help" command-line-args)
  (require 'nano-help))

;; -------------nano-emacs end-----------------

(provide 'init-nano)
