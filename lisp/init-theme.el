;; -*- lexical-binding: t; -*-

;; (use-package dracula-theme
;; 	:straight t)
;; (load-theme 'dracula t)

(add-to-list 'load-path "/Users/wyt/Repository/nano-emacs/")
(setq nano-font-family-monospaced "JuliaMono")
(setq nano-font-family-proportional "Fira Sans")
(setq nano-font-size 16)
(require 'nano-faces)
(require 'nano-theme)
(require 'nano-theme-dark)
(require 'nano-layout)
(require 'nano-defaults)
(require 'nano-modeline)
(require 'nano-bindings)
(require 'nano-help)
(nano-faces)
(nano-theme-set-dark)
(nano-refresh-theme)
(set-face-attribute 'nano-face-critical nil
                    :foreground "#FFFFFF"
                    :background "#0096FF")


(provide 'init-theme)
