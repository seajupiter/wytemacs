;; general.el keybindings

(use-package general
  :straight (general :type git
                     :host github
                     :repo "noctuid/general.el")
  :init
  (general-create-definer my/leader-def
    :prefix "SPC"))

(provide 'init-general)
