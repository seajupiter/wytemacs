;;; general.el keybindings

(use-package general :ensure t
  :straight (general :type git
                     :host github
                     :repo "noctuid/general.el")
  :config
  (general-swap-key nil 'motion
    ";" ":")
  (general-def '(normal motion)
    "H" 'evil-first-non-blank
    "L" 'evil-end-of-line
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)

  (general-create-definer my-leader-def
    :prefix "SPC")

  (my-leader-def 'normal
    "o c" 'my-edit-configuration

    "b d" 'evil-delete-buffer))

(provide 'init-general)
