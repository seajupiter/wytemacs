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

    "b d" 'evil-delete-buffer
    "b i" 'ibuffer

    "x f" 'find-file
    "x e" 'eval-last-sexp
    "x p f" 'project-find-file
    "x p g" 'project-find-regexp

    "SPC" 'fzf-git-files
    "f f" 'fzf-find-file
    "f w" 'fzf-grep-with-narrowing
    "f b" 'fzf-switch-buffer))

(provide 'init-general)
