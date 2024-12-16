;; general.el keybindings

(use-package general :ensure t
  :straight (general :type git
                     :host github
                     :repo "noctuid/general.el")
  :config
  (general-swap-key nil 'motion
    ";" ":")

  (general-def '(normal insert)
    "s-/" (lambda () (interactive)
            (comment-line 1)
            (previous-line)))
  (general-def '(visual motion)
    "s-/" 'comment-dwim)

  (general-def '(normal motion visual)
    "H" 'evil-first-non-blank
    "L" 'evil-end-of-line
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)

  (general-create-definer my/leader-def
    :prefix "SPC")

  (my/leader-def 'normal
    "o c" 'my/edit-configuration

    "b d" 'evil-delete-buffer
    "b i" 'ibuffer

    "TAB" 'treemacs

    "x f" 'find-file
    "x e" 'eval-last-sexp
    "x p f" 'project-find-file
    "x p g" 'project-find-regexp

    "SPC" 'consult-fd
    "f f" 'consult-find
    "f w" 'consult-ripgrep
    "/" 'consult-ripgrep
    "f b" 'consult-buffer
    "<" 'consult-buffer
    "f r" 'recentf)

  (general-def 'insert
    "C-a" 'copilot-accept-completion))

(provide 'init-general)
