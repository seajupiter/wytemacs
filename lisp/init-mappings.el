;; -*- lexical-binding: t; -*-

(use-package general
  :straight (general :type git :host github :repo "noctuid/general.el")
  :config
  (general-create-definer my/leader-def
    :prefix "SPC")

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
    "k" 'evil-previous-visual-line
    "s" 'avy-goto-char-timer)

  (my/leader-def 'normal
    "o c" 'my/edit-configuration

    "b d" 'evil-delete-buffer
    "b i" 'ibuffer

    "TAB" 'neotree-toggle

    "x f" 'find-file
    "x e" 'eval-last-sexp
    "x p f" 'project-find-file
    "x p g" 'project-find-regexp

    "SPC" 'consult-fd
    "f f" 'consult-find
    "f w" 'consult-ripgrep
    "/" 'consult-ripgrep
    "f b" 'consult-buffer
    "," 'consult-buffer
    "f t" 'consult-theme
    "f r" 'recentf

    "w" 'avy-goto-word-1)

  (general-def 'insert
    "C-a" 'copilot-accept-completion))

(provide 'init-mappings)
