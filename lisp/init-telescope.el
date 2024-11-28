(use-package consult :ensure t)

(use-package projectile :ensure t
  :config (projectile-mode 1))

(use-package fzf
  :straight (fzf :type git :host github :repo "bling/fzf.el")
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        ;; command used for `fzf-grep-*` functions
        ;; example usage for ripgrep:
        fzf/grep-command "rg"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t))

(provide 'init-telescope)
