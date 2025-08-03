;; -*- lexical-binding: t; -*-
(setenv "LIBRARY_PATH" "/opt/homebrew/opt/gcc/lib/gcc/14:/opt/homebrew/opt/libgccjit/lib/gcc/14:/opt/homebrew/opt/gcc/lib/gcc/14/gcc/aarch64-apple-darwin24/14")

;; Set user-emacs-directory early to affect all cache locations
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))

;; Configure native compilation cache directory
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-eln-load-path
        (list (expand-file-name "eln-cache/" user-emacs-directory)))
  (setq native-comp-async-report-warnings-errors nil))

(setq package-enable-at-startup nil)
(scroll-bar-mode -1)

