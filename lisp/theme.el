;; -*- lexical-binding: t; -*-

(when (eq window-system 'x)
  (setq x-underline-at-descent-line t))

(setq solarized-distinct-fringe-background t)

(setq solarized-use-variable-pitch nil)

(setq solarized-height-minus-1 1)
(setq solarized-height-plus-1 1)
(setq solarized-height-plus-2 1)
(setq solarized-height-plus-3 1)
(setq solarized-height-plus-4 1)

(setq solarized-scale-org-headlines nil)

(load-theme 'solarized-dark t)

(custom-theme-set-faces
 'solarized-dark
 '(show-paren-match
   ((t (:foreground "#073642" :background "#00736f" :weight bold))) t))

(provide 'theme)
