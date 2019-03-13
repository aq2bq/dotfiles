(use-package highlight-indent-guides
  :init
  (setq highlight-indent-guides-auto-enabled t)
  (setq highlight-indent-guides-method 'column)
  (setq highlight-indent-guides-responsive 'top)
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))
