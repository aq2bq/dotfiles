(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
	 ("Jakefile$" . js2-mode)
	 ("\\.jsx" . js2-jsx-mode))
  :interpreter "node"
  :config
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
    (add-hook 'js2-mode-hook (lambda ()
			       (bind-key "M-j" 'join-line-or-lines-in-region js2-mode-map))))
  )
