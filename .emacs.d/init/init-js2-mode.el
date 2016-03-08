(use-package js2-mode
  :mode (("\\.js$" . js2-mode)
	 ("Jakefile$" . js2-mode)
	 ("\\.jsx" . js2-jsx-mode))
  :bind (("C-c c" . smart-compile))
  :interpreter "node"
  :config
  (setq
   smart-compile-alist
   (append
    '(("\\.js$" . "node %f"))
    ))
  (progn
    (add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
    (add-hook 'js2-mode-hook (lambda ()
			       (bind-key "M-j" 'join-line-or-lines-in-region js2-mode-map))))
  )
;; (use-package js2-refactor
;;   :init
;;   (add-hook 'js2-mode-hook 'js2-refactor-mode)
;;   :config
;;   (js2r-add-keybindings-with-prefix "C-c d")
;;   )
