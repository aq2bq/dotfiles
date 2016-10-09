(use-package slime
  :init
  (add-hook 'lisp-mode-hook
	    '(lambda ()
	       (slime-mode t)))
  :mode (("\\.ros$" . lisp-mode)
	 ("\\.lisp$" . lisp-mode))
  :bind (("C-c c" . smart-compile)
	 ("C-c j" . slime-close-all-parens-in-sexp))
  :commands slime
  :config
  (progn
    (add-hook
     'slime-load-hook
     #'(lambda ()
    	 (slime-setup
    	  '(slime-fancy
    	    slime-repl
    	    slime-banner
    	    slime-highlight-edits
    	    slime-fuzzy))))
    (setq slime-net-coding-system 'utf-8-unix)
    (setq
     smart-compile-alist
     (append
      '(("\\.lisp$" . "ros -l %f"))
      ))
    (use-package ac-slime
      :init
      (add-hook 'slime-mode-hook 'set-up-slime-ac)
      (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
      (eval-after-load "auto-complete"
	'(add-to-list 'ac-modes 'slime-repl-mode))
      )
    ))
