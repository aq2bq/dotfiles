(use-package slime
  :init
  (add-hook 'lisp-mode-hook
	    '(lambda ()
	       (slime-mode t)))
  :mode (("\\.ros$" . lisp-mode)
	 ("\\.lisp$" . lisp-mode))
  :bind (("C-c c" . smart-compile))
  :commands slime
  :config
  (progn
    (add-hook
     'slime-load-hook
     #'(lambda ()
	 (slime-setup
	  '(slime-fancy
	    slime-company
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
    )
  )
