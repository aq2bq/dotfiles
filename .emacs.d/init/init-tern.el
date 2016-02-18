(use-package tern
  :init
  (add-hook 'js2-mode-hook
	    '(lambda ()
	       (when (locate-library "tern")
		 (setq tern-command '("tern" "--no-port-file"))
		 (tern-mode t))))
  :bind
  ("C-c t" . tern-get-type) ;; Rebind from "C-c C-c"
  :config
  (eval-after-load 'tern
    '(progn
       (require 'tern-auto-complete)
       (tern-ac-setup)
       (unbind-key "C-c C-c" tern-mode-keymap)))
  )

