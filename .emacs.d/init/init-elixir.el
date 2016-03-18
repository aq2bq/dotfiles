(use-package elixir-mode
  :mode (("\\.ex$" . elixir-mode)
	 ("\\.exs$" . elixir-mode))
  :bind (("C-c c" . smart-compile))
  :interpreter "elixir"
  :config
  (setq
   smart-compile-alist
	(append
	 '(("\\.exs$" . "elixir %f"))
	 ))
  (add-hook 'elixir-mode-hook 'ac-alchemist-setup)
  )
