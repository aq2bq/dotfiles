(use-package elixir-mode
  :mode (("\\.ex$" . elixir-mode)
	 ("\\.exs$" . elixir-mode))
  :interpreter "elixir"
  :config
  (add-hook 'elixir-mode-hook 'ac-alchemist-setup)
  )
