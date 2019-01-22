(use-package ruby-mode
  :mode (("\\.rb$" . ruby-mode)
	 ("Gemfile$" . ruby-mode)
	 ("Capfile$" . ruby-mode)
	 ("Guardfile$" . ruby-mode)
	 ("[Rr]akefile$" . ruby-mode))
  :bind (("M-s M-s" . spec-jump))
  :interpreter "ruby"
  :config
  (setq ruby-insert-encoding-magic-comment nil)
)
