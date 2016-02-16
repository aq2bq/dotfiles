;; Enhanced Ruby Mode
(use-package enh-ruby-mode
  :commands ruby-mode
  :mode (("\\.rb$" . ruby-mode)
	 ("Gemfile$" . ruby-mode)
	 ("Capfile$" . ruby-mode)
	 ("Guardfile$" . ruby-mode)
	 ("[Rr]akefile$" . ruby-mode))
  :bind (("C-c c" . smart-compile))
  :interpreter "ruby"
  :config
  (setq ruby-insert-encoding-magic-comment nil)
)
