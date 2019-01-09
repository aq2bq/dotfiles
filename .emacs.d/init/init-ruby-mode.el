;;Ruby
(use-package ruby-mode
  :commands ruby-mode
  :init
  (add-hook 'ruby-mode-hook 'ruby-electric-mode)
  :mode (("\\.rb$" . ruby-mode)
	 ("Gemfile$" . ruby-mode)
	 ("Capfile$" . ruby-mode)
	 ("Guardfile$" . ruby-mode)
	 ("[Rr]akefile$" . ruby-mode))
  :bind (("C-c c" . smart-compile))
  :interpreter "ruby"
  :config
  (setq
   smart-compile-alist
	(append
	 '(("\\.rb$" . "ruby %f"))
	 ))
  (setq ruby-insert-encoding-magic-comment nil)
)
