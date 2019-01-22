(use-package ruby-mode
  :init
  (add-hook 'ruby-mode-hook 'eglot-ensure)
  :mode (("\\.rb$" . ruby-mode)
	 ("Gemfile$" . ruby-mode)
	 ("Capfile$" . ruby-mode)
	 ("Guardfile$" . ruby-mode)
	 ("[Rr]akefile$" . ruby-mode))
  :bind (:map ruby-mode-map
              ("M-s M-s" . spec-jump))
  :config
  (setq ruby-insert-encoding-magic-comment nil)
)
