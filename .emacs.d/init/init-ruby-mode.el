(use-package ruby-mode
  :init
  ;; Solargraph + Rails
  ;; https://github.com/castwide/solargraph/issues/87
  ;; https://speakerdeck.com/blue0513/emacs-de-lsp-woshi-tutemitayo?slide=20
  ;; 1. Make sure you have gem documentation installed by running yard gems.
  ;; 2. Add a .solargraph.yml file to the app's root directory (you can generate a default one by running solargraph config) and make the following change to the require section:
  ;;
  ;; $RAILS_ROOT/.solargraph.yml
  ;; require:
  ;; - actioncable
  ;; - actionmailer
  ;; - actionpack
  ;; - actionview
  ;; - activejob
  ;; - activemodel
  ;; - activerecord
  ;; - activestorage
  ;; - activesupport
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
