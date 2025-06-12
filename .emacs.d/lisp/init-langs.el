(leaf prog-mode
  :doc "Major mode for programming language"
  :hook ((prog-mode-hook . display-line-numbers-mode)
         (prog-mode-hook . electric-pair-mode)
         (prog-mode-hook . electric-indent-local-mode)
         (prog-mode-hook . electric-layout-mode)))

(leaf yaml-mode
  :ensure t
  :mode "\\(\.yml\\|\.yaml\\)")

(leaf fish-mode
  :doc "Emacs major mode for fish shell scripts."
  :ensure t
  :custom
  ((fish-indent-offset . 2)))

(leaf ruby-ts-mode
  :mode
  (("\\.rb$" . ruby-ts-mode)
	 ("Gemfile$" . ruby-ts-mode)
	 ("Steepfile$" . ruby-ts-mode)
	 ("Capfile$" . ruby-ts-mode)
	 ("Guardfile$" . ruby-ts-mode)
	 ("[Rr]akefile$" . ruby-ts-mode))
  :hook (electric-pair-mode rubocop-mode eldoc-mode ruby-electric-mode)
  :config
  ;; (leaf ruby-electric
  ;;   :ensure t)
  :custom
  (ruby-insert-encoding-magic-comment . nil))
(leaf rubocop
  :ensure t
  :bind (("C-c C-c f" . rubocop-autocorrect-current-file)))
(leaf rspec-mode
  :ensure t
  :bind (rspec-mode-map
         ("C-c t" . rspec-verify)))
(leaf yard-mode
  :ensure t
  :url "https://github.com/pd/yard-mode.el"
  :hook
  (ruby-ts-mode-hook . yard-mode))

(leaf typescript-ts-mode
  :mode
  (("\\.ts$" . typescript-ts-mode)
   ("\\.tsx$" . typescript-ts-mode))
  :hook (electric-pair-mode eldoc-mode)
  :custom
  ((typescript-indent-level . 2)))

(leaf go-mode
  :ensure t
  ;; :hook ((eglot-ensure))
  :custom
  ((gofmt-command . "goimports"))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (leaf gotest
    :doc "Run Go tests and programs from Emacs"
    :ensure t
    :bind (go-mode-map
           ("C-c C-r" . go-run)
           ("C-c t" . go-test-current-test)
           ("C-c C-t" . go-test-current-file))))

(leaf rust-mode
  :url "https://github.com/rust-lang/rust-mode"
  :ensure t
  :config
  (leaf cargo
    :ensure t
    :hook (rust-mode-hook . cargo-minor-mode))
  :custom ((rust-format-on-save . t)))


(leaf slim-mode
  :ensure t
  :doc "slim-mode provides Emacs support for editing Slim templates. It's based on haml-mode.")


(leaf kotlin-mode
  :ensure t)

(leaf graphql-mode :ensure t)


(leaf scss-mode
  :ensure t)

(leaf terraform-mode
  :ensure t
  :custom ((terraform-format-on-save . t))
  :config
  (defun my-terraform-mode-setup ()
    (when (eq major-mode 'terraform-mode)
      (setq-local lsp-semantic-tokens-enable t)
      (setq-local lsp-semantic-tokens-honor-refresh-requests t))))

;; Python
(leaf python-mode
  :ensure t
  :doc "Python major mode"
  :url "https://gitlab.com/groups/python-mode-devs"
  :mode "\\.py\\'"
  :custom `((py-keep-windows-configuration . t)
            (python-indent-guess-indent-offset . t)
            (python-indent-guess-indent-offset-verbose . nil)
            (py-python-command . ,(if (executable-find "rye run python") "rye run python"
                                    "python"))))
