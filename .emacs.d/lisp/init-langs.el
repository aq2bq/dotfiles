;;; init-langs.el --- Language settings -*- lexical-binding: t; -*-

(leaf prog-mode
  :doc "Major mode for programming language"
  :hook ((prog-mode-hook . display-line-numbers-mode)
         (prog-mode-hook . electric-pair-mode)
         (prog-mode-hook . electric-indent-local-mode)
         (prog-mode-hook . electric-layout-mode)))

(leaf dockerfile-ts-mode
  :tag "builtin"
  :mode
  ("Dockerfile\\'" . dockerfile-ts-mode))

(leaf markdown-modern
  :ensure t
  :url "https://github.com/rjprins/markdown-modern"
  :vc (:url "https://github.com/rjprins/markdown-modern"))

(leaf markdown-mode
  :ensure t
  :doc "Markdown Modeと久々に向き合う"
  :url "https://qiita.com/tadsan/items/7bb0099479f647d2c106"
  :mode ("\\.md\\'" . gfm-mode)
  :bind ((markdown-mode-map
          ("C-c C-c c" . my/copy-buffer-relative-path)))
  :config
  (setopt markdown-command '("pandoc" "--from=markdown" "--to=html5"))
  (setopt markdown-fontify-code-blocks-natively t)
  (setopt markdown-header-scaling t)
  (setopt markdown-indent-on-enter 'indent-and-new-item))

;; `npm install -g @mermaid-js/mermaid-cli` to use
(leaf markdown-mermaid
  :ensure t
  :url "https://github.com/pasunboneleve/markdown-mermaid"
  :bind (markdown-mode-map
         ("C-c m" . markdown-mermaid-preview))
  :custom ((markdown-mermaid-mmdc-path . "~/.nodenv/shims/mmdc")))

(leaf yaml-ts-mode
  :tag "builtin"
  :mode "\\.ya?ml\\'")

(leaf json-ts-mode
  :tag "builtin"
  :mode "\\.jsonc?\\'")

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
  :hook
  ((ruby-ts-mode-hook . rubocop-mode))
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
   ("\\.tsx$" . tsx-ts-mode))
  :custom
  ((typescript-ts-indent-offset . 2)))

(leaf go-mode
  :ensure t
  ;; :hook ((eglot-ensure))
  :custom
  ((gofmt-command . "goimports"))
  :hook
  (go-mode-hook
   . (lambda ()
       (add-hook 'before-save-hook #'gofmt-before-save nil t)))
  :config
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

;; Python: Emacs標準のモードをtreesit-enabled-modesで切り替える。
(leaf python
  :tag "builtin"
  :custom ((python-indent-guess-indent-offset . t)
           (python-indent-guess-indent-offset-verbose . nil))
  :config
  (when (executable-find "uv")
    (setq python-shell-interpreter "uv"
          python-shell-interpreter-args "run python -i")))
