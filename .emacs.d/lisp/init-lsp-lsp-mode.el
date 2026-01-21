(leaf lsp-mode
  :ensure t
  :init (yas-global-mode)
  :hook ((rust-mode . lsp-deferred)
         (ruby-ts-mode-hook . lsp-deferred)
         (python-ts-mode-hook . lsp-deferred)
         (typescript-mode-hook . lsp-deferred)
         (typescript-ts-mode-hook . lsp-deferred)
         (tsx-ts-mode-hook . lsp-deferred)
         (conf-toml-mode-hook . lsp-deferred) ;; require: `cargo install taplo-cli --features lsp`
         (terraform-mode-hook . lsp-deferred) ;; require `brew install hashicorp/tap/terraform-ls`
         (lsp-mode-hook . lsp-ui-mode))
  :bind
  (("C-c h" . lsp-describe-thing-at-point)
   ("C-c C-c a" . lsp-execute-code-action)
   ("C-c C-c r" . lsp-rename)
   ("M-." . lsp-find-definition))
  :custom
  ((lsp-message-project-root-warning . t)
   (lsp-auto-guess-root . nil)
   (lsp-restart . 'auto-restart)
   (lsp-log-io . nil)
   (lsp-eldoc-render-all . t)
   (lsp-lens-mode . t)
   (lsp-completion-provider . :none) ;; to completion using corfu
   (lsp-enable-links . t)
   (lsp-disabled-clients . '(pyls pylsp))

   ;; ruby --
   ;; solargraphを使う場合
   (lsp-solargraph-use-bundler . t)
   (lsp-solargraph-library-directories . '("~/.rbenv/shims/"))
   ;; ruby-lspを使う場合
   ;; (lsp-disabled-clients . '(rubocop-ls))
   ;; (lsp-enabled-clients . '(ruby-lsp-ls))
))

(leaf lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-border . (face-foreground 'default))
  (lsp-ui-doc-enable . t)
  (lsp-ui-doc-deley . 0.5)
  (lsp-ui-doc-header . t)
  (lsp-ui-doc-include-signature . t)
  (lsp-ui-doc-max-width . 150)
  (lsp-ui-doc-max-height . 30)
  (lsp-ui-doc-position . 'at-point)
  (lsp-ui-doc-use-childframe . t)
  (lsp-ui-doc-use-webkit . nil)
  (lsp-ui-doc-show-with-cursor . t)
  (lsp-ui-doc-show-with-mouse . t)
  (lsp-ui-peek-always-show . t)
  (lsp-ui-peek-enable . t)
  (lsp-ui-peek-peek-height . 20)
  (lsp-ui-peek-list-width . 50)
  (lsp-ui-peek-fontify . 'on-demand) ;; never, on-demand, or always
  (lsp-ui-sideline-delay . 0.05)
  (lsp-ui-sideline-show-hover . t)
  (lsp-ui-sideline-show-code-actions . t))

(leaf lsp-pyright
  :ensure t
  :custom
  (lsp-pyright-langserver-command . "basedpyright") ;; requires: `uv tool install basedpyright`
  (lsp-pyright-auto-import-completions . t)
  (lsp-pyright-typechecking-mode . "basic") ;; off, basic, strict
  (lsp-pyright-use-library-code-for-types . t))
