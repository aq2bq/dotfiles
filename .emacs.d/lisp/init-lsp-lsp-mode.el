(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          ;; resolve command from exec-path (in case not found in $PATH)
          (when-let ((command-from-exec-path (executable-find (car orig-result))))
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

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
         (lsp-mode-hook . lsp-ui-mode)
         (lsp-mode . lsp-enable-which-key-integration))
  :bind
  (("C-c h" . lsp-describe-thing-at-point)
   ("C-c C-c a" . lsp-execute-code-action)
   ("C-c C-c r" . lsp-rename)
   ("M-." . lsp-find-definition))
  :custom
  ((lsp-message-project-root-warning . t)
   (lsp-auto-guess-root . nil)
   (lsp-headerline-breadcrumb-enable . nil)
   (lsp-restart . 'auto-restart)
   (lsp-log-io . nil)
   (lsp-eldoc-render-all . t)
   (lsp-lens-mode . t)
   (lsp-completion-provider . :none) ;; to completion using corfu
   (lsp-enable-links . t)
   (lsp-disabled-clients . '(rubocop-ls pyls pylsp))
   (lsp-response-timeout . 30)
   (lsp-log-io . nil) ;; too heavy

   ;; ruby --
   ;; solargraphを使う場合
   (lsp-solargraph-use-bundler . t)
   (lsp-solargraph-library-directories . '("~/.rbenv/shims/"))
   ;; sorbetを併用
   ;; (lsp-sorbet-as-add-on . t)
   ;; (lsp-sorbet-use-bundler . t)
   ;; ruby-lspを使い場合
   ;; (lsp-ruby-lsp-use-bundler . t)
   ;; (lsp-enabled-clients . '(
   ;;                          ruby-lsp-ls
   ;;                          ;; solargraph-ls
   ;;                          ))
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
