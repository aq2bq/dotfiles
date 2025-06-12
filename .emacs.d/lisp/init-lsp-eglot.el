(leaf eglot
  :ensure t
  :doc "Emacs Polyglot: an Emacs LSP client that stays out of your way"
  :bind (("C-c h" . eldoc-doc-buffer))
  :preface
  (defun my/setup-deno-if-applicable ()
    "プロジェクトに deno.json があれば deno LSP に切り替える。"
    (when (locate-dominating-file default-directory "deno.json")
      (setq-local eglot-server-programs
                  '((typescript-ts-mode . ("deno" "lsp"))))
      (eglot-ensure)))
  :hook (
         (ruby-ts-mode-hook . eglot-ensure)
         (typescript-ts-mode-hook . eglot-ensure)
         (typescript-ts-mode . my/setup-deno-if-applicable)
         (before-save-hook . eglot-format-buffer)
         )
  :custom
  (eglot-inlay-hints-mode . t) ;; LSPインレイヒントのオン/オフを切り替えます
  (eldoc-echo-area-use-multiline-p . t)
  :config
  ;; requires: `gem install lsp_router`'
  ;; view log: `tail -F /tmp/lsp_router*`
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode)
                 . ("lsp_router"
                    "--error=/tmp/lsp_router.err"
                    "~/.emacs.d/ruby-lsp_router.conf")))
  ;;
  ;; requires: `gem install ruby-lsp`
  ;; (add-to-list 'eglot-server-programs
  ;;              '((ruby-mode ruby-ts-mode) "ruby-lsp"))
  ;; (add-to-list 'eglot-server-programs
  ;;              ;; https://docs.basedpyright.com/latest/installation/ides/
  ;;              '((python-mode python-ts-mode)
  ;;                "rye" "run" "basedpyright" "--stdio"))
  ;; (setq-default eglot-workspace-configuration
  ;;               '(:basedpyright
  ;;                 (:typeCheckingMode "recommended")
  ;;                 :basedpyright.analysis
  ;;                 (:diagnosticSeverityOverrides
  ;;                  (:reportUnusedCallResult "none")
  ;;                  :inlayHints
  ;;                  (:callArgumentNames :json-false)
  ;;                  )))
  )

(leaf eglot-booster
  :ensure t
  :after eglot
  :global-minor-mode t)

(leaf consult-eglot
  :ensure t
  :doc "A consulting-read interface for eglot."
  :url "https://github.com/mohkale/consult-eglot"
  :after eglot
  :config
)
(leaf consult-eglot-embark
  :ensure t
  :after consult-eglot
  :init (consult-eglot-embark-mode))
