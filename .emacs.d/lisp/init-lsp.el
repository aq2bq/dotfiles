(defvar my/lsp-backend :lsp-mode)

(cl-case my/lsp-backend
  (:eglot (load "init-lsp-eglot"))
  (:lsp-mode (load "init-lsp-lsp-mode")))

(leaf eldoc-box
  :ensure t
  :url "https://github.com/casouri/eldoc-box"
  :doc "Display documentation in a popup box"
  ;; :hook ((eglot-managed-mode-hook . eldoc-box-hover-at-point-mode))
  )
