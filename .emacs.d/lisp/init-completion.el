;;; init-completion.el --- In-buffer completion -*- lexical-binding: t; -*-

;; Snippets
(leaf yasnippet
  :ensure t
  :custom
  (yas-snippet-dirs . '("~/.emacs.d/snippets"))
  :global-minor-mode yas-global-mode)


;; In-buffer completion UI
(leaf corfu
  :ensure t
  :doc "Completion Overlay Region FUnction"
  :url "https://github.com/minad/corfu"

  :global-minor-mode global-corfu-mode

  :custom
  ((global-corfu-minibuffer . t)
   (corfu-auto . t)
   (corfu-auto-prefix . 1)
   (corfu-auto-delay . 0.1)
   (corfu-cycle . t)
   (corfu-preselect . 'prompt))

  :bind
  (corfu-map
   ("TAB" . corfu-next)
   ([tab] . corfu-next)
   ("S-TAB" . corfu-previous)
   ([backtab] . corfu-previous))

  :config
  ;; Show documentation for the selected completion candidate.
  (corfu-popupinfo-mode 1))


(leaf kind-icon
  :ensure t
  :after corfu
  :custom
  ((kind-icon-default-face . 'corfu-default))
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; Additional completion-at-point backends
(leaf cape
  :ensure t
  :require t
  :doc "Completion At Point Extensions"
  :url "https://github.com/minad/cape"

  :custom
  ((dabbrev-case-fold-search . t))

  :bind
  (("C-c p p" . completion-at-point)
   ("C-c p t" . complete-tag)
   ("C-c p d" . cape-dabbrev)
   ("C-c p h" . cape-history)
   ("C-c p f" . cape-file)
   ("C-c p k" . cape-keyword)
   ("C-c p s" . cape-elisp-symbol)
   ("C-c p e" . cape-elisp-block)
   ("C-c p a" . cape-abbrev)
   ("C-c p l" . cape-line)
   ("C-c p w" . cape-dict)
   ("C-c p :" . cape-emoji)
   ("C-c p \\" . cape-tex)
   ("C-c p _" . cape-tex)
   ("C-c p ^" . cape-tex)
   ("C-c p &" . cape-sgml)
   ("C-c p r" . cape-rfc1345))

  :config
  ;; Major mode / LSP が提供する CAPF を優先し、
  ;; Cape は fallback として後ろに追加する。
  (setq-default completion-at-point-functions
                (append
                 (seq-difference
                  (default-value 'completion-at-point-functions)
                  '(cape-dabbrev cape-file cape-elisp-block))
                 '(cape-dabbrev cape-file cape-elisp-block))))


(provide 'init-completion)
;;; init-completion.el ends here
