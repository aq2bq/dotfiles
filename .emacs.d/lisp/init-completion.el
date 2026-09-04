;;; init-completion.el --- In-buffer completion -*- lexical-binding: t; -*-

;; (leaf company
;;   :doc "Modular text completion framework"
;;   :req "emacs-24.3"
;;   :tag "matching" "convenience" "abbrev" "emacs>=24.3"
;;   :url "http://company-mode.github.io/"
;;   :emacs>= 24.3
;;   :ensure t
;;   :blackout t
;;   :leaf-defer nil
;;   :bind ((company-active-map
;;           ("M-n" . nil)
;;           ("M-p" . nil)
;;           ("C-s" . company-filter-candidates)
;;           ("C-n" . company-select-next)
;;           ("C-p" . company-select-previous)
;;           ;; ("<tab>" . company-complete-selection)
;;           ("TAB" . company-select-next)
;;           ("<backtab>" . company-select-previous))
;;          (company-search-map
;;           ("C-n" . company-select-next)
;;           ("C-p" . company-select-previous)))
;;   :custom ((company-idle-delay . 0)
;;            (company-minimum-prefix-length . 1)
;;            (company-transformers . '(company-sort-by-occurrence))
;;            (company-selection-wrap-around . t))
;;   :global-minor-mode global-company-mode)


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
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-file t)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block t))


(provide 'init-completion)
;;; init-completion.el ends here

(leaf yasnippet
  :ensure t
  :custom
  (yas-snippet-dirs . '("~/.emacs.d/snippets")))


(leaf corfu
  :ensure t
  :doc "Completion Overlay Region FUnction(alternative to company-mode)"
  :url "https://github.com/minad/corfu"
  :init (global-corfu-mode)
  :custom ((corfu-popupinfo-mode . t)
           (corfu-auto . t)
           (corfu-auto-prefix . 1)
           (corfu-auto-delay . 0)
           (corfu-cycle . t) ;; Enable cycling for `corfu-next/previous'
           (corfu-preselect 'prompt) ;; Always preselect the prompt
           )
  :bind (corfu-map ;; https://github.com/minad/corfu?tab=readme-ov-file#tab-and-go-completion
         ;; ("TAB" . corfu-next)
         ;; ("<tab>" . corfu-next)
         ([tab] . corfu-next)
         ([backtab] . corfu-previous)
         ;; ("S-TAB" . corfu-previous)
         ;; ("<backtab>" . corfu-previous)
         )
)

(leaf cape
  :ensure t
  :doc "Cape provides Completion At Point Extensions which can be used in combination with Corfu, Company or the default completion UI"
  :url "https://github.com/minad/cape"
  :custom ((dabbrev-case-fold-search . t))
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
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
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))
