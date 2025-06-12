(leaf yasnippet
  :ensure t
  :custom
  (yas-snippet-dirs . '("~/.emacs.d/snippets")))

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ;; ("<tab>" . company-complete-selection)
          ("TAB" . company-select-next)
          ("<backtab>" . company-select-previous))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence))
           (company-selection-wrap-around . t))
  :global-minor-mode global-company-mode)


;; Emacsの次世代ミニバッファ補完UI
;; https://blog.tomoya.dev/posts/a-new-wave-has-arrived-at-emacs/
(leaf orderless
  :doc "補完スタイルの提供"
  :ensure t
  :custom (completion-styles . '(orderless)))
(leaf marginalia
  :ensure t
  :doc "ミニバッファの右側に追加情報を表示する"
  :init (marginalia-mode)
  :global-minor-mode t)
(leaf vertico
  :doc "ミニバッファ補完UI"
  :ensure t
  :global-minor-mode vertico-mode
  :bind ((minibuffer-local-map (
          ("C-l" . vertico-directory-up)
                                )))
  :custom ((vertico-count . 20)))
(leaf embark
  :ensure t
  :bind (("C-." . embark-act)
         (minibuffer-local-map
          :package emacs
          ("M-." . embark-dwim)
          ("C-." . embark-act)))
  :config (setopt embark-help-key "?") ;;  Embark が起動している時に ? を叩いたら help が出る
  )
(leaf consult
  :doc "補完候補リストの作成と便利な補完コマンド"
  :url "https://github.com/minad/consult"
  :ensure t
  :bind (("C-x C-b" . consult-buffer)
         ("C-x l" . consult-goto-line)
         ("C-s" . consult-line)
         ("C-c s" . consult-ripgrep)
         ("C-c C-s" . consult-ripgrep-specific-directory)
         ("C-c C-r" . consult-recent-file))
  ;; :custom ((consult-find-command . "fd --color=never --full-path ARG OPTS"))
  :config
  (defun consult-ripgrep-specific-directory () ;; 都度対象ディレクトリを指定して検索できる
    (interactive)
    (let ((consult-project-function (lambda (_) default-directory)))
      (consult-ripgrep (read-directory-name "Directory: "))))
  (leaf consult-ghq
    :doc "ghq interface"
    :if (executable-find "ghq")
    :ensure t
    :bind (("M-g M-f" . consult-ghq-find))
    :custom ((consult-ghq-find-function . 'dired)))
  ;; (leaf consult-lsp
  ;;   :ensure t)
  (leaf embark-consult
    :ensure t))

(leaf corfu
  :ensure t
  :doc "Completion Overlay Region FUnction(alternative to company-mode)"
  :url "https://github.com/minad/corfu"
  :init (global-corfu-mode)
  :custom ((corfu-popupinfo-mode . t)
           (corfu--auto . t)
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

