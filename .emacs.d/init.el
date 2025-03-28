;; Emacs for Mac
;; brew install --cask emacs-mac-spacemacs-icon

;;;
;; Global key settings
;;;

;; Move to other window C-q
(defun other-window-or-split (val)
  (interactive)
  (when (one-window-p)
    (split-window-horizontally)
    (split-window-vertically))
  (other-window val))

(global-set-key (kbd "C-q") (lambda () (interactive) (other-window-or-split 1)))
(global-set-key (kbd "C-S-q") (lambda () (interactive) (other-window-or-split -1)))
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c n") 'next-buffer)
(global-set-key (kbd "C-c b") 'previous-buffer)
;; (global-set-key (kbd "C-x l") 'goto-line)
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-x f") 'project-find-file)
(global-set-key (kbd "M-TAB") 'other-frame)
(defvar mac-command-modifier 'super)

;;;
;; General Settings
;;;

;; メタキーをaltに(GUI)
(when (display-graphic-p)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (global-set-key (kbd "s-c") 'kill-ring-save) ; コピー
  (global-set-key (kbd "s-x") 'kill-region)    ; カット
  (global-set-key (kbd "s-v") 'yank)           ; ペースト
  (global-set-key (kbd "s-z") 'undo)           ; 元に戻す
  (global-set-key (kbd "s-s") 'save-buffer)    ; 保存
  (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "C-c +") 'text-scale-increase)
  (global-set-key (kbd "C-c -") 'text-scale-decrease)
  (global-set-key (kbd "C-c 0") 'text-scale-adjust))

;; GUIモード時の透明度設定
(when (display-graphic-p)
  (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90))))

;; Highlight current line
(global-hl-line-mode t)

(when (display-graphic-p)
  (setq use-default-font-for-symbols nil)

  (let ((size (if (>= (x-display-pixel-width) 4000) 140 120)))
    (set-face-attribute 'default nil
                        :family "UDEV Gothic 35NFLG"
                        :height size))

  ;; (set-face-attribute 'default nil
  ;;                     :family "UDEV Gothic 35NFLG"
  ;;                     :height 140)

  ;; 1.すべて日本語対応フォントで表示するパターン
  ;; (set-face-attribute 'default nil
  ;;                     :family "Cica"
  ;;                     :height 180)

  ;; ;; 2. ASCIIフォントと日本語フォントを区別するパターン
  ;; ;; ASCIIフォントの設定
  ;; (set-face-attribute 'default nil
  ;;                     :family "Monoid"
  ;;                     :height 140)
  ;; ;; 非ASCIIフォントの設定
  ;; ;; https://misohena.jp/blog/2017-09-26-symbol-font-settings-for-emacs25.html
  ;; (set-fontset-font nil '(#x80 . #x10ffff) (font-spec :family "Cica" :size 18) nil 'prepend)
)

;; メニューバーの非表示
(menu-bar-mode 0)
;; ツールバー(GUI)
(tool-bar-mode 0)
;; 行番号
(display-line-numbers-mode t)

;; set tab width
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; 括弧を自動で補完する
(electric-pair-mode 1)

;; Highlight paren
(show-paren-mode 1)

;; replace inputting yes-no to y-n
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight a blank-space of end of line
(setq-default show-trailing-whitespace t)


;; encoding
(set-language-environment       "Japanese")
(setq file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(prefer-coding-system           'utf-8-unix)
(set-buffer-file-coding-system  'utf-8)
(set-terminal-coding-system     'utf-8)
(set-keyboard-coding-system     'utf-8)
(set-clipboard-coding-system    'utf-8)
(setenv "LC_ALL" "ja_JP.UTF-8")

;; disable generate backupfile
(setq make-backup-files nil)
(setq auto-save-default nil)

;; copy mouse drag region
(setq mouse-drag-copy-region t)

;; for support `ls --dired`
(let ((gls (substring (shell-command-to-string "which gls") 0 -1)))
  (if (file-exists-p gls) (setq insert-directory-program gls)))

;; enable paste to region
(delete-selection-mode t)

;; Enable copy mouse drag region
(setq mouse-drag-copy-region t)

;; use flash instead of beeping
(setq visible-bell t)

;; Again the emacs default is too low 4k considering that the some of the language server responses are in 800k - 3M range
;; https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
(setq read-process-output-max (* 1024 1024)) ;; 1MB

;; The default setting is too low for lsp-mode's needs due to the fact that
;; client/server communication generates a lot of memory/garbage.
;; https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold
(setq gc-cons-threshold 100000000)

;; 行数表示
(global-display-line-numbers-mode)

(setq window-divider-mode t)

;;;
;; Initialize
;;;

(require 'package)
(require 'bytecomp)

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-convert :ensure t)
  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init))
)

;;;
;; Emacs global extentions
;;;

(leaf exec-path-from-shell
  :doc "ensure environment variables inside Emacs look the same as in the user's shell"
  :ensure t
  :init (exec-path-from-shell-initialize))

(leaf sublimity
  :url "https://github.com/zk-phi/sublimity"
  :doc "smooth-scrolling, minimap, and distraction-free writing"
  :ensure t
  :config
  (require 'sublimity-scroll)
  (require 'sublimity-attractive)
  :custom ((sublimity-scroll-weight . 20)
           (sublimity-scroll-drift-length . 5)
           (sublimity-attractive-centering-width . 150)
           (sublimity-mode . t)))

(leaf topsy
  :doc "simple alternative to `semantic-stickyfunc-mode`"
  :ensure t
  :hook (prog-mode-hook . topsy-mode))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf vterm
  ;; requirements: brew install cmake
  :doc "Emacs libvterm integration"
  :url "https://github.com/akermu/emacs-libvterm"
  :ensure t
  :bind (vterm-mode-map
         ("C-h" . vterm-send-backspace)
         ("C-g" . vterm-send-escape)
         ("C-l" . vterm-clear)
         ("C-c C-c" . vterm--self-insert)
         ("C-q" . other-window))
  :hook (vterm-mode-hook . (lambda () (display-line-numbers-mode -1)))
  :custom ((vterm-max-scrollback . 10000))
)

(leaf vterm-toggle
  :doc "toggles between the vterm buffer and whatever buffer you are editing."
  :ensure t
  :custom (vterm-toggle-scope . 'project)
  :bind (("C-s-t" . vterm-toggle)))

(leaf doom-themes
  :ensure t
  :config
  ;; POP系
  ;; (load-theme 'doom-laserwave t nil)
  ;; (load-theme 'doom-dracula t nil)
  ;; (load-theme 'doom-challenger-deep t nil)
  ;; (load-theme 'doom-outrun-electric t nil)
  ;; (load-theme 'doom-shades-of-purple t nil)
  ;; 青系
  (load-theme 'doom-ephemeral t nil)
  ;; (load-theme 'doom-nova t nil)
  ;; (load-theme 'doom-moonlight t nil)
  ;; (load-theme 'doom-palenight t nil)
  ;; (load-theme 'doom-city-lights t nil)
  ;; (load-theme 'doom-oksolar-dark t nil)
  ;; (load-theme 'doom-solarized-dark t nil)
  (doom-themes-neotree-config)
  (when (equal (car custom-enabled-themes) 'doom-ephemeral)
    (custom-set-faces
     '(font-lock-comment-face ((t (:foreground "snow3"))))
     '(font-lock-doc-face ((t (:foreground "honeydew2"))))))
  :custom
  (doom-themes-visual-bell-config . t)
  (doom-themes-enable-italic . t)
  (doom-themes-enable-bold . t)
  (doom-modeline-bar . t)
  (doom-outrun-electric-brighter-comments . t)
  (doom-outrun-electric-brighter-modeline . t)
  (doom-outrun-electric-comment-bg . nil)
  (doom-dracula-brighter-comments . t)
  (doom-dracula-comment-bg . t)
  (doom-challenger-deep-padded-modeline . t)
  (doom-challenger-deep-comment-bg . t)
  (doom-challenger-deep-brighter-comments . t)
  (doom-challenger-deep-brighter-modeline . t)
  (doom-ephemeral-brighter-comments . t)
  (doom-ephemeral-brighter-modeline . t)
  )

(leaf ligature
  :ensure t
  :config
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  :custom ((global-ligature-mode . t)))

;; (leaf srcery-theme
;;   :ensure t
;;   :config
;;   (load-theme 'srcery t))
;; (leaf madhat2r-theme
;;   :ensure t
;;   :config
;;   (load-theme 'madhat2r t))
;; (load-theme 'tango-dark t)

(leaf all-the-icons
  :doc "A utility package to collect various Icon Fonts and propertize them within Emacs."
  :ensure t
  :if (display-graphic-p)) ;; require `M-x all-the-icons-install-fonts`

(leaf centaur-tabs
  :ensure t
  :url "https://github.com/ema2159/centaur-tabs"
  :config (centaur-tabs-mode t)
  :bind ((centaur-tabs-mode-map
          ("C-s-n" . centaur-tabs-forward-tab)
          ("C-s-p" . centaur-tabs-backward-tab)))
  :custom ((centaur-tabs-set-icons . t)
           (centaur-tabs-style . "chamfer")
           (centaur-tabs-set-bar . "over")) ;; To display an overline over the selected tab
  )

(leaf dirvish
  :doc "Dirvish is an improved version of the Emacs inbuilt package Dired"
  :url "https://github.com/alexluigit/dirvish"
  :ensure t
  :bind ((dirvish-mode-map
          ([tab] . dirvish-subtree-toggle)))
  :init (dirvish)
  :config
  (dirvish-override-dired-mode)
  (dirvish-peek-mode) ;; ミニバッファでファイルをプレビュー
  (dirvish-side-follow-mode)
  :custom
  (dired-listing-switches . "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-default-layout . '(0 0.4 0.6)) ;; 親ディレクトリを非表示
  ;; (dirvish-side-width . 38)
  ;; (dirvish-header-line-height . '(25 . 35))
  (dirvish-header-line-format . '(:left (path) :right (free-space)))
  (dirvish-mode-line-format . '(:left (sort symlink) :right (yank index)))
  (dirvish-attributes . '(
                          ;; vc-state      ;; バージョン管理の状態を左端に表示(Error: void-variable (dirvish-vc--always-ignored))
                          ;; git-msg       ;; Gitのコミットメッセージをファイル名に追加(Error: void-variable (dirvish-vc--always-ignored))
                          all-the-icons ;; ファイルのアイコン
                          file-time     ;; 変更時刻
                          file-size     ;; ファイルサイズ
                          collapse      ;; ネストされたパスを折りたたむ
                          subtree-state ;; ディレクトリの展開状態を表示
                                        ))
  )

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
i  :leaf-defer nil
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
  :doc "Enrich existing commands with completion annotations"
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
          ("C-." . embark-act))))
(leaf consult
  :doc "補完候補リストの作成と便利な補完コマンド"
  :url "https://github.com/minad/consult"
  :ensure t
  :bind (("C-x C-b" . consult-buffer)
         ("C-x l" . consult-goto-line)
         ("C-s" . consult-line)
         ("C-c s" . consult-ripgrep)
         ("C-c C-s" . consult-ripgrep-specific-directory))
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
    :bind (("s-g s-g" . consult-ghq-find))
    :custom ((consult-ghq-find-function . 'dired)))
  ;; (leaf consult-lsp
  ;;   :ensure t)
  (leaf embark-consult
    :ensure t))


(leaf anzu
  :doc "provides a minor mode which displays current match and total matches information in the mode-line in various search modes"
  :ensure t
  :bind
  (("C-c r" . anzu-query-replace)
   ("C-c C-m" . anzu-query-replace-at-cursor-thing))
  :custom
  ((global-anzu-mode . t)
   (anzu-mode-lighter . "")
   ((anzu-deactivate-region . t)
    (anzu-deactivate-region . t)
    (anzu-search-threshold . 1000))))

(leaf yafolding
  :doc "Folding code blocks based on indentation."
  :url "https://github.com/zenozeng/yafolding.el"
  :ensure t
  :bind ("C-c i" . yafolding-toggle-element))

(leaf highlight-indent-guides
  :url "https://github.com/DarthFennec/highlight-indent-guides"
  :ensure t
  :custom
  ((highlight-indent-guides-method . 'fill)
   (highlight-indent-guides-auto-enabled . t)
   (highlight-indent-guides-responsive . t))
    :hook (((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode)))

(leaf which-key
  :doc "minor mode for Emacs that displays the key bindings following your currently entered incomplete command (a prefix) in a popup."
  :ensure t
  :init (which-key-mode))

(leaf fish-mode
  :doc "Emacs major mode for fish shell scripts."
  :ensure t
  :custom
  ((fish-indent-offset . 2)))

(leaf nyan-mode
  :doc "analog indicator of your position in the buffer"
  :ensure t
  :url "https://github.com/TeMPOraL/nyan-mode"
  :init (nyan-mode)
  :custom ((nyan-animate-nyancat . t)
           (nyan-wavy-trail . t)))
(leaf save-hist
  :doc "Save minibuffer history"
  :tag "builtin"
  :custom ((savehist-mode . t)
           (history-length . 1000)
           (savehist-additional-variables . '(kill-ring search-ring regexp-search-ring))))
(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :global-minor-mode global-auto-revert-mode)

;;;
;; Programming
;;;
(leaf flymake
  :ensure t
  :bind (flymake-mode-map
         ("C-x C-p" . flymake-goto-prev-error)
         ("C-x C-n" . flymake-goto-next-error))
  :custom ((flymake-mode . nil)
           (flymake-no-changes-timeout . 0.0)  ;; チェックの頻度を減らす場合は値(秒)を増やす
           (flymake-proc-legacy-flymake . t) ;; エラーメッセージのポップアップ
           (flymake-start-syntax-check-on-newline . nil) ;; 行追加するごとにチェックしない
           (flymake-start-syntax-check-on-find-file . nil) ;; ファイルを開いたときにチェックしない
           (flymake-start-on-save-buffer . t)  ;; 保存時のみチェック
           (flymake-diagnostic-functions . '(flymake-proc-legacy-flymake)))
  :config
  (set-face-foreground 'flymake-errline "white")
  (set-face-background 'flymake-errline "red4")
  (set-face-foreground 'flymake-warnline "white")
  (set-face-background 'flymake-warnline "goldenrod3"))


(leaf yasnippet
  :ensure t
  :custom
  (yas-snippet-dirs . '("~/.emacs.d/snippets")))
(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom (show-paren-style . 'mixed)
  :config
  (show-paren-mode t))

(leaf rainbow-delimiters
  :url "https://github.com/Fanael/rainbow-delimiters"
  :ensure t
  :hook ((prog-mode-hook . rainbow-delimiters-mode)))

(leaf eglot
  :ensure t
  :doc "Emacs Polyglot: an Emacs LSP client that stays out of your way"
  :bind (("C-c h" . eldoc-doc-buffer))
  :hook (
         (ruby-ts-mode-hook . eglot-ensure)
         (typescript-ts-mode-hook . eglot-ensure)
         )
  :custom
  (eglot-inlay-hints-mode . t) ;; LSPインレイヒントのオン/オフを切り替えます
  (eldoc-echo-area-use-multiline-p . t)
  :config
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
(leaf kind-icon
  :ensure t
  :after corfu
  :custom
  ((kind-icon-default-face . 'corfu-default))
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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


(leaf magit
  :ensure t)

(leaf yaml-mode
  :ensure t
  :mode "\\(\.yml\\|\.yaml\\)")

(leaf ruby-ts-mode
  :mode
  (("\\.rb$" . ruby-ts-mode)
	 ("Gemfile$" . ruby-ts-mode)
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
  :hook ((eglot-ensure))
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


;; - ref: Emacs 29 でTree-sitterを利用してシンタックスハイライトする
;;   https://zenn.dev/hyakt/articles/42a1d237cdfa2a
(leaf treesit
  :custom ((treesit-font-lock-level . 4)))

(leaf treesit-auto
  :ensure t
  :url "https://github.com/renzmann/treesit-auto"
  :global-minor-mode global-treesit-auto-mode
  :custom ((global-treesit-auto-modes . '((not ruby-mode)(not rust-mode)))
           ;; (treesit-auto-langs . '(typescript))
           (treesit-auto-install . t)
           ;; https://github.com/renzmann/treesit-auto#keep-track-of-your-hooks
           ;; (python-ts-mode-hook . python-mode-hook)
           ;; (typescript-ts-mode-hook . typescript-mode-hook)
           )

  :config
  (global-treesit-auto-mode))

(leaf copilot
  :ensure t
  :custom
  ;; 公式SDKを使うようになって以降設定しておかないとなぜか見つけてくれない
  ;; `(copilot-server-executable . ,(expand-file-name "~/.emacs.d/.cache/copilot/bin/copilot-language-server"))
  (copilot-indent-offset-warning-disable . t)
  (copilot-max-char-warning-disable . t) ;; スキーマ系の長大なファイルで警告が出るので無効化
  :config
  (leaf editorconfig
    :ensure t)
  (leaf s
    :ensure t)
  (leaf dash
    :ensure t)
  :hook (prog-mode-hook . copilot-mode)
  :bind ((copilot-completion-map
          ("C-<return>" . copilot-accept-completion)
          ("C-c TAB" . copilot-accept-completion-by-word)
          ("C-c f" . copilot-accept-completion-by-line))))
(leaf copilot-chat
  :ensure t
  :url "https://github.com/chep/copilot-chat.el"
  :bind (
         ("C-c C-p t" . copilot-chat-transient)
         ("C-c C-p c" . copilot-chat-transient-buffers)
         ("C-c C-p c" . copilot-chat-transient-code)
         ("C-c C-p m" . copilot-chat-transient-magit)
         )
  :custom (
           (copilot-chat-model . "gpt-4o")
           ;; (copilot-chat-model . "o3-mini")
           ;; (copilot-chat-model . "gemini-2.0-flash-001")
           ;; (copilot-chat-model . "claude-3.5-sonnet")
           (copilot-chat-prompt-doc . "/doc 以下のコードについてドキュメントを書いて:\n")
           (copilot-chat-prompt-explain . "/explain 日本語で説明:\n")
           (copilot-chat-prompt-fix . "/fix 問題箇所を修正して、修正内容の解説して:\n")
           (copilot-chat-prompt-optimize . "/optimize パフォーマンスと可読性を向上させるため、以下のコードを最適化:\n")
           (copilot-chat-prompt-review . "/review 以下のコードをレビューして:\n")))

(leaf ollama-buddy
  :ensure t
  :bind ("C-c l" . ollama-buddy-menu)
  :config ;; (ollama-buddy-enable-monitor)
  :custom
  (ollama-buddy-menu-columns . 4)
  (ollama-buddy-command-definitions
   .
   '(
     (open-chat :key 111 :description "Open Chat" :model nil :action
                (lambda nil
                  (pop-to-buffer
                   (get-buffer-create ollama-buddy--chat-buffer))
                  (when
                      (=
                       (buffer-size)
                       0)
                    (insert
                     (ollama-buddy--create-intro-message)))
                  (goto-char
                   (point-max))))
     (show-models :key 118 :description "View model status" :model nil :action ollama-buddy-show-model-status)
     (swap-model :key 109 :description "Swap model" :model nil :action
                 (lambda nil
                   (if
                       (not
                        (ollama-buddy--ollama-running))
                       (error "!!WARNING!! ollama server not running")
                     (let
                         ((new-model
                           (completing-read "Model: "
                                            (ollama-buddy--get-models)
                                            nil t)))
                       (setq ollama-buddy-default-model new-model)
                       (setq ollama-buddy--current-model new-model)
                       (ollama-buddy--update-status "Idle")))))
     (help :key 104 :description "Help assistant" :model nil :action
           (lambda nil
             (pop-to-buffer
              (get-buffer-create ollama-buddy--chat-buffer))
             (goto-char
              (point-max))
             (insert
              (ollama-buddy--create-intro-message))))
     (send-region :key 108 :description "Send region" :model nil :action
                  (lambda nil
                    (ollama-buddy--send-with-command 'send-region)))
     (refactor-code :key 114 :description "Refactor code" :model nil :prompt "以下のコードをリファクタリングしてください:" :action
                    (lambda nil
                      (ollama-buddy--send-with-command 'refactor-code)))
     (git-commit :key 103 :description "Git commit message" :model nil :prompt "以下の内容に対して簡潔なGitコミットメッセージを書いてください:" :action
                 (lambda nil
                   (ollama-buddy--send-with-command 'git-commit)))
     (describe-code :key 99 :description "Describe code" :model nil :prompt "以下のコードを説明してください:" :action
                    (lambda nil
                      (ollama-buddy--send-with-command 'describe-code)))
     (dictionary-lookup :key 100 :description "Dictionary Lookup" :model nil :prompt-fn
                        (lambda nil
                          (concat "単語 {"
                                  (buffer-substring-no-properties
                                   (region-beginning)
                                   (region-end))
                                  "} の意味を辞書のように説明してください:"))
                        :action
                        (lambda nil
                          (ollama-buddy--send-with-command 'dictionary-lookup)))
     (synonym :key 110 :description "Word synonym" :model nil :prompt "この単語の類義語とその意味合いをリストアップしてください:" :action
              (lambda nil
                (ollama-buddy--send-with-command 'synonym)))
     (proofread :key 112 :description "Proofread text" :model nil :prompt "誤字脱字やスペルを修正して:" :action
                (lambda nil
                  (ollama-buddy--send-with-command 'proofread)))
     (make-concise :key 122 :description "Make concise" :model nil :prompt "元の意味を損なわず簡潔にして:" :action
                   (lambda nil
                     (ollama-buddy--send-with-command 'make-concise)))
     (custom-prompt :key 101 :description "Custom prompt" :model nil :action
                    (lambda nil
                      (when-let
                          ((prefix
                            (read-string "Enter prompt prefix: " nil nil nil t)))
                        (unless
                            (string-empty-p prefix)
                          (ollama-buddy--send prefix)))))
     (save-chat :key 115 :description "Save chat" :model nil :action
                (lambda nil
                  (with-current-buffer ollama-buddy--chat-buffer
                    (write-region
                     (point-min)
                     (point-max)
                     (read-file-name "Save conversation to: ")
                     'append-to-file nil))))
     (kill-request :key 120 :description "Kill request" :model nil :action
                   (lambda nil
                     (delete-process ollama-buddy--active-process)))
     (quit :key 113 :description "Quit" :model nil :action
           (lambda nil
             (message "Quit Ollama Shell menu.")))
     ))
  )

(provide 'init)

