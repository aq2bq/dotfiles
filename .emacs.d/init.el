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
  (set-frame-parameter (selected-frame) 'alpha '(85 . 85))
  (add-to-list 'default-frame-alist '(alpha . (85 . 85))))

;; Highlight current line
(global-hl-line-mode t)

(when (display-graphic-p)
  (setq use-default-font-for-symbols nil)

  (set-face-attribute 'default nil
                      :family "UDEV Gothic 35NFLG"
                      :height 140)

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
    (leaf-keywords-init)))

;;;
;; Emacs global extentions
;;;

(leaf exec-path-from-shell
  :doc "ensure environment variables inside Emacs look the same as in the user's shell"
  :ensure t
  :init (exec-path-from-shell-initialize))

(leaf good-scroll
  :doc "Attempt at good pixel-based smooth scrolling in Emacs"
  :ensure t
  :bind (("C-v" . good-scroll-up-full-screen)
         ("M-v" . good-scroll-down-full-screen))
  :init (good-scroll-mode t))

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
  ;; (load-theme 'doom-laserwave t nil)
  ;; (load-theme 'doom-dracula t nil)
  ;; (load-theme 'doom-challenger-deep t nil)
  (load-theme 'doom-outrun-electric t nil)
  (doom-themes-neotree-config)
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
  (doom-challenger-deep-brighter-modeline . t))

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
  :config (dirvish-override-dired-mode)
  :custom ( ;; https://github.com/alexluigit/dirvish/blob/main/docs/CUSTOMIZING.org#dirvish-attributes
           (dirvish-attributes . '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))))

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
  :init (marginalia-mode))
(leaf vertico
  :doc "ミニバッファ補完UI"
  :ensure t
  :global-minor-mode vertico-mode
  :custom ((vertico-count . 20)))
(leaf consult
  :doc "補完候補リストの作成と便利な補完コマンド"
  :url "https://github.com/minad/consult"
  :ensure t
  :bind (("C-x C-b" . consult-buffer)
         ("C-x l" . consult-goto-line)
         ("C-s" . consult-line)
         ("C-c s" . consult-ripgrep))
  ;; :custom ((consult-find-command . "fd --color=never --full-path ARG OPTS"))
  :config
  (leaf consult-ghq
    :doc "ghq interface"
    :if (executable-find "ghq")
    :ensure t
    :bind (("s-g s-g" . consult-ghq-find))
    :custom ((consult-ghq-find-function . 'dired)))
  (leaf consult-flycheck
    :ensure t)
  (leaf consult-lsp
    :ensure t)
  (leaf embark-consult
    :ensure t))
(leaf embark
  :ensure t
  :bind ("M-s" . embark-act))

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

(leaf hlinum
  :doc "Extension for linum.el to highlight current line number"
  :ensure t
  :init (hlinum-activate))

(leaf highlight-indent-guides
  :ensure t
  :custom
  ((highlight-indent-guides-method . 'column)
   (highlight-indent-guides-auto-enabled . t)
   (highlight-indent-guides-responsive . t))
  :hook (prog-mode-hook . highlight-indent-guides-mode)
  )

(leaf which-key
  :doc "minor mode for Emacs that displays the key bindings following your currently entered incomplete command (a prefix) in a popup."
  :ensure t
  :init (which-key-mode))

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :defun (flycheck-may-check-automatically)
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :config
  (flycheck-may-check-automatically '(idle-change mode-enabled new-line save))
  :global-minor-mode global-flycheck-mode)

(leaf fish-mode
  :doc "Emacs major mode for fish shell scripts."
  :ensure t
  :custom
  ((fish-indent-offset . 2)))

;;;
;; Programming
;;;

(leaf yasnippet
  :ensure t
  :custom
  (yas-snippet-dirs . '("~/.emacs.d/snippets")))

(leaf eglot
  :ensure t
  :doc "Emacs Polyglot: an Emacs LSP client that stays out of your way"
  :require t
  :defvar (eglot-server-programs)
  :config
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  ;; (add-hook 'ruby-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure))

(leaf lsp-mode
  :ensure t
  :init (yas-global-mode)
  :hook ((rustic-mode . lsp-deferred)
         (ruby-mode-hook . lsp-deferred)
         (typescript-mode-hook . lsp-deferred)
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
   ;; (when (not (eq major-mode 'tsx-mode))
   ;;   (lsp-semantic-tokens-enable . t)
   ;;   (lsp-semantic-tokens-honor-refresh-requests . t))
   ;; (lsp-idle-delay . 0.1)
   ;; ruby
   (lsp-solargraph-use-bundler . t)
   (lsp-solargraph-library-directories . '("~/.rbenv/shims/"))
   ;; rust
   (lsp-rust-server . 'rust-analyzer)
   (lsp-rust-analyzer-cargo-watch-command . "clippy")
   (lsp-rust-analyzer-cargo-load-out-dirs-from-check . t)
   (lsp-rust-analyzer-proc-macro-enable . t)
   (lsp-rust-analyzer-server-display-inlay-hints . t)
   ;; typescript/js
   (lsp-eslint-enable . t) ;; requires run lsp-install-server
   (lsp-eslint-autofix-on-save . t) ;; does not worked https://github.com/emacs-lsp/lsp-mode/issues/1842
   ;; terraform
   (lsp-disabled-clients . '(tfls)) ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-terraform-ls/#server-note
   (lsp-terraform-ls-enable-show-reference . t)
   (lsp-enable-links . t)
   (lsp-terraform-ls-prefill-required-fields . t)
   )
  :config
  ;; to fix `json-parse-error \u0000 is not allowed without JSON_ALLOW_NUL`
  ;; ref:
  ;; https://github.com/emacs-lsp/lsp-mode/issues/2681
  ;; https://github.com/typescript-language-server/typescript-language-server/issues/559#issuecomment-1259470791
  (leaf *avoid-null-on-parse-json-advice
    :config
    ;; same definition as mentioned earlier
    (advice-add 'json-parse-string :around
                (lambda (orig string &rest rest)
                  (apply orig (replace-match "\\u0000" "" string)
                         rest)))

    ;; minor changes: saves excursion and uses search-forward instead of re-search-forward
    (advice-add 'json-parse-buffer :around
                (lambda (oldfn &rest args)
	                (save-excursion
                    (while (search-forward "\\u0000" nil t)
                      (replace-match "" nil t)))
		              (apply oldfn args)))
    )
)

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

(leaf magit
  :ensure t)

(leaf quickrun
  :ensure t
  :config
  (quickrun-add-command "kotlin"
    '((:command . "kotlin")
      (:exec    . ("kotlinc %o %s" "%c %NKt %a"))
      (:remove  . ("%nKt.class"))
      (:tempfile . nil)
      (:description . "Compile Kotlin file and execute")
      )
    :mode 'kotlin-mode))

(leaf yaml-mode
  :ensure t
  :mode "\\(\.yml\\|\.yaml\\)")

(leaf ruby-mode
  :mode
  (("\\.rb$" . ruby-mode)
	 ("Gemfile$" . ruby-mode)
	 ("Capfile$" . ruby-mode)
	 ("Guardfile$" . ruby-mode)
	 ("[Rr]akefile$" . ruby-mode))
  :hook (electric-pair-mode rubocop-mode eldoc-mode)
  :config
  (leaf ruby-electric
    :ensure t
    :hook (ruby-mode-hook . ruby-electric-mode))
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
    :hook (ruby-mode-hook . yard-mode))
  :custom
  (flycheck-disabled-checkers . '(ruby-rubylint ruby-reek))
  (ruby-insert-encoding-magic-comment . nil)
  (flycheck-checker 'ruby))

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

(leaf rustic
  :ensure t
  :defvar (flycheck-checkers)
  :defun (rust-format-region)
  :bind
  (rustic-mode-map
   ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
   ("C-c C-c t" . rustic-cargo-test))
  :custom ((rustic-format-on-save . t))
  :config
  (push 'rustic-clippy flycheck-checkers))

(leaf slim-mode
  :ensure t
  :doc "slim-mode provides Emacs support for editing Slim templates. It's based on haml-mode.")

(leaf js2-mode
  :ensure t
  :mode
  (("\\.js$" . js2-mode))
  :config
  (add-hook 'js2-mode-hook (lambda()
                             ;; (tern-mode t)
                             (flycheck-mode t)))
  ;; (leaf tern
  ;;   :ensure t
  ;;   :custom
  ;;   (eval-after-load 'tern
  ;;     '(progn
  ;;        (require 'tern-auto-complete)
  ;;        (tern-ac-setup))))
  :custom
  ((js2-basic-offset . 2)))


(leaf kotlin-mode
  :ensure t)

(leaf graphql-mode :ensure t)

;; - ref: Emacs 29 でTree-sitterを利用してシンタックスハイライトする
;;   https://zenn.dev/hyakt/articles/42a1d237cdfa2a
(leaf treesit
  :custom ((treesit-font-lock-level . 4)))

(leaf treesit-auto
  :ensure t
  :url "https://github.com/renzmann/treesit-auto"
  :global-minor-mode global-treesit-auto-mode
  :custom ((treesit-auto-install . t)
           ;; https://github.com/renzmann/treesit-auto#keep-track-of-your-hooks
           (ruby-ts-mode-hook . ruby-mode-hook)
           (typescript-ts-mode-hook . typescript-mode-hook))
  :config (global-treesit-auto-mode))

(leaf typescript-mode
  :ensure t
  :mode (("\\.ts\\'" . typescript-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :custom ((typescript-indent-level . 2)
           (typescript-tsx-indent-offset . 2))
  :hook (typescript-mode-hook . subword-mode)
  :config (add-hook 'before-save-hook 'lsp-eslint-apply-all-fixes))

(leaf scss-mode
  :ensure t
  :custom ((flycheck-checker . 'scss-stylelint)))


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
            (py-python-command . ,(if (executable-find "python3") "python3"
                                    "python")))
  :hook (python-mode-hook . my/python-basic-config)
  :config
  (leaf python-isort :ensure t)
  (leaf blacken
    :ensure t
    :custom ((blacken-line-length . 91)
             (blacken-skip-string-normalization . t))))


(provide 'init)

