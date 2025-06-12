;; 括弧を自動で補完する
(electric-pair-mode 1)

;; Highlight paren
(show-paren-mode 1)

;; replace inputting yes-no to y-n
(fset 'yes-or-no-p 'y-or-n-p)

(leaf flymake
  :ensure t
  :bind (flymake-mode-map
         ("C-x C-p" . flymake-goto-prev-error)
         ("C-x C-n" . flymake-goto-next-error))
  :custom ((flymake-no-changes-timeout . 5)  ;; チェックの頻度を減らす場合は値(秒)を増やす
           (flymake-proc-legacy-flymake . t) ;; エラーメッセージのポップアップ
           (flymake-start-syntax-check-on-newline . t) ;; 行追加するごとにチェックするか
           (flymake-start-syntax-check-on-find-file . t) ;; ファイルを開いたときにチェックするか
           (flymake-start-on-save-buffer . t)  ;; 保存時のみチェック
           (flymake-diagnostic-functions . '(flymake-proc-legacy-flymake)))
  :config
  (set-face-foreground 'flymake-errline "white")
  (set-face-background 'flymake-errline "red4")
  (set-face-foreground 'flymake-warnline "white")
  (set-face-background 'flymake-warnline "goldenrod3"))

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom (show-paren-style . 'mixed)
  :config
  (show-paren-mode t))
(leaf smartparens
  :ensure t
  :global-minor-mode show-smartparens-global-mode
  :config
  (require 'smartparens-ruby))

(leaf posframe
  :ensure t
  :url "https://github.com/tumashu/posframe"
  :doc "A library for creating child frames in Emacs")

(leaf rainbow-delimiters
  :url "https://github.com/Fanael/rainbow-delimiters"
  :ensure t
  :hook ((prog-mode-hook . rainbow-delimiters-mode)))


(leaf topsy
  :doc "simple alternative to `semantic-stickyfunc-mode`"
  :ensure t
  :hook (prog-mode-hook . topsy-mode))

(leaf rotate
  :doc "Rotate the current buffer to the next or previous buffer in the current window"
  :url "https://github.com/daichirata/emacs-rotate"
  :ensure t
  :bind (
         ("M-q" . rotate-window)
         ("M-r" . rotate-layout)
         ;; ("C-x <right>" . rotate-window-right)
         ;; ("C-x <left>" . rotate-window-left)
         ))

(leaf yafolding
  :doc "Folding code blocks based on indentation."
  :url "https://github.com/zenozeng/yafolding.el"
  :ensure t
  :bind ("C-c i" . yafolding-toggle-element))

(leaf which-key
  :doc "minor mode for Emacs that displays the key bindings following your currently entered incomplete command (a prefix) in a popup."
  :ensure t
  :init (which-key-mode))

(leaf highlight-indent-guides
  :url "https://github.com/DarthFennec/highlight-indent-guides"
  :ensure t
  :custom
  ((highlight-indent-guides-method . 'fill)
   (highlight-indent-guides-auto-enabled . t)
   (highlight-indent-guides-responsive . t))
    :hook (((prog-mode-hook yaml-mode-hook) . highlight-indent-guides-mode)))

(leaf keypression
  :ensure t
  :doc "Keystroke visualizer for Emacs"
  :url "https://github.com/chuntaro/emacs-keypression"
  :custom (
           (keypression-use-child-frame . nil)
           (keypression-fade-out-delay . 1)
           (keypression-frame-justify . 'keypression-right-justified)
           (keypression-frame-background-mode . t)
           (keypression-cast-command-name . t)
           (keypression-combine-same-keystrokes . t)
           ))

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

(leaf nyan-mode
  :doc "analog indicator of your position in the buffer"
  :ensure t
  :url "https://github.com/TeMPOraL/nyan-mode"
  :init (nyan-mode)
  :custom ((nyan-animate-nyancat . t)
           (nyan-wavy-trail . t)))
