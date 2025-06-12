(load "init-looks-theme")

;; Highlight current line
(global-hl-line-mode t)
;; メニューバーの非表示
(menu-bar-mode 0)
;; ツールバー(GUI)
(tool-bar-mode 0)

;; ウィンドウの境界線を表示
(setq window-divider-mode t)

;; highlight a blank-space of end of line
(setq-default show-trailing-whitespace t)

;; title bar setting
(when (window-system)
  (setq frame-title-format
        '(:eval (format "%s - %s - %s"
                        (format-time-string "%H:%M")
                        (format "%s" major-mode)
                        (buffer-name)))))

(when (display-graphic-p)
  ;; GUIモード時の透明度設定
  (set-frame-parameter (selected-frame) 'alpha '(85 . 85))
  (add-to-list 'default-frame-alist '(alpha . (85 . 85)))

  (setq use-default-font-for-symbols nil)

  ;; フォント設定
  (let ((size (if (>= (x-display-pixel-width) 4000) 130 120)))
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


(leaf all-the-icons
  ;; require `M-x all-the-icons-install-fonts`
  :doc "A utility package to collect various Icon Fonts and propertize them within Emacs."
  :ensure t
  :if (display-graphic-p)
  :custom (
           (all-the-icons-color-icons . t)
           ))
(leaf all-the-icons-nerd-fonts
  :ensure t ;; requires: `brew install font-hack-nerd-font`
  :if (display-graphic-p)
  :custom ((all-the-icons-nerd-fonts-scale-factor . 1.0))
  :config
  (all-the-icons-nerd-fonts-prefer))
(leaf all-the-icons-dired
  :ensure t
  :if (display-graphic-p)
  :hook (dired-mode-hook . all-the-icons-dired-mode))
(leaf all-the-icons-ibuffer
  :ensure t
  :if (display-graphic-p)
  :hook (ibuffer-mode-hook . all-the-icons-ibuffer-mode))
(leaf all-the-icons-completion
  :ensure t
  :if (display-graphic-p)
  :hook (marginalia-mode-hook . all-the-icons-completion-marginalia-setup)
  :init (all-the-icons-completion-mode))
(leaf kind-icon
  :ensure t
  :after corfu
  :custom
  ((kind-icon-default-face . 'corfu-default))
  :config (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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
(leaf dashboard
  :ensure t
  :doc "An extensible emacs startup screen showing you what’s most important."
  :url "https://github.com/emacs-dashboard/emacs-dashboard"
  :if (display-graphic-p)
  :init (dashboard-setup-startup-hook)
  :custom (
           (dashboard-vertically-center-content . t)
           (dashboard-banner-logo-title . "明日できることは明日やる")
           (dashboard-startup-banner . 'logo) ;; logo, or a file path
           (dashboard-items . '((recents  . 10)
                                (bookmarks . 5)
                                (projects . 5)
                                (agenda . 5)))
           (dashboard-set-heading-icons . t)
           (dashboard-set-file-icons . t)
           (dashboard-display-icons-p . t)
           (dashboard-icon-type . 'nerd-icons)
           (dashboard-center-content . t)))
