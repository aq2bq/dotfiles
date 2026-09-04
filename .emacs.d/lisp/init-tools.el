(leaf flymake
  :ensure t
  :bind (flymake-mode-map
         ("C-x C-p" . flymake-goto-prev-error)
         ("C-x C-n" . flymake-goto-next-error))
  :custom ((flymake-no-changes-timeout . 5)  ;; チェックの頻度を減らす場合は値(秒)を増やす
           (flymake-start-on-save-buffer . t))  ;; 保存時にもチェック
  :config
  (set-face-foreground 'flymake-error "white")
  (set-face-background 'flymake-error "red4")
  (set-face-foreground 'flymake-warning "white")
  (set-face-background 'flymake-warning "goldenrod3"))

(leaf magit
  :ensure t)

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
  :custom (
           (display-line-numbers . nil)
           (vterm-max-scrollback . 10000)))
(leaf vterm-toggle
  :doc "toggles between the vterm buffer and whatever buffer you are editing."
  :ensure t
  :custom (vterm-toggle-scope . 'project)
  :bind (("C-s-t" . vterm-toggle)))

(leaf mindstream
  :doc "quickly write down some thoughts"
  :url "https://github.com/countvajhula/mindstream"
  :ensure t
  :config (mindstream-mode))
