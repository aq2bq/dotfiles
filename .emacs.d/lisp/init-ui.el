;;; init-ui.el --- UI settings -*- lexical-binding: t; -*-

;; 括弧を自動で補完する
(electric-pair-mode 1)

;; replace inputting yes-no to y-n
(fset 'yes-or-no-p 'y-or-n-p)

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom (show-paren-style . 'mixed)
  :config
  (show-paren-mode t))
(leaf posframe
  :ensure t
  :url "https://github.com/tumashu/posframe"
  :doc "A library for creating child frames in Emacs")

(leaf rainbow-delimiters
  :url "https://github.com/Fanael/rainbow-delimiters"
  :ensure t
  :hook ((prog-mode-hook . rainbow-delimiters-mode)))

(leaf window
  :tag "builtin"
  :bind (("M-q" . rotate-windows)
         ;; 上下分割と左右分割を切り替える。
         ("M-r" . window-layout-transpose)))

(leaf hideshow
  :tag "builtin"
  :preface
  (defun my/toggle-code-fold ()
    "標準のHideshowで現在のブロックを開閉する。"
    (interactive)
    (require 'hideshow)
    (unless hs-minor-mode
      ;; 構文解析のないモードでは従来どおりインデントで判定する。
      (unless (treesit-parser-list)
        (hs-indentation-mode 1))
      (hs-minor-mode 1))
    (hs-toggle-hiding))
  :bind ("C-c i" . my/toggle-code-fold))

(leaf which-key
  :doc "minor mode for Emacs that displays the key bindings following your currently entered incomplete command (a prefix) in a popup."
  :tag "builtin"
  :init (which-key-mode 1))

(leaf highlight-indent-guides
  :url "https://github.com/DarthFennec/highlight-indent-guides"
  :ensure t
  :custom
  ((highlight-indent-guides-method . 'fill)
   (highlight-indent-guides-auto-enabled . t)
   (highlight-indent-guides-responsive . t))
    :hook (((prog-mode-hook yaml-ts-mode-hook) . highlight-indent-guides-mode)))

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
