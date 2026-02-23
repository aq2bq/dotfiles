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
