(custom-set-faces
 '(default ((t (:background "#13213A"))))
 ;; vtermなどでTUIを利用すると空白領域に色がついて辛い
 '(whitespace-trailing ((t (:background nil))))
 '(trailing-whitespace ((t (:background nil))))

 ;; コメント系(暗すぎると読めない)
 '(font-lock-comment-face ((t (:foreground "gray"))))
 '(font-lock-doc-face ((t (:foreground "#FFCC00"))))
 )

(leaf doom-themes
  :ensure t
  :url "https://github.com/doomemacs/themes"
  :config
  ;; POP系
  ;; (load-theme 'doom-dracula t nil)
  ;; (load-theme 'doom-outrun-electric t nil)
  ;; 紫系
  (load-theme 'doom-laserwave t nil)
  ;; (load-theme 'doom-challenger-deep t nil)
  ;; (load-theme 'doom-shades-of-purple t nil)
  ;; 暗い系
  ;; (load-theme 'doom-horizon t nil)
  ;; 青系
  ;; (load-theme 'doom-ephemeral t nil)
  ;; (load-theme 'doom-nova t nil)
  ;; (load-theme 'doom-moonlight t nil)
  ;; (load-theme 'doom-palenight t nil)
  ;; (load-theme 'doom-city-lights t nil)
  ;; (load-theme 'doom-oksolar-dark t nil)
  ;; 緑系
  ;; (load-theme 'doom-solarized-dark t nil)
  ;; (load-theme 'doom-material t nil))

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
  (doom-challenger-deep-brighter-modeline . t)
  (doom-ephemeral-brighter-comments . t)
  (doom-ephemeral-brighter-modeline . t)
  )

(leaf ef-themes
  :ensure t
  :url "https://github.com/protesilaos/ef-themes"
  :config
  ;; (ef-themes-select 'ef-deuteranopia-dark)
  ;; (ef-themes-select 'ef-dream)
  ;; (ef-themes-select 'ef-elea-dark)
  ;; (ef-themes-select 'ef-trio-dark)
  ;; (ef-themes-select 'ef-tritanopia-dark)
  )

(leaf rebecca-theme
  :doc "The future was cooler in the 80's."
  :url "https://github.com/vic/rebecca-theme"
  :ensure t
  :config
  ;; (load-theme 'rebecca t nil)
  )
