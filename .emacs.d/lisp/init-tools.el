;;; init-tools.el --- Tool settings -*- lexical-binding: t; -*-

(leaf flymake
  :tag "builtin"
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

(leaf ghostel
  :doc "Terminal emulator powered by libghostty"
  :url "https://github.com/dakra/ghostel"
  :ensure t
  :commands (ghostel ghostel-project)
  :preface
  (defun my/ghostel-toggle ()
    "Toggle the current project's terminal and the editing buffer."
    (interactive)
    (if (derived-mode-p 'ghostel-mode)
        (quit-window)
      (require 'project)
      (if (project-current nil)
          (ghostel-project)
        (ghostel))))
  :bind (("C-s-t" . my/ghostel-toggle)
         (ghostel-semi-char-mode-map
          ("C-h" . (lambda () (interactive) (ghostel-send-key "backspace")))
          ("C-g" . (lambda ()
                     (interactive)
                     (setq quit-flag nil)
                     (ghostel-send-key "escape")))
          ("C-l" . ghostel-clear)
          ("C-c C-c" . ghostel-send-C-c)
          ("C-q" . other-window)
          ("C-s-t" . my/ghostel-toggle)))
  :hook (ghostel-mode-hook . (lambda () (display-line-numbers-mode -1)))
  ;; 8 MiB: Ghostel measures scrollback in bytes, rather than lines.
  :custom (ghostel-max-scrollback . 8388608))

(leaf mindstream
  :doc "quickly write down some thoughts"
  :url "https://github.com/countvajhula/mindstream"
  :ensure t
  :config (mindstream-mode))
