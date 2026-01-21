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
           (copilot-chat-default-model . "gpt-4.1")
           (copilot-chat-prompt-doc . "/doc 以下のコードについてドキュメントを書いて:\n")
           (copilot-chat-prompt-explain . "/explain 日本語で説明:\n")
           (copilot-chat-prompt-fix . "/fix 問題箇所を修正して、修正内容の解説して:\n")
           (copilot-chat-prompt-optimize . "/optimize パフォーマンスと可読性を向上させるため、以下のコードを最適化:\n")
           (copilot-chat-prompt-review . "/review 以下のコードをレビューして:\n")))


(add-to-list 'load-path "~/go/src/github.com/aq2bq/goose.el/")
(leaf goose
  :init
  (require 'goose)
  :bind (("C-c g" . goose-transient))
  :hook (goose-mode-hook . (lambda ()
                             (display-line-numbers-mode -1)))
  :custom ((goose-program-name .
                               ;; "GOOSE_LEAD_MODEL=gpt-4.1 GOOSE_MODEL=gpt-4.1 goose"
                               ;; "GOOSE_LEAD_MODEL=o4-mini GOOSE_MODEL=gpt-4.1 goose"
                               "goose"
                               )))
