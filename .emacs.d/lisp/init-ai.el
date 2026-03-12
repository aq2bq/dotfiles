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

(defun my/copilot-chat-prefer-chep ()
  ;; 2026-03:
  ;; copilot.el が同名ライブラリ `copilot-chat.el` を同梱し始めたため、
  ;; chep/copilot-chat.el と名前衝突する。commit 生成系は chep 側にあるので、
  ;; `copilot-chat-*` ディレクトリを先頭に寄せて解決順を固定する。
  "Put newest chep/copilot-chat package directory first in `load-path`."
  (let* ((elpa-dir (and (boundp 'package-user-dir) package-user-dir))
         (pattern (and elpa-dir (expand-file-name "copilot-chat-[0-9]*" elpa-dir)))
         (candidates (if pattern (file-expand-wildcards pattern t) nil))
         (target (car (sort candidates #'string>))))
    (when (and target (file-directory-p target))
      (setq load-path (cons target (delete target load-path))))))

(defun my/copilot-chat-load-chep ()
  ;; `copilot-chat-git` だけを先に読むと `copilot-chat-backend` が未初期化になりうる。
  ;; 先にトップレベル `copilot-chat` を明示ロードして必要変数を初期化する。
  "Load chep/copilot-chat top-level and git module."
  (my/copilot-chat-prefer-chep)
  (if (require 'copilot-chat nil t)
      (require 'copilot-chat-git nil t)
    nil))

(defun my/copilot-chat-insert-commit-message-safe ()
  ;; `git-commit-setup-hook` で直接 autoload を踏むと衝突側を引くことがある。
  ;; chep 側トップレベルを初期化してから呼び出し、失敗は明示エラーにする。
  "Insert commit message via chep/copilot-chat if available."
  (if (my/copilot-chat-load-chep)
      (if (fboundp 'copilot-chat-insert-commit-message)
          (copilot-chat-insert-commit-message)
        (user-error
         "[copilot-chat] `copilot-chat-insert-commit-message` is unavailable"))
    (user-error "[copilot-chat] failed to load `copilot-chat` modules")))

(leaf copilot-chat
  :ensure t
  :url "https://github.com/chep/copilot-chat.el"
  :init
  ;; 通常の `M-x copilot-chat*` も含め、名前解決を常に chep 側へ寄せる。
  (my/copilot-chat-prefer-chep)
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
           (copilot-chat-prompt-review . "/review 以下のコードをレビューして:\n"))
  ;; 既存のコミット自動挿入の操作感は維持しつつ、衝突だけを回避する。
  :hook (git-commit-setup-hook . my/copilot-chat-insert-commit-message-safe))


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
