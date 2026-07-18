;;; init-ai.el --- AI関連設定 -*- lexical-binding: t; -*-

(defvar my/gptel-commit-instructions-file
  (expand-file-name "git-commit-instructions.md"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "コミットメッセージ生成の指示ファイル。")

(defvar my/gptel-commit-max-diff-chars 30000)

(defun my/gptel-commit--buffer-empty-p ()
  "コミットバッファの編集領域(コメント行より前)が空なら non-nil。"
  (save-excursion
    (goto-char (point-min))
    (let ((end (if (re-search-forward "^#" nil t)
                   (match-beginning 0)
                 (point-max))))
      (string-empty-p
       (string-trim (buffer-substring-no-properties (point-min) end))))))

(defvar-local my/gptel-commit--overlay nil)

(defun my/gptel-commit--show-progress ()
  "バッファ先頭に生成中インジケーターを表示する。"
  (my/gptel-commit--clear-progress (current-buffer))
  (let ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'after-string
                 (propertize "⏳ コミットメッセージを生成中...\n" 'face 'shadow))
    (setq my/gptel-commit--overlay ov)))

(defun my/gptel-commit--clear-progress (buf)
  "BUF の生成中インジケーターを消す。"
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when my/gptel-commit--overlay
        (delete-overlay my/gptel-commit--overlay)
        (setq my/gptel-commit--overlay nil)))))

(defun my/gptel-commit--staged-diff ()
  "ステージ済み差分を返す。空なら nil。"
  (let ((diff (shell-command-to-string
               "git diff --cached --no-ext-diff --find-renames --stat --patch")))
    (unless (string-empty-p (string-trim diff))
      (if (> (length diff) my/gptel-commit-max-diff-chars)
          (concat (substring diff 0 my/gptel-commit-max-diff-chars)
                  "\n\n[差分が長いためここで省略]")
        diff))))

(defun my/gptel-insert-commit-message ()
  "空のコミットバッファに生成したメッセージを非同期で挿入する。"
  (interactive)
  (require 'gptel)
  (when-let* (((my/gptel-commit--buffer-empty-p))
              (diff (my/gptel-commit--staged-diff))
              (buf (current-buffer)))
    (my/gptel-commit--show-progress)
    (gptel-request
        (concat "次のステージ済み差分からコミットメッセージを作成してください。\n\n" diff)
      :system (with-temp-buffer
                (insert-file-contents my/gptel-commit-instructions-file)
                (string-trim (buffer-string)))
      :callback
      (lambda (response info)
        ;; reasoning等のcons応答は無視し、文字列(本文)とnil(失敗)だけ扱う
        (cond
         ((null response)
          (my/gptel-commit--clear-progress buf)
          (message "コミットメッセージ生成に失敗しました: %s"
                   (plist-get info :status)))
         ((and (stringp response) (buffer-live-p buf))
          (my/gptel-commit--clear-progress buf)
          (with-current-buffer buf
            ;; 生成待ちの間にユーザーが書き始めていたら挿入しない
            (when (my/gptel-commit--buffer-empty-p)
              (save-excursion
                (goto-char (point-min))
                (insert (string-trim response) "\n\n"))
              (message "コミットメッセージを挿入しました")))))))))

(leaf gptel
  :ensure t
  :url "https://github.com/karthink/gptel"
  :hook ((git-commit-setup-hook . my/gptel-insert-commit-message))
  :config
  (setq gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream nil
                        ;; thinkingを切らないと生成に10秒以上かかる
                        :request-params '(:think :json-false :keep_alive "30m")
                        :models '(gemma4:e4b-mlx))
        gptel-model 'gemma4:e4b-mlx))


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
