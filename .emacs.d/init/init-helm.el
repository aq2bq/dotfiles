;; Helm
(helm-mode 1)
(require 'use-package)
(use-package bind-key)
(bind-key "M-x" 'helm-M-x)
(bind-key "C-x C-f" 'helm-find-files)
(bind-key "C-x f" 'helm-find)
(bind-key "C-x C-r" 'helm-recentf)
(bind-key "C-x b" 'helm-buffers-list)


;; Force define Backspace C-h
(bind-key "C-h" 'delete-backward-char helm-map)
(bind-key "C-h" 'delete-backward-char helm-find-files-map)

;; Enable completions on minibuffer TAB
(bind-key "TAB" 'helm-execute-persistent-action helm-read-file-map)
(bind-key "TAB" 'helm-execute-persistent-action helm-find-files-map)
