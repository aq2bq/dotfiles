;; Emacs for Mac
;; brew install --cask emacs-mac-spacemacs-icon

;;;
;; Global key settings
;;;

(defun my/other-window-or-split (val)
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window val))

(defun my/other-frame-or-create ()
  (interactive)
  (if (> (length (frame-list)) 1)
      (other-frame 1)
    (make-frame)))

(defun my/copy-buffer-relative-path ()
  (interactive)
  (let* ((filename (buffer-file-name))
         (project (project-current))
         (project-root (when project (expand-file-name (project-root project))))
         (relpath (when (and filename project-root)
                    (file-relative-name filename project-root))))
    (cond
     ((not filename)
      (message "not file buffer"))
     ((not project-root)
      (message "not found project root"))
     (t
      (kill-new relpath)
      (message "kill-new：%s" relpath)))))

(add-hook
 'kill-emacs-query-functions
 (lambda ()
   (y-or-n-p "Really exit Emacs? ")))

(global-set-key (kbd "C-c C-c c") 'my/copy-buffer-relative-path)

(global-set-key (kbd "C-q") (lambda () (interactive) (my/other-window-or-split 1)))
(global-set-key (kbd "C-S-q") 'my/other-frame-or-create)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c n") 'next-buffer)
(global-set-key (kbd "C-c b") 'previous-buffer)
;; (global-set-key (kbd "C-x l") 'goto-line)
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-x f") 'project-find-file)
(global-set-key (kbd "M-TAB") 'other-frame)
(defvar mac-command-modifier 'super)

;;;
;; General Settings
;;;

;; メタキーをaltに(GUI)
(when (display-graphic-p)
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)
  (global-set-key (kbd "s-c") 'kill-ring-save) ; コピー
  (global-set-key (kbd "s-x") 'kill-region)    ; カット
  (global-set-key (kbd "s-v") 'yank)           ; ペースト
  (global-set-key (kbd "s-z") 'undo)           ; 元に戻す
  (global-set-key (kbd "s-s") 'save-buffer)    ; 保存
  (global-set-key (kbd "s-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "C-c +") 'text-scale-increase)
  (global-set-key (kbd "C-c -") 'text-scale-decrease)
  (global-set-key (kbd "C-c 0") 'text-scale-adjust))


;; set tab width
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; encoding
(set-language-environment       "Japanese")
(setq file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(prefer-coding-system           'utf-8-unix)
(set-buffer-file-coding-system  'utf-8)
(set-terminal-coding-system     'utf-8)
(set-keyboard-coding-system     'utf-8)
(set-clipboard-coding-system    'utf-8)
(setenv "LC_ALL" "ja_JP.UTF-8")

;; disable generate backupfile
(setq make-backup-files nil)
(setq auto-save-default nil)

;; copy mouse drag region
(setq mouse-drag-copy-region t)

;; for support `ls --dired`
(let ((gls (substring (shell-command-to-string "which gls") 0 -1)))
  (if (file-exists-p gls) (setq insert-directory-program gls)))

;; enable paste to region
(delete-selection-mode t)

;; Enable copy mouse drag region
(setq mouse-drag-copy-region t)

;; use flash instead of beeping
(setq visible-bell t)

;; Again the emacs default is too low 4k considering that the some of the language server responses are in 800k - 3M range
;; https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
(setq read-process-output-max (* 1024 1024)) ;; 1MB

;; The default setting is too low for lsp-mode's needs due to the fact that
;; client/server communication generates a lot of memory/garbage.
;; https://emacs-lsp.github.io/lsp-mode/page/performance/#adjust-gc-cons-threshold
(setq gc-cons-threshold 100000000)


;;;
;; Package management
;;;

(require 'package)
(require 'bytecomp)

(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))
(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-convert :ensure t)
  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init))
)


;;;;
;; General packages configuration
(leaf exec-path-from-shell
  :doc "ensure environment variables inside Emacs look the same as in the user's shell"
  :ensure t
  :init (exec-path-from-shell-initialize))


(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))


(leaf save-hist
  :doc "Save minibuffer history"
  :tag "builtin"
  :custom ((savehist-mode . t)
           (history-length . 1000)
           (savehist-additional-variables . '(kill-ring search-ring regexp-search-ring))))
(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :global-minor-mode global-auto-revert-mode)


;;;;
;; Other utilities
(leaf anzu
  :doc "provides a minor mode which displays current match and total matches information in the mode-line in various search modes"
  :ensure t
  :bind
  (("C-c r" . anzu-query-replace)
   ("C-c RET" . anzu-query-replace-at-cursor-thing))
  :custom
  ((global-anzu-mode . t)
   (anzu-mode-lighter . "")
   ((anzu-deactivate-region . t)
    (anzu-deactivate-region . t)
    (anzu-search-threshold . 1000))))


;; - ref: Emacs 29 でTree-sitterを利用してシンタックスハイライトする
;;   https://zenn.dev/hyakt/articles/42a1d237cdfa2a
(leaf treesit
  :custom ((treesit-font-lock-level . 4)))

(leaf treesit-auto
  :ensure t
  :url "https://github.com/renzmann/treesit-auto"
  :global-minor-mode global-treesit-auto-mode
  :custom ((global-treesit-auto-modes . '((not ruby-mode)(not rust-mode)))
           (treesit-auto-install . t))
  :config
  (global-treesit-auto-mode))


;;;;
;; Load custom init files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(load "init-ui")
(load "init-looks")
(load "init-completion")
(load "init-langs")
(load "init-lsp")
(load "init-ai")
(load "init-tools")


(provide 'init)

