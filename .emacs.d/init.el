;;;
;;; Emacs Client
;;;
;; run server
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))



;; -------------------------------------
;; Initialize El-Get: package management
;; -------------------------------------

(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; Auto load configures init-/packagename/.el
(setq el-get-user-package-directory (locate-user-emacs-file "init"))

;; -------------------
;; General Settings
;; -------------------

;; set tab width
(setq tab-width 2)

;; encoding
(set-language-environment       "Japanese")
(setq file-name-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(prefer-coding-system           'utf-8-unix)
(setq                           default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system  'utf-8)
(set-terminal-coding-system     'utf-8)
(set-keyboard-coding-system     'utf-8)
(set-clipboard-coding-system    'utf-8)

;; disable generate backupfile
(setq make-backup-files nil)
(setq auto-save-default nil)

;; copy mouse drag region
(setq mouse-drag-copy-region t)


;; for support `ls --dired`
(let ((gls "/usr/local/bin/gls"))
  (if (file-exists-p gls) (setq insert-directory-program gls)))

;; --------------------------
;; Bundle Packages
;; --------------------------

;; Emacs
(el-get-bundle! use-package)
(el-get-bundle! bind-key)
(el-get-bundle! powerline)
(el-get-bundle k1LoW/emacs-drill-instructor)
(el-get-bundle auto-complete)
;; (el-get-bundle minibuf-isearch)
;; (el-get-bundle iswitchb-highlight)
(el-get-bundle hlinum)

(el-get-bundle smart-compile)
(el-get-bundle anzu)
(el-get-bundle flycheck)
(el-get-bundle flycheck-color-mode-line)

;; Markdown
(el-get-bundle markdown-mode)

;; Helm
(el-get-bundle helm)
(el-get-bundle migemo)	
(el-get-bundle helm-migemo)
(el-get-bundle helm-swoop)
(el-get-bundle helm-descbinds)
(el-get-bundle helm-ag)

;; Git
(el-get-bundle magit)

;; Ruby / Rails
(el-get-bundle zenspider/enhanced-ruby-mode)
(el-get-bundle ruby-block)
(el-get-bundle ruby-electric)
(el-get-bundle inf-ruby)
(el-get-bundle rcodetools)
(el-get-bundle rubocop)
(el-get-bundle robe-mode)
(el-get-bundle helm-robe)
(el-get-bundle rspec-mode)
(el-get-bundle rails-el)
(el-get-bundle yaml-mode)
(el-get-bundle masutaka/emacs-helm-bundle-show)


;; JS / JSX / Coffee Script
(el-get-bundle js2-mode)
(el-get-bundle coffee-mode)
(el-get-bundle web-mode)

;; Slim
(el-get-bundle slim-mode)


;; -----------
;; Appearance
;; -----------

;; Highlight paren
(show-paren-mode 1)

;; Display line number(& hilight current line number)
(global-linum-mode t)
(setq linum-format "%4d|\s")
(hlinum-activate)


;; ------------
;; Key Bindings
;; ------------

(require 'bind-key)

;; Drill Instructor!!
(require 'drill-instructor)
(drill-instructor t)
(setq drill-instructor-global t)

;; Suspend EmacsClient
;; (bind-key "C-x C-c" 'ns-do-hide-emacs)

;; Exit EmacsClient
;; (defalias 'exit 'save-buffers-kill-emacs)

;; Backspace C-h
;; (bind-key "C-h" 'delete-backward-char)
(global-set-key (kbd "C-h") 'delete-backward-char)
;; Toggle comment/uncomment
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

;; Move to other window C-q
(defun other-window-or-split (val)
  (interactive)
  (when (one-window-p)
					;    (split-window-horizontally) ;split horizontally
    (split-window-vertically) ;split vertically
    )
  (other-window val))

(global-set-key (kbd "C-q") (lambda () (interactive) (other-window-or-split 1)))
(global-set-key (kbd "C-S-q") (lambda () (interactive) (other-window-or-split -1)))

;; Change buffer next/before C-c n/C-c b
(global-set-key (kbd "C-c n") 'next-buffer)
(global-set-key (kbd "C-c b") 'previous-buffer)

;; Goto line C-x l
(define-key ctl-x-map "l" 'goto-line)

;; Incremental seach from buffer C-f
(icomplete-mode 1)

;; -----------
;; Mouse
;; -----------

;; Enable copy mouse drag region
(setq mouse-drag-copy-region t)



