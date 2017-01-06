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


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

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

;; replace inputting yes-no to y-n
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight a blank-space of end of line
(setq-default show-trailing-whitespace t)

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
(setenv "LC_ALL" "ja_JP.UTF-8")

;; disable generate backupfile
(setq make-backup-files nil)
(setq auto-save-default nil)

;; copy mouse drag region
(setq mouse-drag-copy-region t)


;; for support `ls --dired`
(let ((gls "/usr/local/bin/gls"))
  (if (file-exists-p gls) (setq insert-directory-program gls)))

;; enable paste to region
(delete-selection-mode t)



;; --------------------------
;; Bundle Packages
;; --------------------------

;; Emacs
(el-get-bundle! use-package)
					; (el-get-bundle! bind-key)
(el-get-bundle! let-alist)
(el-get-bundle! powerline)
(el-get-bundle k1LoW/emacs-drill-instructor)
(el-get-bundle auto-complete)
(el-get-bundle! company
  :type github :pkgname "company-mode/company-mode"
  (global-company-mode +1))
(el-get-bundle yasnippet)
;; (el-get-bundle minibuf-isearch)
;; (el-get-bundle iswitchb-highlight)
(el-get-bundle hlinum)

(el-get-bundle smart-compile)
(el-get-bundle anzu)
(el-get-bundle seq)
(el-get-bundle flycheck/flycheck)
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
(el-get-bundle ruby-mode)
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
(el-get-bundle! cl-lib)
(el-get-bundle mooz/js2-mode)
(el-get-bundle coffee-mode)
(el-get-bundle web-mode)
(el-get-bundle tern) ;; require `npm install -g tern`
(el-get-bundle company-tern)
(el-get-bundle js2-refactor)
(el-get-bundle json-mode)

;; SASS
(el-get-bundle nex3/sass-mode)
(el-get-bundle flymake-sass)
(el-get-bundle haml-mode)

;; HTML
(el-get-bundle ac-html)
(el-get-bundle html5)

;; Slim
(el-get-bundle slim-mode)

;; Elixir
(el-get-bundle elixir) ;; elixir-mode
(el-get-bundle alchemist)
(el-get-bundle syohex/emacs-ac-alchemist)

;; Lisp
(el-get-bundle ac-slime)
(el-get-bundle slime/slime)
(load (expand-file-name "~/.roswell/helper.el"))
(setq inferior-lisp-program "ros -Q run")
(setf slime-lisp-implementations
      `((sbcl    ("sbcl" "--dynamic-space-size" "2000"))
	(roswell ("ros" "-Q" "run"))))
(setf slime-default-lisp 'roswell)


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



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-search-threshold 1000)
 '(anzu-use-migemo t)
 '(robe-completing-read-func (quote helm-robe-completing-read))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
