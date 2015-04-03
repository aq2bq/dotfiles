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


;; -------------------
;; General Settings
;; -------------------

;; set tab width
(setq tab-width 2)

;; encoding
(set-language-environment       "Japanese")
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


;; --------------------------
;; Bundle Packages
;; --------------------------

;; Emacs
(el-get-bundle k1LoW/emacs-drill-instructor)
(el-get-bundle auto-complete)
(el-get-bundle minibuf-isearch)
(el-get-bundle iswitchb-highlight)
(el-get-bundle anything)

;; Ruby
(el-get-bundle ruby-mode)
(el-get-bundle ruby-block)
(el-get-bundle ruby-electric)
(el-get-bundle rspec-mode)
(el-get-bundle rails-el)




;; ------------
;; Key Bindings
;; ------------

;; Drill Instructor!!
(require 'drill-instructor)
(drill-instructor t)
(setq drill-instructor-global t)

;; Backspace
(define-key global-map "\C-h" 'delete-backward-char)

;; Toggle comment/uncomment
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)

;; windmove.el
(windmove-default-keybindings 'meta)



