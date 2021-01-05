;;;
;; Global key settings
;;;

;; Move to other window C-q
(defun other-window-or-split (val)
  (interactive)
  (when (one-window-p)
    (split-window-horizontally)
    (split-window-vertically))
  (other-window val))

(global-set-key (kbd "C-q") (lambda () (interactive) (other-window-or-split 1)))
(global-set-key (kbd "C-S-q") (lambda () (interactive) (other-window-or-split -1)))
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c n") 'next-buffer)
(global-set-key (kbd "C-c b") 'previous-buffer)
(global-set-key (kbd "C-x l") 'goto-line)
(global-set-key (kbd "C--") 'undo)
(global-set-key (kbd "C-x f") 'project-find-file)
(setq mac-command-modifier 'super)

;;;
;; General Settings
;;;

;; メニューバーの非表示
(menu-bar-mode -1)

;; ツールバーの非表示
(tool-bar-mode -1)

;; set tab width
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

;; 括弧を自動で補完する
(electric-pair-mode 1)

;; Highlight paren
(show-paren-mode 1)

;; replace inputting yes-no to y-n
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight a blank-space of end of line
(setq-default show-trailing-whitespace t)

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
(let ((gls "/usr/local/bin/gls"))
  (if (file-exists-p gls) (setq insert-directory-program gls)))

;; enable paste to region
(delete-selection-mode t)

;; Enable copy mouse drag region
(setq mouse-drag-copy-region t)

;;;
;; Initialize
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
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;;;
;; Emacs global extentions
;;;

(leaf srcery-theme
  :ensure t
  :config
  (load-theme 'srcery t))
;; (leaf madhat2r-theme
;;   :ensure t
;;   :config
;;   (load-theme 'madhat2r t))
;; (load-theme 'tango-dark t)

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ;; ("<tab>" . company-complete-selection)
          ("TAB" . company-select-next)
          ("<backtab>" . company-select-previous))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence))
           (company-selection-wrap-around . t))
  :global-minor-mode global-company-mode)

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom
  ((ivy-initial-inputs-alist . nil)
   (ivy-re-builders-alist . '((t . ivy--regex-fuzzy)
                              (swiper . ivy--regex-plus)
                              (counsel-ag . ivy--regex-plus)
                              (counsel-rg . ivy--regex-plus)))
   (ivy-use-selectable-prompt . t)
   (ivy-height . 30))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper)))

  (leaf ag
    :doc "Ag.el allows you to search using ag from inside Emacs. You can filter by file type, edit results inline, or find files."
    :ensure t)

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :blackout t
    :config
    (defun my-counsel-ag ()
      (interactive)
      (counsel-ag "" default-directory))
    :bind (("C-S-s" . counsel-imenu)
           ("C-c s" . my-counsel-ag)
           ("C-x C-r" . counsel-recentf)
           ("C-x C-b" . counsel-switch-buffer))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t))

(leaf ivy-rich
  :doc "More friendly display transformer for ivy."
  :req "emacs-24.5" "ivy-0.8.0"
  :tag "ivy" "emacs>=24.5"
  :emacs>= 24.5
  :ensure t
  :after ivy
  :global-minor-mode t)

(leaf ivy-ghq
  :if (executable-find "ghq")
  :el-get (ivy-ghq
           :type github
           :pkgname "analyticd/ivy-ghq")
  :bind (("C-x g" . ivy-ghq-open))
  :custom
  (ivy-ghq-short-list . t))


(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :commands (prescient-persist-mode)
  :custom `((prescient-aggressive-file-save . t)
            (prescient-save-file . ,(locate-user-emacs-file "prescient")))
  :global-minor-mode prescient-persist-mode)

(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)

(leaf anzu
  :doc "provides a minor mode which displays current match and total matches information in the mode-line in various search modes"
  :ensure t
  :bind
  (("C-c r" . anzu-query-replace)
   ("C-c C-m" . anzu-query-replace-at-cursor-thing))
  :config
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-search-threshold 1000))
  :custom
  (global-anzu-mode . t))

(leaf yafolding
  :doc "Folding code blocks based on indentation."
  :url "https://github.com/zenozeng/yafolding.el"
  :ensure t
  :bind ("C-c i" . yafolding-toggle-element))

(leaf linum-mode
  :custom
  ((global-linum-mode . t)
   (linum-format . "%4d|\s")))

(leaf hlinum
  :doc "Extension for linum.el to highlight current line number"
  :ensure t
  :init (hlinum-activate))

(leaf highlight-indent-guides
  :ensure t
  :custom
  ((highlight-indent-guides-auto-enabled . t)
   (highlight-indent-guides-method . 'column))
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(leaf which-key
  :doc "minor mode for Emacs that displays the key bindings following your currently entered incomplete command (a prefix) in a popup."
  :ensure t
  :init (which-key-mode))

(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :config
  (flycheck-may-check-automatically '(idle-change mode-enabled new-line save))
  :global-minor-mode global-flycheck-mode)


;;;
;; Programming
;;;

(leaf eglot
  :ensure t
  :doc "Emacs Polyglot: an Emacs LSP client that stays out of your way"
  :require t
  :config
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (add-hook 'ruby-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook 'eglot-ensure))

(leaf magit
  :ensure t)

(leaf quickrun
  :ensure t
  :config
  (quickrun-add-command "kotlin"
    '((:command . "kotlin")
      (:exec    . ("kotlinc %o %s" "%c %NKt %a"))
      (:remove  . ("%nKt.class"))
      (:tempfile . nil)
      (:description . "Compile Kotlin file and execute")
      )
    :mode 'kotlin-mode))

(leaf yaml-mode
  :ensure t
  :mode "\\(\.yml\\|\.yaml\\)")

(leaf ruby-mode
  :mode
  (("\\.rb$" . ruby-mode)
	 ("Gemfile$" . ruby-mode)
	 ("Capfile$" . ruby-mode)
	 ("Guardfile$" . ruby-mode)
	 ("[Rr]akefile$" . ruby-mode))
  :hook (electric-pair-mode rubocop-mode)
  :config
  (leaf ruby-electric
    :ensure t
    :hook (ruby-mode-hook . ruby-electric-mode))
  (leaf rubocop
    :ensure t)
  (leaf rspec-mode
    :ensure t
    :bind (rspec-mode-map
           ("C-c t" . rspec-verify)))
  :custom
  (flycheck-disabled-checkers . '(ruby-rubylint))
  (ruby-insert-encoding-magic-comment . nil)
  (flycheck-checker 'ruby)
  )

(leaf go-mode
  :ensure t
  :hook ((eglot-ensure))
  :custom
  ((gofmt-command . "goimports"))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (leaf gotest
    :doc "Run Go tests and programs from Emacs"
    :ensure t
    :bind (go-mode-map
           ("C-c C-r" . go-run)
           ("C-c t" . go-test-current-test)
           ("C-c C-t" . go-test-current-file))))

(leaf slim-mode
  :ensure t
  :doc "slim-mode provides Emacs support for editing Slim templates. It's based on haml-mode.")

(leaf js2-mode
  :ensure t
  :mode
  (("\\.js$" . js2-mode))
  :config
  (add-hook 'js2-mode-hook (lambda()
                             (tern-mode t)
                             (flycheck-mode t)))
  (leaf tern
    :ensure t
    :custom
    (eval-after-load 'tern
      '(progn
         (require 'tern-auto-complete)
         (tern-ac-setup))))
  :custom
  ((js2-basic-offset . 2)))

(leaf web-mode
  :ensure t)

;; EmacsにおけるTypescript + React JSXの苦悩と良さげな設定について
;; https://qiita.com/nuy/items/ebcb25ad14f02ab72790
(leaf typescript-mode
  :ensure t
  :mode (("\\.tsx\\'" . typescript-mode))
  :bind (typescript-mode-map
         ("C-c C-r" . quickrun))
  :config
  (leaf mmm-mode
    :commands (mmm-mode)
    :ensure t
    :config
    (setq mmm-global-mode t)
    (setq mmm-submode-decoration-level 0)
    (mmm-add-classes
     '((mmm-jsx-mode
        :submode web-mode
        :face mmm-code-submode-face
        :front "\\(return\s\\|n\s\\|(\n\s*\\)<"
        :front-offset -1
        :back ">\n?\s*)\n}\n"
        :back-offset 1
        )))
    (mmm-add-mode-ext-class 'typescript-mode nil 'mmm-jsx-mode)
    (defun mmm-reapply ()
      (mmm-mode)
      (mmm-mode))
    (add-hook 'after-save-hook
              (lambda ()
                (when (string-match-p "\\.tsx?" buffer-file-name)
                  (mmm-reapply)))))
  (add-hook 'typescript-mode-hook
            (lambda ()
              (interactive)
              (mmm-mode)))
  (leaf tide
    :doc "TypeScript Interactive Development Environment for Emacs"
    :ensure t
    :config
    (add-hook 'typescript-mode-hook
              (lambda ()
                (tide-setup)
                (tide-hl-identifier-mode t)
                (flycheck-mode t)
                (setq flycheck-check-syntax-automatically '(save mode-enabled))
                (eldoc-mode t)
                (company-mode-on)))))

(leaf kotlin-mode
  :ensure t)

(provide 'init)

