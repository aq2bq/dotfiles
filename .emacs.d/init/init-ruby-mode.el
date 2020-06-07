(use-package ruby-mode
  :init
  ;; Solargraph + Rails
  ;; https://github.com/castwide/solargraph/issues/87
  ;; https://speakerdeck.com/blue0513/emacs-de-lsp-woshi-tutemitayo?slide=20
  ;; 1. Make sure you have gem documentation installed by running yard gems.
  ;; 2. Add a .solargraph.yml file to the app's root directory (you can generate a default one by running solargraph config) and make the following change to the require section:
  ;;
  ;; $RAILS_ROOT/.solargraph.yml
  ;; require:
  ;; - actioncable
  ;; - actionmailer
  ;; - actionpack
  ;; - actionview
  ;; - activejob
  ;; - activemodel
  ;; - activerecord
  ;; - activestorage
  ;; - activesupport
  :mode (("\\.rb$" . ruby-mode)
	 ("Gemfile$" . ruby-mode)
	 ("Capfile$" . ruby-mode)
	 ("Guardfile$" . ruby-mode)
	 ("[Rr]akefile$" . ruby-mode))
  :init
  (add-hook 'ruby-mode-hook 'electric-pair-mode)
  (setq ruby-electric-expand-delimiters-list nil)
  (add-hook 'ruby-mode-hook (lambda()
                              (company-mode)
                              (setq company-auto-expand t)
                              (setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
                              (setq company-idle-delay 0) ; 遅延なしにすぐ表示
                              (setq company-minimum-prefix-length 1) ; 何文字打つと補完動作を行うか設定
                              (setq company-selection-wrap-around t) ; 候補の最後の次は先頭に戻る
                              (setq completion-ignore-case t)
                              (setq company-dabbrev-downcase nil)
                              (global-set-key (kbd "C-M-i") 'company-complete)
                              ;; C-n, C-pで補完候補を次/前の候補を選択
                              (define-key company-active-map (kbd "C-n") 'company-select-next)
                              (define-key company-active-map (kbd "C-p") 'company-select-previous)
                              (define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
                              (define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
                              (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う
                              ))
  ;; (add-hook 'ruby-mode-hook 'eglot-ensure)
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  )
