;;; init-minibuffer.el --- Minibuffer completion -*- lexical-binding: t; -*-

;; Completion matching style;; Emacsの次世代ミニバッファ補完UI
;; https://blog.tomoya.dev/posts/a-new-wave-has-arrived-at-emacs/
(leaf orderless
  :ensure t
  :doc "Completion matching style"

  :custom
  ((completion-styles . '(orderless basic))
   (completion-category-defaults . nil)
   (completion-category-overrides
    . '((file (styles basic partial-completion))))))


;; Minibuffer completion UI
(leaf vertico
  :ensure t
  :doc "Vertical minibuffer completion UI"

  :global-minor-mode vertico-mode

  :bind
  (minibuffer-local-map
   ("C-l" . vertico-directory-up))

  :custom
  ((vertico-count . 20)))


;; Annotations for minibuffer candidates
(leaf marginalia
  :ensure t
  :doc "Annotations for minibuffer completion"

  :global-minor-mode marginalia-mode)


;; Actions on completion candidates
(leaf embark
  :ensure t

  :bind
  (("C-." . embark-act)
   (minibuffer-local-map
    :package emacs
    ("M-." . embark-dwim)
    ("C-." . embark-act)))

  :config
  ;; Embark 起動中に ? で help を表示
  (setopt embark-help-key "?"))


;; Search / navigation commands using completing-read
(leaf consult
  :ensure t
  :doc "Enhanced search and navigation commands"
  :url "https://github.com/minad/consult"

  :bind
  (("C-x C-b" . consult-buffer)
   ("C-x l" . consult-goto-line)
   ("C-s" . consult-line)
   ("C-c s" . consult-ripgrep)
   ("C-c C-s" . consult-ripgrep-specific-directory)
   ("C-c C-r" . consult-recent-file))

  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  ;; 都度対象ディレクトリを指定して ripgrep
  (defun consult-ripgrep-specific-directory ()
    (interactive)
    (let ((consult-project-function
           (lambda (_) default-directory)))
      (consult-ripgrep
       (read-directory-name "Directory: ")))))


;; Embark + Consult integration
(leaf embark-consult
  :ensure t
  :after (embark consult))


;; ghq repositories via Consult
(leaf consult-ghq
  :ensure t
  :if (executable-find "ghq")
  :doc "ghq interface for Consult"

  :bind
  (("M-g M-f" . consult-ghq-find))

  :custom
  ((consult-ghq-find-function . 'dired)))


(provide 'init-minibuffer)
