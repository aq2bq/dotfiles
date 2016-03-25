(use-package tern
  :init
  (add-hook 'js2-mode-hook
	    '(lambda ()
	       (when (locate-library "tern")
		 (setq tern-command '("tern" "--no-port-file"))
		 (tern-mode t))))
  ;; (setq company-tern-property-marker "")
  ;; (defun company-tern-depth (candidate)
  ;;   "Return depth attribute for CANDIDATE. 'nil' entries are treated as 0."
  ;;   (let ((depth (get-text-property 0 'depth candidate)))
  ;;         (if (eq depth nil) 0 depth)))
  :bind
  ("C-c t" . tern-get-type) ;; Rebind from "C-c C-c"
  :config
  (eval-after-load 'tern
    '(progn
       ;; (require 'tern-auto-complete)
       ;; (tern-ac-setup)
       (unbind-key "C-c C-c" tern-mode-keymap)
       (require 'company)
       (company-mode t)
       (add-to-list 'company-backends 'company-tern)))
  )

;; REF: http://qiita.com/sune2/items/e54bb5db129ae73d004b

