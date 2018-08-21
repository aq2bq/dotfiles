;; 僕のGolang開発環境（Emacs編）
;; https://log.shinofara.xyz/2016/07/%E5%83%95%E3%81%AEgolang%E9%96%8B%E7%99%BA%E7%92%B0%E5%A2%83emacs%E7%B7%A8/

(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook (lambda()
                          (add-hook 'before-save-hook' 'gofmt-before-save)
                          (local-set-key (kbd "M-.") 'godef-jump)
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)
                          ))
