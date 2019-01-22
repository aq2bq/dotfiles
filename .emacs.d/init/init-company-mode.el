(use-package company-mode
  :init
  (global-company-mode t)
  (setq company-selection-wrap-around t) ;; 候補の最下部まで来たら上に戻る
  (setq company-idle-delay 0)
  :bind (:map company-active-map
              ("TAB" . company-select-next)
              ("<backtab>" . company-select-previous)))
