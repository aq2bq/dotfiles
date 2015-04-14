(use-package anzu
  :commands anzu-mode
  :init
  (global-anzu-mode +1)
  :bind 
  (("C-c C-r" . anzu-query-replace)
   ("C-c C-m" . anzu-query-replace-at-cursor-thing))
  :config
  (custom-set-variables
   '(anzu-mode-lighter "")
   '(anzu-deactivate-region t)
   '(anzu-use-migemo t)
   '(anzu-search-threshold 1000))
)
