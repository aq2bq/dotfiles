(use-package inf-ruby
  :commands inf-ruby
  :init
  (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)
  :config
  (;(setq inf-ruby-default-implementation "pry")
   ;;(setq inf-ruby-eval-binding "Pry.toplevel_binding")
   )
)
