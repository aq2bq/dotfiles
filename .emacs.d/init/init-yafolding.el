(use-package yafolding
  :bind ("C-c i" . yafolding-toggle-element)
  :init
  (add-hook 'ruby-mode-hook 'yafolding-mode)
)
