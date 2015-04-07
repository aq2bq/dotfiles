;; Markdown
(require 'use-package)
(use-package markdown-mode
  :commands markdown-mode
  :mode (("\\.md$" . markdown-mode))
 )
