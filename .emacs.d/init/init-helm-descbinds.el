;; Helm Descbinds
;; show all Bindings
(helm-descbinds-mode t)
(require 'bind-key)
(bind-key "C-c k" 'helm-descbinds)
