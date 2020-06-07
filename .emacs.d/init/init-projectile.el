;; Bridge projectile and project together so packages that depend on project
;; like eglot work
(use-package projectile
  :init
  ;; Bridge projectile and project together so packages that depend on project
  ;; like eglot work
  (defun my-projectile-project-find-function (dir)
    (let ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))
  (projectile-mode t)
  (with-eval-after-load 'project
    (add-to-list 'project-find-functions 'my-projectile-project-find-function)))
