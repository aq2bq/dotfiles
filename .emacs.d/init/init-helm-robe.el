(use-package helm-robe
  :config
  (custom-set-variables
   '(robe-completing-read-func 'helm-robe-completing-read))
  (defun helm-robe-completing-read (prompt choices &optional predicate require-match)
    (let ((collection (mapcar (lambda (c) (if (listp c) (car c) c)) choices)))
      (helm-comp-read prompt collection :test predicate :must-match require-match)))
  )
