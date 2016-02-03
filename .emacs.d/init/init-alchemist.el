(eval-after-load "alchemist"
  #'(progn
      (eval-after-load "elixir-mode"
	#'(progn
	    (define-key alchemist-mode-map (kbd "C-x C-e") 'alchemist-iex-send-last-sexp)

	    (defun my-alchemist-iex-electric-send-thing (uarg)
	      "Sends the code fragment to the inferior IEx process.
If universal argument (C-u) is given, jump to the buffer.
when region is active, sends the marked region.
Otherwise sends the current line."
	      (interactive "P")
	      (cond
	       ;; regionがアクティブかつC-uが押されている
	       ((and uarg (use-region-p))
		(alchemist-iex-send-region-and-go))
	       ;; regionがアクティブ
	       ((and (not uarg) (use-region-p))
		(alchemist-iex-send-region (point) (mark)))
	       ;; regionなし、かつC-uが押されている
	       ((and uarg (not (use-region-p)))
		(alchemist-iex-send-current-line-and-go))
	       ;; なんにもなし
	       ((and (not uarg) (not (use-region-p)))
		(alchemist-iex-send-current-line))))

	    (define-key alchemist-mode-map (kbd "C-M-x") 'my-alchemist-iex-electric-send-thing)))))
