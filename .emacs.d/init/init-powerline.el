;; http://jedipunkz.github.io/blog/2012/05/04/powerline.el-emacs/

(defun arrow-right-xpm (color1 color2)
  "Return an XPM right arrow string representing."
    (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\".           \",
\"..          \",
\"...         \",
\"....        \",
\".....       \",
\"......      \",
\".......     \",
\"........    \",
\".........   \",
\".........   \",
\"........    \",
\".......     \",
\"......      \",
\".....       \",
\"....        \",
\"...         \",
\"..          \",
\".           \"};"  color1 color2))

(defun arrow-left-xpm (color1 color2)
  "Return an XPM right arrow string representing."
    (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\". c %s\",
\"  c %s\",
\"           .\",
\"          ..\",
\"         ...\",
\"        ....\",
\"       .....\",
\"      ......\",
\"     .......\",
\"    ........\",
\"   .........\",
\"   .........\",
\"    ........\",
\"     .......\",
\"      ......\",
\"       .....\",
\"        ....\",
\"         ...\",
\"          ..\",
\"           .\"};"  color2 color1))


(defconst color1 "#0000FF")
(defconst color3 "yellow")
(defconst color2 "#FFFFFF")
(defconst color4 "#000000")

(defvar arrow-right-1 (create-image (arrow-right-xpm color1 color2) 'xpm t :ascent 'center))
(defvar arrow-right-2 (create-image (arrow-right-xpm color2 "None") 'xpm t :ascent 'center))
(defvar arrow-left-1  (create-image (arrow-left-xpm color2 color1) 'xpm t :ascent 'center))
(defvar arrow-left-2  (create-image (arrow-left-xpm "None" color2) 'xpm t :ascent 'center))

(setq-default mode-line-format
	      (list  '(:eval (concat (propertize " %b " 'face 'mode-line-color-1)
				     (propertize " " 'display arrow-right-1)))
		     '(:eval (concat (propertize " %m " 'face 'mode-line-color-2)
				     (propertize " " 'display arrow-right-2)))

		     ;; Justify right by filling with spaces to right fringe - 16
		     ;; (16 should be computed rahter than hardcoded)
		     '(:eval (propertize " " 'display '((space :align-to (- right-fringe 17)))))

		     '(:eval (concat (propertize " " 'display arrow-left-2)
				     (propertize " %p " 'face 'mode-line-color-2)))
		     '(:eval (concat (propertize " " 'display arrow-left-1)
				     (propertize "%4l:%2c  " 'face 'mode-line-color-1)))
		     ))

(make-face 'mode-line-color-1)
(set-face-attribute 'mode-line-color-1 nil
		    :foreground "#000"
		    :background color1)

(make-face 'mode-line-color-2)
(set-face-attribute 'mode-line-color-2 nil
		    :foreground "#090909"
		    :background color2)

(set-face-attribute 'mode-line nil
		    :foreground "midnightblue"
		    :background color3
		    :box nil)
(set-face-attribute 'mode-line-inactive nil
		    :foreground "#000"
		    :background color4)
