(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :asdf)
  (ql:quickload :alexandria)
  (ql:quickload :uiop)
  (ql:quickload :clx-truetype))

(defpackage "PAULE-HW"
  (:use "COMMON-LISP" "XLIB"))
(in-package "PAULE-HW")

(defun hello-world (host &rest args &key (display 0) (string "Hello World") (font "fixed"))
  (let ((display  (open-display host :display display))
	    (abort t))
	(let* ((screen (display-default-screen display))
		   (black (screen-black-pixel screen))
		   (white (screen-white-pixel screen))
		   (font (open-font display font))
		   ;; (border 1) ; Minimum margin around the text
		   (width  200) (height 200)
		   (x 200) (y 200)
		   (window (create-window :parent (screen-root screen)
					              :x x :y y :width width :height height
					              :background black	  :border white
					              :border-width 1	  :colormap (screen-default-colormap screen)
					              :bit-gravity :center
					              :event-mask '(:exposure :button-press)))
		   (gcontext (create-gcontext :drawable window
					                  :background black
					                  :foreground white
					                  :font font)))
	  ;; Set window manager hints
	  (set-wm-properties window
				         :name 'hello-world
				         :icon-name string
				         :resource-name string
				         :resource-class 'hello-world
				         :command (list* 'hello-world host args)
				         :x x :y y :width width :height height
				         :min-width width :min-height height
				         :input :off :initial-state :normal)
	  (map-window window)		; Map the window
	  ;; Handle events
	  (event-case (display :discard-p t :force-output-p t)
		(exposure  ;; Come here on exposure events
		 (window count)
		 (when (zerop count) ;; Ignore all but the last exposure event
		   (with-state (window)
		     (let ((x (truncate (- (drawable-width window) width) 2))
			       (y (truncate (- (+ (drawable-height window)
					                  (max-char-ascent font))
					               (max-char-descent font))
					            2)))
			   ;; Draw text centered in widnow
			   (clear-area window)
			   (draw-glyphs window gcontext x y string)))
		   ;; Returning non-nil causes event-case to exit
		   nil))
		(button-press () t)))  ;; Pressing any mouse-button exits
	(setq abort nil)
    ;; Ensure display is closed when done
    (when display
	  (close-display display :abort abort))))

;; (paule-hw::hello-world "127.0.0.1" :display 1)

