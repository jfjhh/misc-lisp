(require :sdl2)
(require :cl-opengl)

(defun polygon (&optional (r 1.0) (v 32))
  (gl:with-primitive :triangles
    (gl:color 1.0 0.0 1.0)
    (loop :for i :from 1 :to v :do
       (let ((t1 (* 2 pi (/ (1- i) v)))
	     (t2 (* 2 pi (/ i v))))
	 
	 (gl:vertex 0.0 0.0)
	 (gl:vertex (* r (cos t1)) (* r (sin t1)))
	 (gl:vertex (* r (cos t2)) (* r (sin t2)))))))

(defun circle (&optional (r 1.0))
  ;; Adjust the number of vertices in the polygon for size, such that
  ;; smaller circles use fewer vertices, linear in the radius.
  (polygon r (ceiling (+ (* 56 r) 8))))

(defun render ()
  ;; Draw a triangle.
  (gl:with-primitive :triangles
    (gl:color 1.0 0.0 0.0)
    (gl:vertex  0.0 0.5)
    (gl:vertex -0.5 -0.5)
    (gl:vertex  0.5 -0.5))

  ;; Draw a quad.
  (gl:with-primitive :triangles
    (gl:color 0.0 1.0 0.0)
    (gl:vertex -0.2 -0.2)
    (gl:vertex  0.2 -0.2)
    (gl:vertex -0.2  0.2)
    (gl:vertex -0.2  0.2)
    (gl:vertex  0.2 -0.2)
    (gl:vertex  0.2  0.2))

  ;; Draw a circle.
  (circle 1))

(sdl2:with-init (:everything)
  (sdl2:with-window (win :title "SDL2/OpenGL Test" :w 640 :h 640 :flags '(:shown :opengl))
    (sdl2:with-gl-context (gl-context win)
      ;; SDL2 window and OpenGL setup.
      (sdl2:gl-make-current win gl-context)
      (gl:viewport 0 0 640 640)
      (gl:matrix-mode :projection)
      (gl:ortho -1 1 -1 1 -1 1)
      (gl:matrix-mode :modelview)
      (gl:load-identity)
      (gl:clear-color 0.0 0.0 1.0 1.0)

      (gl:clear :color-buffer)

      (render)

      (gl:flush)
      (sdl2:gl-swap-window win)

      ;; Sleep to keep the window alive.
      (sleep 3))))

(defun range (min max)
  (unless (> min max)
    (cons min (range (1+ min) max))))
