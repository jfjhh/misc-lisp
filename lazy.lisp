;;;;
;;;; An attempt at implementing lazy evaluation.
;;;; Alex Striff
;;;;

;;;
;;; TODO:
;;;
;;; - Combine currying and laziness.
;;; - Is there a need to know beforehand how many arguments a function takes
;;;   for proper lazy currying?
;;;

(defun curry (f &rest parts)
  (lambda (&rest args) (apply f (append parts args))))

(defun flip (f)
  (lambda (&rest args) (apply f (append (cdr args) (list (car args))))))

(defun compose (&rest funcs)
  (cond ((null funcs) #'identity)
	((not (cdr funcs)) (car funcs))
	(t (lambda (&rest args) (funcall (car funcs)
				(apply (apply #'compose (cdr funcs)) args))))))

(defmacro lazy (&body body)
  (let ((value (gensym))
	(memoized (gensym)))
    `(let ((,value nil)
	   (,memoized nil))
       (lambda ()
	 (unless ,memoized
	   (setf ,memoized t)
	   (setf ,value (progn ,@body)))
	 ,value))))

(defun force (lazy-f)
  (funcall lazy-f))

(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (lst)
  (not (force lst)))

(defun lazy-listp (lst)
  (force lst))

(defmacro lazy-when (p &body body)
  `(if ,p
       (progn ,@body)
       (lazy-nil)))

(defmacro lazy-unless (p &body body)
  `(lazy-when (not ,p)
     ,@body))

(defmacro lazy-cons (a b)
  `(lazy (cons ,a ,b)))

(defun lazy-car (lcons)
  (when lcons
    (car (force lcons))))

(defun lazy-cdr (lcons)
  (when lcons
    (cdr (force lcons))))

(defun lazy-first (lcons)
  (lazy-car lcons))

(defun lazy-rest (lcons)
  (lazy-cdr lcons))

(defun lazy-nth (n lcons)
  (if (<= n 0)
      (lazy-car lcons)
      (lazy-nth (1- n) (lazy-cdr lcons))))

(defun lazy-list (&rest elements)
  (make-lazy elements))

(defun make-lazy (lst)
  (lazy-when lst
      (lazy-cons (car lst) (make-lazy (cdr lst)))))

(defun take (n lst)
  (unless (or (not (plusp n)) (lazy-null lst))
    (cons (lazy-car lst) (take (1- n) (lazy-cdr lst)))))

(defun take-rep (n lst &optional (rep nil))
  (if (and (plusp n) (lazy-listp lst))
      (cons (lazy-car lst) (take-rep (1- n) (lazy-cdr lst) rep))
      (if (numberp n)
	(take n (replicate rep)))))

(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))

(defun lazy-mapcar (f lst)
  (lazy-unless (lazy-null lst)
    (lazy-cons (funcall f (lazy-car lst))
	       (lazy-mapcar f (lazy-cdr lst)))))

(defun lazy-mapcan (f lst)
  (labels ((g (cur)
	     (if (lazy-null cur)
		 (force (lazy-mapcan f (lazy-cdr lst)))
		 (cons (lazy-car cur)
		       (lazy (g (lazy-cdr cur)))))))
    (lazy (unless (lazy-null lst)
	    (g (funcall f (lazy-car lst)))))))

(defun lazy-zip (&rest lists)
  (lazy-unless (some #'lazy-null lists)
    (lazy-cons (mapcar #'lazy-car lists)
	       (apply #'lazy-zip (mapcar #'lazy-cdr lists)))))

(defun lazy-zip-lazy (lst)
  (lazy-cons (lazy-mapcar #'lazy-car lst)
	     (lazy-zip-lazy (lazy-mapcar #'lazy-cdr lst))))

(defun lazy-zip-with (f lst)
  (lazy-cons (lazy-mapcar f lst)
	     (lazy-zip-lazy (lazy-mapcar #'lazy-cdr lst))))

(defun lazy-filter (p lst)
  (lazy-unless (lazy-null lst)
    (let ((head (lazy-car lst)))
      (if (funcall p head)
	  (lazy-cons head (lazy-filter p (lazy-cdr lst)))
	  (lazy-filter p (lazy-cdr lst))))))

(defun lazy-find-if (p lst)
  (unless (lazy-null lst)
    (let ((head (lazy-car lst)))
      (if (funcall p head)
	  head
	  (lazy-find-if p (lazy-cdr lst))))))

(defun replicate (&optional (x nil))
  (lazy-cons x (replicate x)))

(defun replicate-f (f)
  (lazy-cons (funcall f) (replicate-f f)))

(defun identity-matrix ()
  (let ((index 0))
    (lazy-mapcar
     #'(lambda ()
	 (lazy-mapcar #'(lambda (x) (if (= x index) 1 0))
		      *naturals*)
	 (incf index))
     (replicate))))

(defparameter *naturals*
  (labels ((f (n) (lazy-cons n (f (1+ n)))))
    (f 0)))

(defparameter *identity-matrix*
  (labels ((f (n)
	     (labels ((g (m) (lazy-cons (if (= m n) 1 0) (g (1+ m)))))
	       (lazy-cons (g 0) (f (1+ n))))
	     ))
    (f 0)))

(defun random-matrix (n)
  (replicate-f #'(lambda () (replicate-f (curry #'random n)))))

(defun random-int-matrix (n)
  (replicate-f #'(lambda () (replicate-f (compose (curry #'- n) (curry #'random (* n 2)))))))

(defun print-matrix (lst n)
  (let* ((m (mapcar (curry #'take n) (take n lst)))
	 (width (1+ (apply #'max (mapcar #'(lambda (row) (apply #'max (mapcar #'(lambda (x) (if (zerop x) 1 (let ((y (abs x)) (extra (if (plusp x) 0 1))) (+ extra (floor (log y 10)))))) row))) m)))))
    (format t "~{|~{ ~{~Vd~}~} |~%~}"
	    (mapcar #'(lambda (r) (mapcar #'(lambda (v) (list width v)) r)) m))))
