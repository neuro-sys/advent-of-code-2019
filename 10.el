(defconst *char-astro* (string-to-char "#"))

(defun load-input ()
  (with-temp-buffer
    (insert-file-contents "10.txt")
    (split-string (buffer-string) "\n" t)))

(defun get-dimensions (data)
  (list (length (elt data 0)) (length data)))

(defun get-map-array (data)
  (mapconcat 'identity data ""))

(defun get-vectors (m dims)
  (let ((v)
        (w (elt dims 0))
        (h (elt dims 1)))
    (dotimes (y h)
      (dotimes (x w)
        (let ((el (elt m (+ (* y w) x))))
          (if (eq el *char-astro*)
              (push (list x y) v)))))
    v))

(defun make-vec2 (x y)
  (list x y))

(defun vec2-x (obj)
  (nth 0 obj))

(defun vec2-y (obj)
  (nth 1 obj))

(defun vec2-add (v1 v2)
  (make-vec2
   (+ (vec2-x v1) (vec2-x v2))
   (+ (vec2-y v1) (vec2-y v2))))

(defun vec2-sub (v1 v2)
  (make-vec2
   (- (vec2-x v1) (vec2-x v2))
   (- (vec2-y v1) (vec2-y v2))))

(defun vec2-get-length (v1)
  (+ (abs (vec2-x v1)) (abs (vec2-y v1))))

(defun dot-product (a b)
  (+ (* (vec2-x a) (vec2-x b)) (* (vec2-y a) (vec2-y b))))

(defun cross-product (a b)
  (list
   0
   0
   (- (* (vec2-x a) (vec2-y b)) (* (vec2-y a) (vec2-x b)))))

(defun is-colinear (a b c)
  (let ((ab (vec2-sub b a))
        (ac (vec2-sub c a)))
    (eq (elt (cross-product ab ac) 2) 0)))

(defun is-c-between (a b c)
  (let* ((ab (vec2-sub b a))
         (ac (vec2-sub c a))
         (d1 (dot-product ab ac))
         (d2 (dot-product ab ab)))
    (< 0 d1 d2)))

(defun is-c-on-point (a b c)
  (and (is-colinear a b c)
       (is-c-between a b c)))

(let* ((a (make-vec2 0 1))
       (b (make-vec2 0 3))
       (c (make-vec2 0 2)))
  (is-c-on-point a b c))

(defun run ()
  (let* ((data (load-input))
         (dims (get-dimensions data))
         (m (get-map-array data))
         (v (get-vectors m dims))
         (location-map (make-hash-table :test 'equal)))
    (dolist (el1 v)
      (let ((sum 0))
        (dolist (el2 v)
          (if (not (eq el1 el2))
              (cl-block 'check-obstructed
                (let ((obstructed))
                  (dolist (el3 v)
                    (if (and (not (eq el3 el1))
                             (not (eq el3 el2))
                             (is-c-on-point el1 el2 el3))
                        (cl-return-from 'check-obstructed
                          (setq obstructed t)))) ;; break loop early
                  (if (not obstructed)
                      (setq sum (+ 1 sum)))))))
        (puthash el1 sum location-map)))
    (let ((max 0)
          (max-k))
      (maphash (lambda (k v)
                 (if (> v max)
                     (progn
                       (setq max v)
                       (setq max-k k))))
               location-map)
      (gethash max-k location-map))))

(insert (message "Result: %d" (run)))
