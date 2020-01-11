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

(defun dir-len-to-vec2 (dir len)
  "Converts params dir \"R\" and len 12 into vec2 12 0"
  (cond ((string= dir "R") (make-vec2 len 0))
        ((string= dir "L") (make-vec2 (- len) 0))
        ((string= dir "U") (make-vec2 0 len))
        ((string= dir "D") (make-vec2 0 (- len)))
        (t (throw 'invalid-direction nil))))

(defun parse-item (item)
  "Converts the string of the format \"R12\" to vec2 type 12, 0"
  (let* ((dir (substring item 0 1))
         (len (string-to-number (substring item 1 (length item)))))
    (dir-len-to-vec2 dir len)))

(defun parse-line (line)
  (mapcar 'parse-item (split-string line "," t)))

(defun load-wires ()
  (with-temp-buffer
    (insert-file-contents "3.txt")
    (let* ((lines (split-string (buffer-string) "\n" t))
           (lines-with-vectors (mapcar 'parse-line lines)))
      lines-with-vectors)))

(defun swap (a-sym b-sym)
  (let ((temp))
    (setq temp (symbol-value a-sym))
    (set a-sym (symbol-value b-sym))
    (set b-sym temp)))

(defun get-intersect (p1 p2 p3 p4)
  "Return the intersection of two line segments if they are
perpendicular"
  (if (= (vec2-x p1) (vec2-x p2)) (progn (swap 'p1 'p3) (swap 'p2 'p4)))
  (if (< (vec2-x p2) (vec2-x p1)) (progn (swap 'p2 'p1)))
  (if (< (vec2-y p4) (vec2-y p3)) (progn (swap 'p4 'p3)))
  (if (and (not (= (vec2-y p1) (vec2-y p2))) (not (= (vec2-x p3) (vec2-x p4))))
    throw 'not-perpendicular-segments nil)
  (if (and
         (and (< (vec2-x p1) (vec2-x p3)) (< (vec2-x p3) (vec2-x p2)))
         (and (< (vec2-x p1) (vec2-x p4)) (< (vec2-x p4) (vec2-x p2)))
         (and (< (vec2-y p3) (vec2-y p1)) (< (vec2-y p1) (vec2-y p4)))
         (and (< (vec2-y p3) (vec2-y p2)) (< (vec2-y p2) (vec2-y p4))))
    (make-vec2 (vec2-x p3) (vec2-y p1))))

(defun find-intersection (wire1 wire)
  (let ((wire2-pos (make-vec2 0 0))
        (wire2-len 0))
    (dotimes (i (- (length wire2) 1))
      (let ((wire1-pos (make-vec2 0 0))
            (wire1-len 0))
        (dotimes (j (- (length wire1) 1))
          (let* ((p1 (elt wire1 j))
                 (p2 (elt wire1 (+ 1 j)))
                 (p3 (elt wire2 i))
                 (p4 (elt wire2 (+ 1 i)))
                 (p1-wire-pos (vec2-add wire1-pos p1))
                 (p2-wire-pos (vec2-add p1-wire-pos p2))
                 (p3-wire-pos (vec2-add wire2-pos p3))
                 (p4-wire-pos (vec2-add p3-wire-pos p4))
                 (intersect (get-intersect p1-wire-pos p2-wire-pos p3-wire-pos p4-wire-pos)))
            (if intersect (push (+
                                   wire1-len (vec2-get-length p1) (vec2-get-length (vec2-sub p1-wire-pos intersect))
                                   wire2-len (vec2-get-length p3) (vec2-get-length (vec2-sub p3-wire-pos intersect)))
                                  distances))
            (setq wire1-len (+ wire1-len (vec2-get-length p1)))
            (setq wire1-pos (vec2-add wire1-pos p1))))
        (setq wire2-len (+ wire2-len (vec2-get-length (elt wire2 i))))
        (setq wire2-pos (vec2-add wire2-pos (elt wire2 i)))))))

(defun get-intersections (wires)
  (let* ((distances)
         (i 0))
    (while (< i (length wires))
      (let* ((wire1 (nth i wires))
             (j i))
        (while (< j (length wires))
          (let ((wire2 (nth j wires)))
            ;; Cross check every wire with each other except for itself
            (if (not (eq wire1 wire2))
                (find-intersection wire1 wire2)))
          (setq j (+ 1 j))))
      (setq i (+ i 1)))
  distances))

(defun get-nearest-length (lengths)
  (let* ((shortest most-positive-fixnum))
    (dolist (len lengths)
      (if (< len shortest)
        (setq shortest len)))
    shortest))

(defun run ()
  (get-nearest-length (get-intersections (load-wires))))

(insert (format "%d" (run)))
