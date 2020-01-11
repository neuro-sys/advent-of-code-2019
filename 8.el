(require 'cl)

(defconst pix-black 0)
(defconst pix-white 1)
(defconst pix-trans 2)

(defun convert-string-to-number-seq (str)
  (cl-map 'vector 'string-to-number (split-string str "" t)))

(defun load-input ()
  (with-temp-buffer
    (insert-file-contents "8.txt")
    (let ((str (buffer-string)))
      (convert-string-to-number-seq (substring str 0 (- (length str) 1))))))

(defun parse-image-layers (data w h)
  (let* ((layer-len (* w h))
         (data-len (length data))
         (layers)
         (offset 0))
    (if (not (eq (mod data-len layer-len) 0))
        (throw 'invalid-data "Data size does not evenly divide by given dimensions"))
    (let ((layer-num (/ data-len layer-len)))
      (dotimes (i layer-num)
        (push (substring data offset (+ offset layer-len)) layers)
        (setq offset (+ offset layer-len))))
    layers))

(defun count-digits (layer digit)
  (reduce
   (lambda (a b)
     (if (eq digit b) (+ a 1) a))
   layer
   :initial-value 0))

(defun find-layer-with-fewest-0-digits (layers)
  (let ((found-layer-count most-positive-fixnum)
        (found-layer))
    (dolist (layer layers)
      (setq layer (convert-string-to-number-seq layer))
      (let ((count (count-digits layer 0)))
        (if (and (not (zerop count))
                 (< count found-layer-count))
            (progn
              (setq found-layer-count count)
              (setq found-layer layer)))))
    found-layer))

(defun render-image (data w h)
  (let ((layers (parse-image-layers data w h))
        (image (make-vector (* w h) pix-trans)))
    (dotimes (i (length layers))
      ;; (let ((layer (elt layers (- (length layers) i 1))))
      (let ((layer (elt layers i)))
        (dotimes (y h)
          (dotimes (x w)
            (let* ((index (+ (* y w) x))
                   (cur-pix (elt layer index)))
              (if (not (eq cur-pix pix-trans))
                  (aset image index cur-pix)))))))
    image))

(defun print-image (image w h)
  (insert (message "\n;;\t"))
  (dotimes (y h)
    (dotimes (x w)
      (let* ((index (+ (* y w) x))
             (cur-pix (elt image index)))
        (if (eq cur-pix pix-white)
            (insert (message "%d" cur-pix))
          (insert (message " ")))))
    (insert (message "\n;;\t")))
  (insert (message "\n")))

(render-image (load-input) 2 2)

(print-image (elt (parse-image-layers (load-input) 2 2) 3) 2 2)

(let* ((w 25)
       (h 6)
       (image (render-image (load-input) w h)))
  (print-image image w h))
;;	1  1 1111  11  111  1   1
;;	1 1  1    1  1 1  1 1   1
;;	11   111  1  1 111   1 1 
;;	1 1  1    1111 1  1   1  
;;	1 1  1    1  1 1  1   1  
;;	1  1 1    1  1 111    1  
;;	
