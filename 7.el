(require 'queue)

(defconst op-add 1)
(defconst op-mul 2)
(defconst op-in 3)
(defconst op-out 4)
(defconst op-jump-if-true 5)
(defconst op-jump-if-false 6)
(defconst op-less-than 7)
(defconst op-equals 8)
(defconst op-end 99)

(defconst mode-position 0)
(defconst mode-immediate 1)
    
(defun load-program ()
  (with-temp-buffer
    (insert-file-contents "7.txt")
    (vconcat
     (mapcar 'string-to-number (split-string (buffer-string) "," t)))))

(defun decode-mode (op)
  (let ((a (% (floor (/ op 100)) 10))
        (b (% (floor (/ op 1000)) 10))
        (c (% (floor (/ op 10000)) 10)))
    (list a b c)))

(defun decode-inst (op)
  (% op 100))

;; Simplify decoder further

(defun decode-op-in-param (memory pc-sym)
  (let* ((pc-val (symbol-value pc-sym))
         (op (elt memory pc-val))
         (modes (decode-mode op))
         (a (elt memory (+ 1 pc-val))))
    (list a)))

(defun decode-op-1-param (memory pc-sym)
  (let* ((pc-val (symbol-value pc-sym))
         (op (elt memory pc-val))
         (modes (decode-mode op))
         (a (elt memory (+ 1 pc-val)))
         (a (if (= (nth 0 modes) mode-position) (elt memory a) a)))
    (list a)))

(defun decode-op-2-param (memory pc-sym)
  (let* ((pc-val (symbol-value pc-sym))
         (op (elt memory pc-val))
         (modes (decode-mode op))
         (a (elt memory (+ 1 pc-val)))
         (b (elt memory (+ 2 pc-val)))
         (a (if (= (nth 0 modes) mode-position) (elt memory a) a))
         (b (if (= (nth 1 modes) mode-position) (elt memory b) b)))
    (list a b)))

(defun decode-op-3-param (memory pc-sym)
  (let* ((pc-val (symbol-value pc-sym))
         (op (elt memory pc-val))
         (modes (decode-mode op))
         (a (elt memory (+ 1 pc-val)))
         (b (elt memory (+ 2 pc-val)))
         (c (elt memory (+ 3 pc-val)))
         (a (if (= (nth 0 modes) mode-position) (elt memory a) a))
         (b (if (= (nth 1 modes) mode-position) (elt memory b) b)))
    (list a b c)))

(defun do-op-add (memory pc-sym)
  (let ((operands (decode-op-3-param memory pc-sym)))
    (aset memory (nth 2 operands) (+ (nth 0 operands) (nth 1 operands)))
    (set pc-sym (+ 4 (symbol-value pc-sym)))))

(defun do-op-mul (memory pc-sym)
  (let ((operands (decode-op-3-param memory pc-sym)))
    (aset memory (nth 2 operands) (* (nth 0 operands) (nth 1 operands)))
    (set pc-sym (+ 4 (symbol-value pc-sym)))))

(defun do-op-in (memory pc-sym in-buffer)
  (let ((operands (decode-op-in-param memory pc-sym))
        (b (if (queue-empty in-buffer)
               (throw 'waiting-input nil)
             (queue-dequeue in-buffer))))
    (aset memory (nth 0 operands) b))
  (set pc-sym (+ 2 (symbol-value pc-sym))))

(defun do-op-out (memory pc-sym out-buffer)
  (let* ((operands (decode-op-1-param memory pc-sym))
         (b (nth 0 operands)))
    (queue-enqueue out-buffer b))
  (set pc-sym (+ 2 (symbol-value pc-sym))))

(defun do-op-jump-if-true (memory pc-sym)
  (let ((operands (decode-op-2-param memory pc-sym)))
    (if (not (zerop (nth 0 operands)))
        (set pc-sym (nth 1 operands))
      (set pc-sym (+ 3 (symbol-value pc-sym))))))

(defun do-op-jump-if-false (memory pc-sym)
  (let ((operands (decode-op-2-param memory pc-sym)))
    (if (zerop (nth 0 operands))
        (set pc-sym (nth 1 operands))
      (set pc-sym (+ 3 (symbol-value pc-sym))))))

(defun do-op-less-than (memory pc-sym)
  (let ((operands (decode-op-3-param memory pc-sym)))
    (if (< (nth 0 operands) (nth 1 operands))
        (aset memory (nth 2 operands) 1)
      (aset memory (nth 2 operands) 0)))
  (set pc-sym (+ 4 (symbol-value pc-sym))))

(defun do-op-equals (memory pc-sym)
  (let ((operands (decode-op-3-param memory pc-sym)))
    (if (= (nth 0 operands) (nth 1 operands))
        (aset memory (nth 2 operands) 1)
      (aset memory (nth 2 operands) 0)))
  (set pc-sym (+ 4 (symbol-value pc-sym))))

(defun int-code-create (memory in-buffer out-buffer)
  (list memory
        in-buffer
        out-buffer
        (length memory)
        0))

(defun int-code-run (int-code)
  (let* ((memory (nth 0 int-code))
         (in-buffer (nth 1 int-code))
         (out-buffer (nth 2 int-code))
         (memory-length (nth 3 int-code))
         (pc (nth 4 int-code))
         (cur-op (decode-inst (elt memory pc))))
    (while (/= cur-op op-end)
      (cond ((= cur-op op-add) (do-op-add memory 'pc))
            ((= cur-op op-mul) (do-op-mul memory 'pc))
            ((= cur-op op-in) (do-op-in memory 'pc in-buffer))
            ((= cur-op op-out) (do-op-out memory 'pc out-buffer))
            ((= cur-op op-jump-if-true) (do-op-jump-if-true memory 'pc))
            ((= cur-op op-jump-if-false) (do-op-jump-if-false memory 'pc))
            ((= cur-op op-less-than) (do-op-less-than memory 'pc))
            ((= cur-op op-equals) (do-op-equals memory 'pc))
            (t (throw 'invalid-op-code cur-op)))
      (setf (nth 4 int-code) pc)
      (setq cur-op (decode-inst (elt memory pc)))))
  t)

(defun swap (v i j)
  (let ((temp))
    (setq temp (elt v i))
    (aset v i (elt v j))
    (aset v j temp)))

(defun get-adjacent (v is-left index)
  (if is-left
      (elt v (- index 1))
    (elt v (+ index 1))))

(defun get-adjacent-index (v is-left index)
  (if is-left
      (- index 1)
    (+ index 1)))

(defun is-mobile (v directions index)
  (defvar is-mobile)
  (block is-mobile
    (let* ((el (elt v index))
           (is-left (not (gethash el directions))))
      (if (or (and is-left (eq index 0)) (and (not is-left) (eq (- (length v) 1) index)))
          (return-from is-mobile nil))
      (let ((neighbour (get-adjacent v is-left index)))
        (> el neighbour)))))

(defun find-largest-mobile-index (v directions)
  (let ((largest-mobile -1)
        (largest-mobile-index))
    (dotimes (i (length v))
      (let ((el (elt v i)))
        (if (and (is-mobile v directions i)
                 (> el largest-mobile))
            (progn
              (setq largest-mobile el)
              (setq largest-mobile-index i)))))
    largest-mobile-index))

(defun johnson-tretter-permutate (v)
  (let* ((p)
         (directions (make-hash-table :test 'equal))
         (largest-mobile-index (find-largest-mobile-index v directions)))
    (dotimes (i (length v))
      (puthash (elt v i) nil directions))
    (push (copy-sequence v) p)
    (while (not (eq largest-mobile-index nil))
      ;; Switch the direction of all the elements whose value is
      ;; greater than the mobile integer value.
      (dotimes (j (length v))
        (let* ((el (elt v j))
               (dir (gethash el directions)))
          (if (> el (elt v largest-mobile-index))
              (puthash el (not dir) directions))))
      (let* ((el (elt v largest-mobile-index))
             (is-left (not (gethash el directions)))
             (adjacent-index (get-adjacent-index v is-left largest-mobile-index)))
        (swap v largest-mobile-index adjacent-index))
      (setq largest-mobile-index (find-largest-mobile-index v directions))
      (push (copy-sequence v) p))
    p))

(defun run ()
  (let* ((memory (load-program))
         (max-thrust 0)
         (signal-seq (johnson-tretter-permutate (vector 5 6 7 8 9))))
    (dolist (seq signal-seq)
      (let* ((phase-a (elt seq 0))
             (phase-b (elt seq 1))
             (phase-c (elt seq 2))
             (phase-d (elt seq 3))
             (phase-e (elt seq 4))
             (buffer-1 (queue-create))
             (buffer-2 (queue-create))
             (buffer-3 (queue-create))
             (buffer-4 (queue-create))
             (buffer-5 (queue-create))
             (int-code-a (int-code-create (copy-sequence memory) buffer-1 buffer-2))
             (int-code-b (int-code-create (copy-sequence memory) buffer-2 buffer-3))
             (int-code-c (int-code-create (copy-sequence memory) buffer-3 buffer-4))
             (int-code-d (int-code-create (copy-sequence memory) buffer-4 buffer-5))
             (int-code-e (int-code-create (copy-sequence memory) buffer-5 buffer-1))
             (cont t))
        (queue-enqueue buffer-1 phase-a)
        (queue-enqueue buffer-1 0)
        (queue-enqueue buffer-2 phase-b)
        (queue-enqueue buffer-3 phase-c)
        (queue-enqueue buffer-4 phase-d)
        (queue-enqueue buffer-5 phase-e)
        (while cont
          (catch 'waiting-input (int-code-run int-code-a))
          (catch 'waiting-input (int-code-run int-code-b))
          (catch 'waiting-input (int-code-run int-code-c))
          (catch 'waiting-input (int-code-run int-code-d))
          (if (catch 'waiting-input (int-code-run int-code-e))
              (setq cont nil)))
        (let ((cur-thrust (queue-dequeue buffer-1)))
          (if (> cur-thrust max-thrust)
              (setq max-thrust cur-thrust)))
        ))
    max-thrust))

(insert (message "Max-thrust: %d" (run)))
