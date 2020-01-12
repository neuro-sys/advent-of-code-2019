(require 'queue)

(defconst op-add 1) ;; Add:           add a, b, c  => Add a and b, write to c
(defconst op-mul 2) ;; Multiply:      mul a, b, c  => Multiply a and b, write to c
(defconst op-in 3)  ;; Read input:    in a         => Read input queue, write to a
(defconst op-out 4) ;; Write output:  out a        => Read a, write to putput queue
(defconst op-jt 5) ;; Jump if True:   jt a, b      => If a = 1 then jump to b
(defconst op-jf 6) ;; Jump if False:  jf a, b      => If a = 0 then jump to b
(defconst op-lt 7) ;; Less Than:      lt a, b, c   => If a > b then write 1 to c, otherwise 0
(defconst op-eq 8) ;; Equals:         eq a, b, c   => If a = b then write 1 to c, otherwise 0
(defconst op-rel 9) ;; Set Relative:  rel a        => Set rel register to a
(defconst op-end 99) ;; Program end:  end

(defconst mode-position 0)
(defconst mode-immediate 1)
(defconst mode-relative 2)

(defun load-program (&optional extra-space)
  (with-temp-buffer
    (insert-file-contents "9.txt")
    (vconcat
     (mapcar 'string-to-number (split-string (buffer-string) "," t))
     (if extra-space (make-vector extra-space 0)))))

(defun decode-mode (op)
  (let ((a (% (floor (/ op 100)) 10))
        (b (% (floor (/ op 1000)) 10))
        (c (% (floor (/ op 10000)) 10)))
    (list a b c)))

(defun decode-inst (op)
  (% op 100))

;; Simplify decoder further

(defun decode-param-with-mode (memory mode param regs &optional is-write)
  (cond ((= mode mode-position) (if is-write param (elt memory param)))
        ((= mode mode-relative) (if is-write (+ (nth 1 regs) param) (elt memory (+ (nth 1 regs) param))))
        ((= mode mode-immediate) param)
        (t (throw 'invalid-mode nil))))

(defun decode-op-1-param (memory pc-sym regs &optional a-write)
  (let* ((pc-val (symbol-value pc-sym))
         (op (elt memory pc-val))
         (modes (decode-mode op))
         (a (elt memory (+ 1 pc-val)))
         (mode (nth 0 modes))
         (a (decode-param-with-mode memory mode a regs a-write)))
    (list a)))

(defun decode-op-2-param (memory pc-sym regs &optional a-write b-write)
  (let* ((pc-val (symbol-value pc-sym))
         (op (elt memory pc-val))
         (modes (decode-mode op))
         (a (elt memory (+ 1 pc-val)))
         (b (elt memory (+ 2 pc-val)))
         (a (decode-param-with-mode memory (nth 0 modes) a regs a-write))
         (b (decode-param-with-mode memory (nth 1 modes) b regs b-write)))
    (list a b)))

(defun decode-op-3-param (memory pc-sym regs &optional a-write b-write c-write)
  (let* ((pc-val (symbol-value pc-sym))
         (op (elt memory pc-val))
         (modes (decode-mode op))
         (a (elt memory (+ 1 pc-val)))
         (b (elt memory (+ 2 pc-val)))
         (c (elt memory (+ 3 pc-val)))
         (a (decode-param-with-mode memory (nth 0 modes) a regs a-write))
         (b (decode-param-with-mode memory (nth 1 modes) b regs b-write))
         (c (decode-param-with-mode memory (nth 2 modes) c regs c-write)))
    (list a b c)))

(defun do-op-add (memory pc-sym regs)
  (let ((operands (decode-op-3-param memory pc-sym regs nil nil t)))
    (aset memory (nth 2 operands) (+ (nth 0 operands) (nth 1 operands)))
    (set pc-sym (+ 4 (symbol-value pc-sym)))))

(defun do-op-mul (memory pc-sym regs)
  (let ((operands (decode-op-3-param memory pc-sym regs nil nil t)))
    (aset memory (nth 2 operands) (* (nth 0 operands) (nth 1 operands)))
    (set pc-sym (+ 4 (symbol-value pc-sym)))))

(defun do-op-in (memory pc-sym in-buffer regs)
  (let ((operands (decode-op-1-param memory pc-sym regs t))
        (b (if (queue-empty in-buffer)
               (throw 'waiting-input nil)
             (queue-dequeue in-buffer))))
    (aset memory (nth 0 operands) b))
  (set pc-sym (+ 2 (symbol-value pc-sym))))

(defun do-op-out (memory pc-sym out-buffer regs)
  (let* ((operands (decode-op-1-param memory pc-sym regs))
         (b (nth 0 operands)))
    (queue-enqueue out-buffer b))
  (set pc-sym (+ 2 (symbol-value pc-sym))))

(defun do-op-jt (memory pc-sym regs)
  (let ((operands (decode-op-2-param memory pc-sym regs)))
    (if (not (zerop (nth 0 operands)))
        (set pc-sym (nth 1 operands))
      (set pc-sym (+ 3 (symbol-value pc-sym))))))

(defun do-op-jf (memory pc-sym regs)
  (let ((operands (decode-op-2-param memory pc-sym regs)))
    (if (zerop (nth 0 operands))
        (set pc-sym (nth 1 operands))
      (set pc-sym (+ 3 (symbol-value pc-sym))))))

(defun do-op-lt (memory pc-sym regs)
  (let ((operands (decode-op-3-param memory pc-sym regs nil nil t)))
    (if (< (nth 0 operands) (nth 1 operands))
        (aset memory (nth 2 operands) 1)
      (aset memory (nth 2 operands) 0)))
  (set pc-sym (+ 4 (symbol-value pc-sym))))

(defun do-op-eq (memory pc-sym regs)
  (let ((operands (decode-op-3-param memory pc-sym regs nil nil t)))
    (if (= (nth 0 operands) (nth 1 operands))
        (aset memory (nth 2 operands) 1)
      (aset memory (nth 2 operands) 0)))
  (set pc-sym (+ 4 (symbol-value pc-sym))))

(defun do-op-rel (memory pc-sym regs)
  (let* ((operands (decode-op-1-param memory pc-sym regs))
         (b (nth 0 operands)))
    (setf (nth 1 regs) (+ (nth 1 regs) b)))
  (set pc-sym (+ 2 (symbol-value pc-sym))))
  
(defun int-code-create (memory in-buffer out-buffer)
  ;; :memory :in-buffer :out-buffer :memory-len :regs
  (list memory
        in-buffer
        out-buffer
        (length memory)
        ;; :pc :rel-base
        (list 0 0)))

(defun int-code-run (int-code)
  (let* ((memory (nth 0 int-code))
         (in-buffer (nth 1 int-code))
         (out-buffer (nth 2 int-code))
         (memory-length (nth 3 int-code))
         (regs (nth 4 int-code))
         (pc (nth 0 regs))
         (cur-op (decode-inst (elt memory pc))))
    (while (/= cur-op op-end)
      (cond ((= cur-op op-add) (do-op-add memory 'pc regs))
            ((= cur-op op-mul) (do-op-mul memory 'pc regs))
            ((= cur-op op-in) (do-op-in memory 'pc in-buffer regs))
            ((= cur-op op-out) (do-op-out memory 'pc out-buffer regs))
            ((= cur-op op-jt) (do-op-jt memory 'pc regs))
            ((= cur-op op-jf) (do-op-jf memory 'pc regs))
            ((= cur-op op-lt) (do-op-lt memory 'pc regs))
            ((= cur-op op-eq) (do-op-eq memory 'pc regs))
            ((= cur-op op-rel) (do-op-rel memory 'pc regs))
            (t (throw 'invalid-op-code cur-op)))
      (setf (nth 0 regs) pc)
      (setq cur-op (decode-inst (elt memory pc)))))
  t)

(let* ((in-buffer (queue-create))
       (out-buffer (queue-create))
       (int-code (int-code-create (load-program 1000) in-buffer out-buffer)))
  (queue-enqueue in-buffer 2)
  (int-code-run int-code)
  (insert (message "\n;; %S" out-buffer)))
;; #s(queue (58534) (58534))
;; #s(queue (2870072642) (2870072642))

(defun test-op-rel (input expected)
  (let* ((in-buffer (queue-create))
         (out-buffer (queue-create))
         (int-code (int-code-create (vconcat input (make-vector 100 0)) in-buffer out-buffer)))
    (queue-enqueue in-buffer 1)
    (int-code-run int-code)
    (let ((result (queue-dequeue out-buffer)))
      (if (/= result expected)
          (throw 'test-failed (format "Expected %d but got %d." expected result))))))

;; Test cases for relative mode 
(progn 
  (test-op-rel (list 109 -1 4 1 99) -1)
  (test-op-rel (list 109 -1 104 1 99) 1)
  (test-op-rel (list 109 -1 204 1 99) 109)
  (test-op-rel (list 109 1 9 2 204 -6 99) 204)
  (test-op-rel (list 109 1 109 9 204 -6 99) 204)
  (test-op-rel (list 109 1 209 -1 204 -106 99) 204)

  ;; rel 1 ; set rel to 1
  ;; in (3) ; read input and write to address 3
  ;; out (rel+2) ; output rel+2 (3)
  ;; end
  (test-op-rel (list 109 1 3 3 204 2 99) 1)

  ;; rel 1 ; set rel to 1
  ;; in (rel+2) ; read input and write to address rel+2 (3)
  ;; out (rel+2) ; output rel+2 (3)
  ;; end
  (test-op-rel (list 109 1 203 2 204 2 99) 1))
