(defun load-program ()
  (with-temp-buffer
    (insert-file-contents "2.txt")
    (vconcat
     (mapcar 'string-to-number (split-string (buffer-string) "," t)))))

(defun do-op-add (memory pc-sym)
  (let* ((pc-val (symbol-value pc-sym))
         (a (elt memory (+ 1 pc-val)))
         (b (elt memory (+ 2 pc-val)))
         (c (elt memory (+ 3 pc-val))))
    (aset memory c (+ (elt memory a) (elt memory b)))
    (set pc-sym (+ 4 pc-val))))

(defun do-op-mul (memory pc-sym)
  (let* ((pc-val (symbol-value pc-sym))
         (a (elt memory (+ 1 pc-val)))
         (b (elt memory (+ 2 pc-val)))
         (c (elt memory (+ 3 pc-val))))
    (aset memory c (* (elt memory a) (elt memory b)))
    (set pc-sym (+ 4 pc-val))))

(defun run-int-code (noun verb)
  "Run the Intcode program with noun and verb pairs as parameter
  placed in addresses of 1 and 2, and return the value in address
  of 0 when the program finished"
  (let* ((memory (load-program))
         (memory-length (length memory))
         (pc 0) ;; program counter
         (cur-op (elt memory pc))) ;; fetch first instruction
    (defconst op-add 1)
    (defconst op-mul 2)
    (defconst op-end 99)
    (aset memory 1 noun)
    (aset memory 2 verb)
    (while (/= cur-op op-end)
      (cond ((= cur-op op-add) (do-op-add memory 'pc))
            ((= cur-op op-mul) (do-op-mul memory 'pc))
            (t (throw 'invalid-op-code cur-op)))
      (setq cur-op (elt memory pc)))
    (elt memory 0)))

(defun run ()
  "Find the noun-verb pair that results in the expected-result
  when the Intcode program runs"
  (defconst expected-result 19690720)
  (let* ((noun 0)
         (verb 0)
         (found-p nil))
    (while (and (not found-p) (< noun 99))
      (setq verb 0)
      (while (and (not found-p) (< verb 99))
        (if (eq (run-int-code noun verb) expected-result)
            (setq found-p t)
          (setq verb (+ 1 verb))))
      (when (not found-p) (setq noun (+ 1 noun))))
    (when (not found-p) (throw 'not-found nil))
    (+ verb (* noun 100))))

(insert (message "%d" (run)))
