(require 'seq)
(require 'subr-x)
(require 'dash)

(defun make-node (name)
  (list name (list)))

(defun load-program ()
  (with-temp-buffer
    (insert-file-contents "6.txt")
    (split-string (buffer-string) "\n" t)))

(defun build-orbit-pairs ()
  (let ((orbit-map (make-hash-table :test 'equal)))
    (dolist (item (load-program))
      (let* ((tuple (split-string item ")"))
             (first (nth 0 tuple))
             (second (nth 1 tuple)))
        (puthash second first orbit-map)))
    orbit-map))

(defun build-neighbours-table (orbit-map)
  (let ((neighbours (make-hash-table :test 'equal))
        (keys (get-unique-planets orbit-map)))
    (dolist (key keys)
      (puthash key (get-neighbours orbit-map key) neighbours))
    neighbours))

(defun get-neighbours (orbit-map planet)
  (let ((neighbours))
    (maphash (lambda (k v)
               (if (equal k planet)
                   (push v neighbours))
               (if (equal v planet)
                   (push k neighbours)))
             orbit-map)
    neighbours))

(defun get-unique-planets (orbit-map)
  (-distinct (append (hash-table-keys orbit-map) (hash-table-values orbit-map))))

(defun build-shortest-path-tree (orbit-map src dest)
  (let ((distances (make-hash-table :test 'equal))
        (visited (make-hash-table :test 'equal))
        (neighbours-map (build-neighbours-table orbit-map))
        (keys (get-unique-planets orbit-map)))
    ;; Initialize all distances as infinite except source as closest
    (dolist (key keys)
      (if (equal src key)
          (puthash key 0 distances)
        (puthash key most-positive-fixnum distances)))
    ;; For every vertex
    (dolist (key keys)
      ;; Find the minimum distance
      (let ((min-key)
            (min-value most-positive-fixnum))
        (maphash (lambda (k v)
                   (if (and (< v min-value) (null (gethash k visited)))
                       (progn (setq min-value v) (setq min-key k))))
                 distances)
        (puthash min-key t visited)
        ;; Update dist value of adjacents
        (let ((neighbours (gethash min-key neighbours-map)))
          (dolist (neighbour neighbours)
            (let ((not-visited (not (gethash neighbour visited))))
              (if not-visited
                  (puthash neighbour (+ min-value 1) distances)))))
        ))
    ;; Because source and target is always in orbit around its
    ;; immediate neighbour, subtract 2
    (max 0 (- (gethash dest distances) 2))))

(benchmark-run (build-shortest-path-tree (build-orbit-pairs) "YOU" "SAN"))
