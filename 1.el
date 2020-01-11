(defun get-fuel-from-mass (mass)
  (max 0 (- (floor (/ mass 3)) 2)))

(defun get-list-of-module-mass ()
  (with-temp-buffer
    (insert-file-contents "1.txt")
    (mapcar 'string-to-number (split-string (buffer-string) "\n" t))))

(defun sum (list)
  (apply '+ list))

(defun get-total-fuel-from-mass (mass)
  (let* ((fuel (get-fuel-from-mass mass))
         (sum fuel))
    (while (> fuel 0)
      (setq fuel (get-fuel-from-mass fuel))
      (setq sum (+ sum fuel)))
    sum))

(defun get-total-fuel-from-all-mass ()
  (sum (mapcar 'get-total-fuel-from-mass (get-list-of-module-mass))))

(insert (format "%d" (get-total-fuel-from-all-mass)))
