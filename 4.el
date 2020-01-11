(defun is-six-digit (pass)
  "Return true if the string value is six digit"
  (= (length pass) 6))

(defun is-value-in-range (pass minv maxv)
  "Return true if the string value is within range of minv maxv"
  (and (>= (string-to-number pass) minv)
       (<= (string-to-number pass) maxv)))

(defun is-adjacent-digits-are-max-two (pass)
  "Return true if two adjacent digits are the same"
  (let ((i 0)
        (found))
    (while (< i (- (length pass) 1))
      ;; Rules:
      ;;
      ;; - Current and next chars are equal
      ;;
      ;; - it is the first char or the one before is not equal to
      ;; current one
      ;; 
      ;; - it is the second char before the last or the two after is
      ;; not equal to curret one
      (if (and (equal (substring pass i (+ 1 i))
                      (substring pass (+ 1 i) (+ 2 i)))
               (or (= i 0)
                   (not (equal (substring pass i (+ 1 i))
                               (substring pass (- i 1) i))))
               (or (>= i (- (length pass) 2))
                   (not (equal (substring pass i (+ 1 i))
                               (substring pass (+ 2 i) (+ 3 i))))))
          (setq found t))
      (setq i (+ 1 i)))
    found))

(defun digits-never-decrease (pass)
  "Return true if digits of the string pass never decrease from
  left to right"
  (let* ((pass-list (string-to-list pass))
         (prev (nth 0 pass-list))
         (invalid))
    (dolist (cur-char (cdr pass-list))
      (if (< cur-char prev)
          (setq invalid t))
      (setq prev cur-char))
    (not invalid)))

(defun is-valid-password (pass minv maxv)
  "Return true if the string pass fits all criteria"
  (and (is-six-digit pass)
       (is-value-in-range pass minv maxv)
       (is-adjacent-digits-are-max-two pass)
       (digits-never-decrease pass)))

(defun how-many-password-fit-rule (minv maxv)
  "Return the number of valid passwords that fit the criteria"
  (let ((i minv)
        (counter 0))
    (while (< i maxv)
      (if (is-valid-password (number-to-string i) minv maxv)
          (setq counter (+ 1 counter)))
      (setq i (+ 1 i)))
    counter))

(insert (message "%d" (how-many-password-fit-rule 382345 843167)))290
