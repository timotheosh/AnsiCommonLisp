;; 1.
;;   (a)
;; Order of operations: subtract 1 from 5, then add 3 and 7 and add the results.
(+ (- 5 1) (+ 3 7))

;;   (b)
;; Returns a list of (1 5)
(list 1 (+ 2 3))

;;   (c)
;; listp 1 returns false so returns the sum of 3 and 4.
(if (listp 1) (+ 1 2) (+ 3 4))

;;   (d)
;; if listp 3 returns false, and thus returns a list of (nil 3)
(list (and (listp 3) t) (+ 1 2))

;; 2.
;; Give three distinct cons expressions that return (a b c).
(cons 'a '(b c))
(cons 'a (cons 'b '(c)))
(cons 'a (cons 'b (cons 'c nil)))

;; 3.
;; Using car and cdr, define a function to return the fourth element of a
;; list.
(car (cdr (cdr (cdr '(1 2 3 4 5)))))

;; 4.
;; Define a function that takes two arguments and returns the greater
;; of the two.
;;
;; The chapter does not cover the use of cond, but I thought it was
;; better for accomodating when the two values were equal.
(defun greater (x y)
  (cond ((> x y)
         x)
        ((> y x)
         y)
        (t nil)))

;; 5.
;; What do these functions do?
;;
;;   (a) Recursively goes through a list and returns true is a list is
;;   sent with a nil value.
(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x)))))

;;   (b) If y is empty lis (nil) return nil, otherwise check if x is
;;   equal to the car of y, return 0 if it is. Otherwise, recurse
;;   myster with c and the cdr of y and assign that to z and return
;;   the sum of z and 1 if both the sum and z are not nil.
(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
          0
          (let ((z (mystery x (cdr y))))
            (and z (+ z 1))))))

;; 6.
;; What could occur in place of the x in each of the following exchanges?
;;
;;  (a)
;;  (car (x (cdr '(a (b c) d))))
;;  B
(car (car (cdr '(a (b c) d))))

;;  (b)
;;  (x 13 (/ 1 0))
;;  13
(or 13 (/ 1 0))

;;  (c)
;;  (x #'list 1 nil)
;;  (1)
(apply #'list 1 nil)

;; 7.
;; Using only operators introduced in this chapter, define a function
;; that takes a list as an argument and returns true if one of its
;; elements is a list.
(defun exc7 (x)
  ":param x a list."
  (if (null x)
      nil
      (if (listp (car x))
          t
          (exc7 (cdr x)))))

;; 8.
;; Give iterative and recursive definitions of a function that
;;    (a) takes a positive integer and prints that many dots.
;;    recursive
(defun recurse-dots-exc8 (x)
  (if (> x 0)
      (progn
        (format t ".")
        (recurse-dots-exc8 (- x 1)))
      nil))
;;    iterative
(defun iterative-dots-exc8 (x)
  (dotimes (i x)
    (format t ".")))
;;    (b) takes a list and returns the number of times the symbol a
;;        occurs in it.
;;    recursive
(defparameter *anum* 0)
(defun recurse-anum-exc8 (x)
  (if (eql 'a (car x))
      (setf *anum* (+ *anum* 1)))
  (if (not (null (cdr x)))
      (recurse-anum-exc8 (cdr x)))
  *anum*)
;;    iterative
(defun iterative-anum-exc8 (x)
  (let ((count 0))
    (loop for y in x
       do
         (if (eql 'a y)
             (setf count (+ count 1))))
    count))

;; 9.
;; A friend is trying to write a function that returns the sum of all
;; the non-nil elements in a list. He has written two versions of this
;; function, and neither of them work. Explain what's wrong with each,
;; and give a correct version:
;;
;; (a) (defun summit (lst)
;;       (remove nil lst) <- this does not create a new list!
;;       (apply #' + lst))
(defun summit1 (lst)
  (let ((x (remove nil lst)))
    (apply #'+ x)))

;; (b) (defun summit (lst)
;;       (let ((x (car lst)))
;;         (if (null x)
;;            (summit (cdr lst)) <- infinite loop because an empty list just
;;                                  recurses instead of returning a value
;;            (+ x (summit (cdr lst))))))
(defun summit2 (lst)
  (if (null lst)
      0
      (let ((x (car lst)))
        (if (and (> (length lst) 0)
                 (null x))
            (summit2 (cdr lst))
            (+ x (summit2 (cdr lst)))))))
