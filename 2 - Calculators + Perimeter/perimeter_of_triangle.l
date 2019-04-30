;; Brandon Druschel - 9/19/18
;;;; TASK I: DISTANCE BETWEEN TWO POINTS

(defun distance (x1 y1 x2 y2)
   (setf change-x (expt (- x2 x1) 2))
   (setf change-y (expt (- y2 y1) 2))
   (setf dist (sqrt (+ change-x change-y)))
   dist
)

;;;; TASK II: PERIMETER OF TRIANGLE

(defun perimeter (x1 y1 x2 y2 x3 y3)
   (setf a (distance x1 y1 x2 y2))
   (setf b (distance x2 y2 x3 y3))
   (setf c (distance x3 y3 x1 y1))
   (setf perim (+ a b c))
   perim
)
