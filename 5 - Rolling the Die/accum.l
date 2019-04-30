;;;; PROGRAMMING CHALLENGE: ROLLING THE DICE
;; Brandon Druschel - 10/26/18

;; TASK 2 - ACCUMULATOR MODEL

( defclass accumulator ()
   (
      ( value :accessor value :initarg :value :initform 0 )
   )
)

( defmethod to-string (( a accumulator ))
   ( format nil "[~A]" 
      ( value a )
   )
)

( defmethod inc ((a accumulator) (n number))
   ( setf (value a) ( + n (value a) ) )
   nil
)

( defmethod reset ((a accumulator))
   ( setf (value a ) 0 )
   nil
)

