;;;; PROGRAMMING CHALLENGE: ROLLING THE DICE
;; Brandon Druschel - 10/26/18

;; TASK 1 - DIE MODEL

( defclass die ()
   (  ( num-faces :accessor die-num-faces :initarg :faces :initform 6)
      ( top-face :accessor die-top-face :initarg :top-face :initform 1)
      ( history :accessor die-history :initform () )
   )
)

( defmethod display (( d die ))
   ( format t "[~A, ~A, ~A]" 
      ( die-num-faces d ) ( die-top-face d ) ( die-history d )
   )
   nil
)

( defmethod to-string (( d die ))
   ( format nil "[~A, ~A, ~A]" 
      ( die-num-faces d ) ( die-top-face d ) ( die-history d )
   )
)

( defmethod roll (( d die ))
   ( setf ( die-top-face d ) ( + 1 (random (die-num-faces d)) ) )
   ( setf 
      ( die-history d )
      ( snoc ( die-top-face d ) ( die-history d ) )
   )
   nil
)

( defmethod forget ((d die))
   ( setf ( die-history d ) () )
   nil
)


;; TASK 3 - ROLL-FOR-PAIR

( defmethod roll-for-pair ( (d1 die) (d2 die) )
   ( roll d1 )
   ( roll d2 )
   ( if ( not ( equal (die-top-face d1) (die-top-face d2) ) )
      ( roll-for-pair d1 d2 )
   )
   nil
)


;; TASK 4 - ROLL-FOR-SUM

( defmethod roll-for-sum ( (d1 die) (d2 die) n )
   ( roll d1 )
   ( roll d2 )
   ( if ( not ( equal ( + (die-top-face d1) (die-top-face d2) ) n ) )
      ( roll-for-sum d1 d2 n )
   )
   nil
)


;; TASK 5 - EMPIRICAL PROBLEM SOLVING

( defmethod roll-count-standard-pair ()
   ( setf d6-1 (make-instance 'die) )
   ( setf d6-2 (make-instance 'die) ) 
   ( roll-for-pair d6-1 d6-2 )
   ( list-length (die-history d6-1) )
)

( defmethod roll-count-d3-d12-pair ()
   ( setf d3 (make-instance 'die :faces 3) )
   ( setf d12 (make-instance 'die :faces 12) ) 
   ( roll-for-pair d3 d12 )
   ( list-length (die-history d3) )
)

( defmethod roll-count-standard-sum ( n )
   ( setf d6-1 (make-instance 'die) )
   ( setf d6-2 (make-instance 'die) ) 
   ( roll-for-sum d6-1 d6-2 n )
   ( list-length (die-history d6-1) )
)

( defmethod roll-count-d3-d12-sum ( n )
   ( setf d3 (make-instance 'die :faces 3) )
   ( setf d12 (make-instance 'die :faces 12) ) 
   ( roll-for-sum d3 d12 n )
   ( list-length (die-history d3) )
)

( defmethod roll-count-7-11-pair ( (d1 die) (d2 die) )
   ( roll d1 )
   ( roll d2 )
   ( if ( not ( equal (die-top-face d1) (die-top-face d2) ) )
      ( if ( not ( equal ( + (die-top-face d1) (die-top-face d2) ) 7 ) )
         ( if ( not ( equal ( + (die-top-face d1) (die-top-face d2) ) 11 ) )
            ( roll-count-7-11-pair d1 d2 )
         )
      )
   )
   ( list-length (die-history d1) )
)