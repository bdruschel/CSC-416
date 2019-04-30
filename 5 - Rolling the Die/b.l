;;;; PROGRAMMING CHALLENGE: ROLLING THE DICE
;; Brandon Druschel - 10/26/18

;; Batch file for pre-loading PC5 files and functions

;; DIE.L
( load "die.l" )
;; ( setf d6 (make-instance 'die) )
;; ( setf d4 (make-instance 'die :faces 4) )
;; ( setf d12 (make-instance 'die :faces 12) )

;; ACCUM.L
( load "accum.l" )

;; SNOC
( defun snoc ( lo ll )  ;; lo = lisp object, ll = list
   ( cond
      (( null ll )
         ( list lo )
      )
      ( t
         ( cons ( car ll ) ( snoc lo ( cdr ll )))
      )
   )
)