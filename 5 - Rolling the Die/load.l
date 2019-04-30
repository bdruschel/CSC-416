;;;; PROGRAMMING CHALLENGE: ROLLING THE DICE
;; Brandon Druschel - 10/26/18

;; Batch file for pre-loading die.l and its attributes

( load "die.l" )
( setf d6 (make-instance 'die) )
( setf d4 (make-instance 'die :faces 4) )
( setf d12 (make-instance 'die :faces 12) )

(to-string d4)
(to-string d6)
(to-string d12)