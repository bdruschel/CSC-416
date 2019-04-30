;; PROGRAMMING CHALLENGE: M&C STATE SPACE PROBLEM SOLVER 
;; Brandon Druschel
;; Original Program by Craig Graci
;; 11/12/18

;; PROGRAM DESCRIPTION
;
; Missionaries and Cannibals Problem Solver
;
; This program is a state space problem solver for the classic
; "Missionaries and Cannibals" probem. An explicit state
; space tree is grown in concert with breadth first search
; for a solution.
;
; Banks are represented as a 3-slot class consisting of 
; missionaries, cannibals, and boat.
;
; States are represented as a 2-slot class consisting of
; left-bank and right-bank.
;
; Operators are represented as a 3-slot class consisting of a name,
; a precondition, and a description.
;
; Nodes are represented as a 4-slot class consisting of
; a state, a parent node, a move (state space operator), and a name.

( setf *trace-search* t )
( setf *goal* nil )

; ------------------------------------------------------------------
; Modelling a bank

( defclass bank ()
   (
      ( missionaries :accessor bank-missionaries :initarg :missionaries )
      ( cannibals :accessor bank-cannibals :initarg :cannibals )
      ( boat :accessor bank-boat :initarg :boat )
   )
)

( defmethod display ( (b bank) ) 
   ( format t "MISSIONARIES=~A CANNIBALS=~A BOAT=~A~%"
          (bank-missionaries b)
          (bank-cannibals b)
          (bank-boat b)
   )
)

(defmethod copy ((b bank))
  ( make-instance 'bank 
      :missionaries (copy-list (bank-missionaries b))
      :cannibals (copy-list (bank-cannibals b))
      :boat (bank-boat b))
)

( defmethod equal-bank-p ( (b1 bank) (b2 bank) )
   ( and 
     ( equal (bank-missionaries b1) (bank-missionaries b2) ) 
     ( equal (bank-cannibals b1) (bank-cannibals b2) ) 
     ( equal (bank-boat b1) (bank-boat b2) )
   )
)


; ------------------------------------------------------------------
; Modelling a state

( defclass state ()
   (  
      ( left-bank :accessor state-left-bank :initarg :left-bank )
      ( right-bank :accessor state-right-bank :initarg :right-bank )
   )
)

( defmethod display ( ( s state ) )
   ( display ( state-left-bank s ) )
   ( display ( state-right-bank s ) )
)

( defmethod copy ( (s state) )
   ( make-instance 'state
      :left-bank  (copy (state-left-bank s))
      :right-bank (copy (state-right-bank s))
   )
) 


; ------------------------------------------------------------------
; Modelling a node

( defclass node ()
   (
      ( name :accessor node-name :initarg :name )
      ( state :accessor node-state :initarg :state )
      ( parent :accessor node-parent :initarg :parent )
      ( operator :accessor node-operator :initarg :operator )
   )
)

( defmethod display ( ( n node ) )
   ( cond
      ( ( not *goal* )
         ( if ( not ( rootp n ) )
            ( princ "N" )
         )
         ( princ (node-name n) ) ( princ " " )
         ( if ( not ( rootp n ) )
            ( let () 
               ( display ( node-operator n ) ) 
            )
         )
         ( terpri )
         ( display ( node-state n ) )
      )
      ( t
         ( if ( not ( rootp n ) )
            ( let () 
               ( display ( node-operator n ) ) 
            )
         )
         ( terpri )
      )
   )
   nil
)   


; ------------------------------------------------------------------
; Modelling an operator

( defclass operator ()
   (
      ( name :accessor operator-name :initarg :name )
      ( precondition :accessor operator-precondition :initarg :precondition )
      ( description :accessor operator-description :initarg :description )
   )
)

( defmethod display ( (o operator) )
   ( if (not *goal*)
      ( format t "~A" (operator-name o) )
      ( format t "~A" (operator-description o) )
   )
)


; ------------------------------------------------------------------
; Modelling a name-generator

( defclass name-generator ()
   ( ( prefix :accessor name-generator-prefix :initarg :prefix :initform "name" )
      ( nr :accessor name-generator-nr :initform 0 )
   )
)

( defmethod next ( (ng name-generator) )
   ( concatenate 'string
      ( name-generator-prefix ng )
      ( write-to-string ( name-generator-nr ng ) )
   )
   ( setf ( name-generator-nr ng ) ( + 1 ( name-generator-nr ng ) ) )
)


; ------------------------------------------------------------------

( defmethod rootp ( ( n node ) )
   ( eq ( node-name n ) 'root )
)

( defmethod goalp ( (s state) )
   ( and 
      ( left-state-ok-p (state-left-bank s) )
      ( right-state-ok-p (state-right-bank s) )
   )
)

( defmethod feast-state-p ( (s state) )
   ( or
      ( and 
         ( < 0 (list-length (bank-missionaries (state-left-bank s))) )
         ( > (list-length (bank-cannibals (state-left-bank s))) 
            (list-length (bank-missionaries (state-left-bank s))) 
         )
      )
      ( and
         ( < 0 (list-length (bank-missionaries (state-right-bank s))) )
         ( > (list-length (bank-cannibals (state-right-bank s))) 
            (list-length (bank-missionaries (state-right-bank s))) 
         )
      )      
   )
)

( defmethod left-state-ok-p ( (b bank) )
   ( and
      ( equal (bank-missionaries b) '() )
      ( equal (bank-cannibals b) '() )
      ( equal (bank-boat b) nil )
   )
)

( defmethod right-state-ok-p ( (b bank) )
   ( and
      ( equal (bank-missionaries b) '(m m m) )
      ( equal (bank-cannibals b) '(c c c) )
      ( equal (bank-boat b) 'b )
   )
)

( defmethod exploredp ( (e-node node) )
   ( member-node-p e-node *explored* )
)

( defmethod member-node-p ( (n node) (l list) )
   ( cond
      ( (null l) nil )
      ( ( equal-node-p n (first l) ) 
         t 
      )
      ( T 
         ( member-node-p n (rest l) ) 
      )
   )
)

( defmethod equal-node-p ( (n1 node) (n2 node) )
   ( and
      ( equal-bank-p (state-left-bank (node-state n1)) (state-left-bank (node-state n2)) )
      ( equal-bank-p (state-right-bank (node-state n1)) (state-right-bank (node-state n1)) )
   )
)

( defmethod applicablep ( (op operator) (e-node node) )
   ( cond
      ( ( eq (operator-name op) 'move-lr-c )
         ( applicable-move-lr-c e-node 0 )
      )
      ( ( eq (operator-name op) 'move-lr-cc )
         ( applicable-move-lr-c e-node 1 )
      )
      ( ( eq (operator-name op) 'move-lr-m )
         ( applicable-move-lr-m e-node 2 )
      )
      ( ( eq (operator-name op) 'move-lr-mm )
         ( applicable-move-lr-m e-node 1 )
      )
      ( ( eq (operator-name op) 'move-rl-c )
         ( applicable-move-rl-c e-node 0 )
      )
      ( ( eq (operator-name op) 'move-rl-cc )
         ( applicable-move-rl-c e-node 1 )
      )
      ( ( eq (operator-name op) 'move-rl-m )
         ( applicable-move-rl-m e-node 0 )
      )
      ( ( eq (operator-name op) 'move-rl-mm )
         ( applicable-move-rl-m e-node 1 )
      )
      ( ( eq (operator-name op) 'move-rl-mc )
         ( applicable-move-rl-mc e-node )
      )
      ( ( eq (operator-name op) 'move-lr-mc )
         ( applicable-move-lr-mc e-node )
      )
   )
)

( defmethod applicable-move-lr-c ((e-node node) i) ;; Cannibal L -> R applicable?
   ( and
      ( > ( length (bank-cannibals (state-left-bank (node-state e-node))) ) i )
      ( bank-boat (state-left-bank (node-state e-node)) )
   )
)

( defmethod applicable-move-lr-m ((e-node node) i) ;; Missionary L -> R applicable?
   ( and
      ( > ( length (bank-missionaries (state-left-bank (node-state e-node))) ) i )
      ( bank-boat (state-left-bank (node-state e-node)) )
   )
)

( defmethod applicable-move-rl-c ((e-node node) i) ;; Cannibal L <- R applicable?
   ( and
      ( > ( length (bank-cannibals (state-right-bank (node-state e-node))) ) i )
      ( bank-boat (state-right-bank (node-state e-node)) )
   )
)

( defmethod applicable-move-rl-m ((e-node node) i) ;; Missionary L <- R applicable?
   ( and
      ( > ( length (bank-missionaries (state-right-bank (node-state e-node))) ) i )
      ( bank-boat (state-right-bank (node-state e-node)) )
   )
)

( defmethod applicable-move-rl-mc ( (e-node node) )
   ( and
      ( applicable-move-rl-c e-node 0)
      ( applicable-move-rl-m e-node 0)
   )
)

( defmethod applicable-move-lr-mc ( (e-node node) )
   ( and
      ( applicable-move-lr-c e-node 0)
      ( applicable-move-lr-m e-node 0)
   )
)

( defun remove-n ( list n )
  (subseq list n)
)

( defun add-n ( list n elem )
   ( if ( <= n 0 )
      list
      ( cons elem (add-n list (1- n) elem) )
   )
)

( defmethod move-lr ((s state) &key (M 0) (C 0))
   ( let* 
      (  (new-state  (copy s))
         (left-bank  (state-left-bank new-state))
         (right-bank (state-right-bank new-state))
      )
      ;; remove from left bank
      ( setf (bank-boat left-bank) nil )
      ( setf (bank-missionaries left-bank ) (remove-n (bank-missionaries left-bank) M) )
      ( setf (bank-cannibals left-bank) (remove-n (bank-cannibals left-bank) C) )
      ;; add to right bank
      ( setf (bank-boat right-bank) 'B)
      ( setf (bank-missionaries right-bank) (add-n (bank-missionaries right-bank) M 'M) )
      ( setf (bank-cannibals right-bank) (add-n (bank-cannibals right-bank) C 'C) )
      ;; return new state
      new-state
   )
)

( defmethod move-rl ( (s state) &key (M 0) (C 0) )
   ( let* 
      (  ( new-state  (copy s) )
         ( left-bank  (state-left-bank new-state) )
         ( right-bank (state-right-bank new-state) ) 
      )
      ;; remove from right bank
      ( setf (bank-boat right-bank ) nil )
      ( setf (bank-missionaries right-bank ) (remove-n (bank-missionaries right-bank) M) )
      ( setf (bank-cannibals right-bank ) (remove-n (bank-cannibals right-bank) C) )
      ;; add to left bank
      ( setf (bank-boat left-bank) 'B )
      ( setf (bank-missionaries left-bank ) (add-n (bank-missionaries left-bank) M 'M) )
      ( setf (bank-cannibals left-bank) (add-n (bank-cannibals left-bank) C 'C) )
      ;; return new state
      new-state
   )
)


; ------------------------------------------------------------------
; Main method

( defmethod mc ()
   ( establish-operators )
   ( setup )
   ( solve )
)


; ------------------------------------------------------------------
; The setup

( defmethod setup ( &aux root lb rb istate )
   ;; establish root node
   ( setf lb ( make-instance 'bank :missionaries '(m m m) :cannibals '(c c c) :boat 'b ) )
   ( setf rb ( make-instance 'bank :missionaries '() :cannibals '() :boat nil ) )
   ( setf istate ( make-instance 'state :left-bank lb :right-bank rb ) )
   ( setf root ( make-instance 'node :state istate :name 'root ) )
   ;; initialize list of unexplored nodes
   ( setf *unexplored* ( list root ) )
   ;; initialize list of explored nodes
   ( setf *explored* () )
   ;; get ready to create good names
   ( setf *ng* ( make-instance 'name-generator :prefix "N" ) )
   nil
)

( defmethod establish-operators () 
   ( setf *move-lr-c* 
      ( make-instance 'operator
         :name 'move-lr-c
         :precondition "left bank has a cannibal and a boat."
         :description "Move a cannibal + boat from left bank to right bank."
      )
   )
   ( setf *move-lr-cc* 
      ( make-instance 'operator
         :name 'move-lr-cc
         :precondition "left bank has two cannibals and a boat"
         :description "Move two cannibals + boat from left bank to right bank."
      )
   )
   ( setf *move-lr-m* 
      ( make-instance 'operator
         :name 'move-lr-m
         :precondition "left bank has a missionary and a boat"
         :description "Move a missionary + boat from left bank to right bank."
      )
   )
   ( setf *move-lr-mm* 
      ( make-instance 'operator
         :name 'move-lr-mm
         :precondition "left bank has a two missionaries and a boat"
         :description "Move two missionaries + boat from left bank to right bank."
      )
   )
   ( setf *move-rl-c* 
      ( make-instance 'operator
         :name 'move-rl-c
         :precondition "right bank has a cannibal and a boat"
         :description "Move a cannibal + boat from right bank to left bank."
      )
   )
   ( setf *move-rl-cc* 
      ( make-instance 'operator
         :name 'move-rl-cc
         :precondition "right bank has a two cannibals and a boat"
         :description "Move two cannibals + boat from right bank to left bank"
      )
   )
   ( setf *move-rl-m* 
      ( make-instance 'operator
         :name 'move-rl-m
         :precondition "right bank has a missionary and a boat"
         :description "Move a missionary + boat from right bank to left bank"
      )
   )
   ( setf *move-rl-mm* 
      ( make-instance 'operator
         :name 'move-rl-mm
         :precondition "right bank has a missionary and a boat"
         :description "Move two missionaries + boat from right bank to left bank"
      )
   )
   ( setf *move-rl-mc* 
      ( make-instance 'operator
         :name 'move-rl-mc
         :precondition "right bank has a missionary, cannibal and a boat"
         :description "Move a missionary and cannibal + boat from right bank to left bank"
      )
   )
   ( setf *move-lr-mc* 
      ( make-instance 'operator
         :name 'move-lr-mc
         :precondition "left bank has a missionary, cannibal and a boat"
         :description "Move a missionary and cannibal + boat from left bank to right bank"
      )
   )
   ( setf *operator-list*
      ( list *move-lr-c* *move-lr-cc* *move-lr-m* *move-lr-mm* *move-rl-c* *move-rl-cc* 
      *move-rl-m* *move-rl-mm* *move-rl-mc* *move-lr-mc*)
   )
   nil
)


; ------------------------------------------------------------------
; Breadth first search

( defmethod solve ( &aux kids e-node )
   ( if *trace-search*
      ( let ()
         ( terpri ) ( write-line ">>> Solve" ) ( terpri )
         ( display-explored-nodes )
         ( display-unexplored-nodes )
      )
  )
   ( cond
      ( ( null *unexplored* )
         ( write-line "There is no solution." )
         ( return-from solve NIL )
      )
   )
   ( setf e-node ( pop *unexplored* ) )
   ( if *trace-search*
      ( let ()
         ( display-e-node e-node )
      )
   )
   ( cond
      ( ( goalp ( node-state e-node ) )
         ( display-solution e-node )
      )
      ( ( exploredp e-node )
         ( solve )
      )
      ( ( feast-state-p ( node-state e-node ) )
         ( if *trace-search* 
            ( format t ">>> DEATH!~%" )
         )
         ( solve )
      )
      ( t 
         ( push e-node *explored* )
         ( setf kids ( children-of e-node ) )
         ( setf *unexplored* ( append *unexplored* kids ) )
         ( solve )
      )  
   )
)


; ------------------------------------------------------------------
; Construct and display the solution

( defmethod display-solution ( ( n node ) )
   ( setf *goal* t )
   ( write-line ">>>>>> SOLUTION FOUND!" )
   ( display-parents n )
)

( defmethod display-parents ( (n node) )
   ( if ( not (rootp n) )
      ( display-parents (node-parent n) )
   )
   ( display n )
)


; ------------------------------------------------------------------
; Basic display commands

( defmethod display-explored-nodes ()
   ( prin1 'EXPLORED ) ( terpri ) (terpri)
   ( mapcar #'display *explored* )
   ( terpri )
   nil
)

( defmethod display-unexplored-nodes ()
   ( prin1 'UNEXPLORED ) ( terpri ) (terpri)
   ( mapcar #'display *unexplored* )
   ( terpri )
   nil
)

( defmethod display-e-node ( (e-node node) )
   ( prin1 'E-NODE ) (terpri) (terpri)
   ( display e-node )
   ( terpri )
)


; ------------------------------------------------------------------
; Generating children

( defmethod children-of ( ( e-node node ) &aux kids )
   ( if (applicablep *move-lr-c* e-node)
      ( push (child-of e-node *move-lr-c*) kids )
   )
   ( if (applicablep *move-lr-cc* e-node)
      ( push (child-of e-node *move-lr-cc*) kids )
   )
   ( if (applicablep *move-lr-m* e-node)
      ( push (child-of e-node *move-lr-m*) kids )
   )
   ( if (applicablep *move-lr-mm* e-node)
      ( push (child-of e-node *move-lr-mm*) kids )
   )
   ( if (applicablep *move-rl-c* e-node)
      ( push (child-of e-node *move-rl-c*) kids )
   )
   ( if (applicablep *move-rl-cc* e-node)
      ( push (child-of e-node *move-rl-cc*) kids )
   )
   ( if (applicablep *move-rl-m* e-node)
      ( push (child-of e-node *move-rl-m*) kids )
   )
   ( if (applicablep *move-rl-mm* e-node)
      ( push (child-of e-node *move-rl-mm*) kids )
   )
   ( if (applicablep *move-rl-mc* e-node)
      ( push (child-of e-node *move-rl-mc*) kids )
   )
   ( if (applicablep *move-lr-mc* e-node)
      ( push (child-of e-node *move-lr-mc*) kids )
   )
   kids
)

( defmethod child-of ( ( n node ) ( o operator ) &aux c )
   ( let* 
      (  ( new-node-name  (next *ng*) )
         ( new-node-state (apply-operator o (node-state n)) )
         ( new-node 
            ( make-instance 'node 
               :name new-node-name
               :state new-node-state
               :parent n
               :operator o
            )
         )
      )
      new-node
   )
)

(defmethod apply-operator ((o operator) (c state))
   ( case (operator-name o)
      ('move-rl-m  (move-rl c :M 1))
      ('move-lr-m  (move-lr c :M 1))
      ('move-rl-c  (move-rl c :C 1))
      ('move-lr-c  (move-lr c :C 1))
      ('move-rl-mm (move-rl c :M 2))
      ('move-lr-mm (move-lr c :M 2))
      ('move-rl-cc (move-rl c :C 2))
      ('move-lr-cc (move-lr c :C 2))
      ('move-rl-mc (move-rl c :M 1 :C 1))
      ('move-lr-mc (move-lr c :M 1 :C 1))
   )
)


; ------------------------------------------------------------------
; Basic display method

( defmethod print-new-line ( o ) 
   ( prin1 o ) ( terpri )
)  

;;(load "mc-trace.l")