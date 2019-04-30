;;;; PROGRAMMING CHALLENGE: INTERACTIVE MISSIONARIES AND CANNIBALS PROBLEM SOLVING FRAMEWORK
;; Brandon Druschel - 10/16/18
;; Original code by Craig Graci

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

( defun establish-world ()
;; problem initially starts w/ 3 missionaries and 3 cannibals on the left-bank
   ( setf *left-bank* '(m m m c c c b) )
   ( setf *right-bank* '() )
)

( defun init-move-list ()
;; move-list records the history of moves made en route to the goal
   ( setf *move-list* '() )
)

( defun mc ()
   ( establish-world )
   ( init-move-list )
   ( make-moves )
)

( defun display-world ()
   ( format t "*left-bank*   ~A~%" *left-bank* )
   ( format t "*right-bank*  ~A~%" *right-bank* )
)

( defun goalp ()
   ( cond
      ( ( and 
           (eq (list-count 'c *right-bank*) 3 ) 
           (eq (list-count 'm *right-bank*) 3 ) 
           (eq (list-count 'b *right-bank*) 1 )
        )
         t
      )
      ( t
         nil
      )
   )
)

;; counting method ~ for feast-state-p
(defun list-count (o lst)
   ( cond
      ( ( null lst ) 0 )
      ( ( equal o (car lst) )
         ( + 1 (list-count o (cdr lst)) ) 
      )
      ( t ( list-count o (cdr lst) ) )
   )
)

( defun feast-state-p ()
   ( cond
      ( ( and
         ( < (list-count 'm *right-bank*) (list-count 'c *right-bank*) ) 
         ( > (list-count 'm *right-bank*) 0 )
         ( > (list-count 'c *right-bank*) 0 )
         ( eq (list-count 'b *right-bank*) 0 )
        ) 
         t )
      ( ( and 
         ( < (list-count 'm *left-bank*) (list-count 'c *left-bank*) ) 
         ( > (list-count 'm *left-bank*) 0 )
         ( > (list-count 'c *left-bank*) 0 )
         ( eq (list-count 'b *left-bank*) 0 )
        ) 
         t )
      ( t
         nil
      )
   )
)

;; check: is the 'move' (i.e. '(m b)) applicable from current-bank?
( defun applicable-p ( move )
   ( cond
      ( ( null move ) nil )
      ( ( not (eq (list-count 'b move) 1) ) nil )  ;; # boats =/= 1? inapplicable!
      ( ( > (list-count 'c move) (list-count 'c (current-bank)) ) nil )
      ( ( > (list-count 'm move) (list-count 'm (current-bank)) ) nil )
      ( t
         t
      )
   )
)

( defun make-moves()
   ( display-world ) 
   ( cond
      ( ( goalp )
         ( write-line "good work!" )
         nil
      )
      ( ( feast-state-p )
         ( write-line "Yummy yummy yummy, I got Good in my tummy!!" ) ;; sic
         nil
      )
      ( t
         ( let ( (m (read)) ) ;; user inputs a move (i.e. '(b m)
            ( if ( applicable-p m )  ;; is move applicable to current-bank?
               ( let () ( perform-move m ) ( make-moves ) )
               ( let () ( write-line "move inapplicable" ) nil)
            )
         )
      )
   )
)

( defun current-bank ()
   ( cond
      ( (null (member 'b *left-bank*) )
         *right-bank*
      )
      ( t
         *left-bank*
      )
   )
)

( defun perform-move ( move )
   ( setf *move-list* ( snoc move *move-list* ) )  ;; add 'move' to move-list
   ( if ( equal ( current-bank ) *left-bank* )
      ( move-lr move )
      ( move-rl move )
   )
)

( defun move-lr ( ml )
   ( if ( null ml ) ( return-from move-lr ) )
   ( move-lr-1 ( first ml ) ) ;; TODO: implement move-lr-1
   ( move-lr ( rest ml ) )
)

( defun move-rl ( ml )
   ( if ( null ml ) ( return-from move-rl ) )
   ( move-rl-1 ( first ml ) ) ;; TODO: implement move-rl-1
   ( move-rl ( rest ml ) )
)

;; take-first ~ return the list with element (one occurence) removed
;; ...for use in move-rl-1 and move-lr-1
( defun take-first ( o lst )
   ( cond
      (( null lst )
         ( )
      )
      (( equal o ( car lst )) ;; is object 'o' equal to element
         ( cdr lst )
      )
      ( t
         ( cons ( car lst ) ( take-first o ( cdr lst )))
      )
   )
)

( defun move-rl-1 ( m )
   ( setf *right-bank* (take-first m *right-bank* ) )
   ( setf *left-bank* (cons m *left-bank* ) )
)

( defun move-lr-1 ( m )
   ( setf *left-bank* (take-first m *left-bank* ) )
   ( setf *right-bank* (cons m *right-bank*) ) 
)

( defun print-moves ( lst )
   ( cond
      ( (null lst)
         ()
      )
      ( t
         ( format t "~A~%" (car lst) )
         ( print-moves (cdr lst) )
      )
   )
)

( defun display-solution ()
   ( print-moves *move-list* )
)