;; Brandon Druschel - 9/29/18
;;;; TASK 0, I: SINGLETON-P -- RAC -- RDC -- SNOC -- PALINDROME-P

;; singleton-p
( defun singleton-p (lst)
   ( cond
      (( null lst ) nil )        ;; if list is empty, return NIL.
      (( null ( cdr lst )) t )   ;; if CDR of list is empty, return T.
      ( t nil )                  ;; otherwise, return NIL.
   )
)

;; RAC
( defun rac ( lst )
   ( cond
      ( ( singleton-p lst )     ;; if list is a singleton, return CAR of list
         ( car lst )
      )
      ( t
         ( rac ( cdr lst ))   ;; recursively loop rac with CDR of list
      )                       ;; until list input is a singleton.
   )
)

;; RDC
( defun rdc ( lst )
   ( cond
      (( singleton-p lst)     ;; if list is singleton, return empty list
         ( )
      )
      ( t
         ( cons ( car lst ) ( rdc ( cdr lst ))) 
      )                       ;; return CONS of CAR of list and RDC of
   )                          ;; CDR of list. recursively loop RDC until
)                             ;; list input is a singleton (which should
                              ;; be the last element in the list)
                              
;; SNOC
( defun snoc ( lo ll )        ;; lo = lisp object, ll = list
   ( cond
      (( null ll )            ;; terminal condition: empty list
         ( list lo )
      )
      ( t
         ( cons ( car ll ) ( snoc lo ( cdr ll )))
      )
   )
)

;; palindrome-p
( defun palindrome-p ( lst )
   ( cond
      (( null lst )           ;; empty list? it's a palindrome
         t
      )
      (( singleton-p lst )   ;; list is singleton? it's a palindrome
         t
      )
      (( equal ( car lst ) ( rac lst ))
         ( palindrome-p ( cdr ( rdc lst )))
      )
      ( t
         nil
      )
   )
)


;;;; TASK II: SELECT -- PICK

;; SELECT - take one argument (int), return element of list in given position
;; (assume the position exists)
( defun select ( n lst )
   ( cond
      (( eq n 0 )
         ( car lst )
      )
      ( ( > n 0 )
         ( select ( - n 1 ) ( cdr lst ))
      )
      ( t
         nil
      )
   )
)

;; pick
( defun pick ( lst )
   ( cond
      ( t 
         ( select ( random ( list-length lst )) lst )
      )
   )
)


;;;; TASK III: SUM -- PRODUCT

;; sum
( defun sum ( lst )
   ( cond
      (( null lst )
         0
      )
      ( t
         ( + ( car lst ) ( sum ( cdr lst ) ) )
      )
   )
)

;; product
( defun product ( lst )
   ( cond
      (( null lst )
         1
      )
      ( t
         ( * ( car lst ) ( product ( cdr lst ) ) )
      )
   )
)


;;;; TASK IV: IOTA -- DUPLICATE

;; iota
( defun iota ( n )
   ( cond
      (( = n 0 )
         ( )
      )
      ( t
         ( snoc n ( iota ( - n 1 )))
      )
   )
)

;; duplicate
( defun duplicate ( n lo ) ;; n = nonnegative number, lo = Lisp object
   ( cond
      (( = n 0 )
         ( )
      )
      ( t
         ( snoc lo ( duplicate ( - n 1 ) lo ))
      )
   )
)


;;;; TASK V: FACTORIAL -- POWER

;; factorial ~ using iota and product
( defun factorial ( n )
   ( product ( iota n ) )
)

;; power 
( defun power ( n p )
   ( product ( duplicate p n ))
)


;;;; TASK VI: FILTER-IN -- FILTER-OUT

;; filter-in
( defun filter-in ( pr lst ) ;; pr = predicate
   ( cond
      (( null lst )
         ( )
      )
      ((funcall pr ( car lst ))  ;; does pr apply to element
         ( cons ( car lst ) ( filter-in pr ( cdr lst ))) 
      )                          ;; ^ add element to lst
      ( t
         ( filter-in pr ( cdr lst )) ;; skip to next element
      )
   )
)

;; filter-out
( defun filter-out ( pr lst )
   ( cond
      (( null lst )
         ( )
      )
      (( funcall pr ( car lst ))  ;; does pr apply to element
         ( filter-out pr ( cdr lst ))
      )
      ( t
         ( cons ( car lst ) ( filter-out pr ( cdr lst )))
      )
   )
)


;;;; TASK VII: TAKE-FROM

;; take-from
( defun take-from ( o lst )
   ( cond
      (( null lst )
         ( )
      )
      (( equal o ( car lst )) ;; is object 'o' equal to element
         ( take-from o ( cdr lst ))
      )
      ( t
         ( cons ( car lst ) ( take-from o ( cdr lst )))
      )
   )
)


;;;; TASK VIII: RANDOM-PERMUTATION

;; take-first ~ return the list with element (one occurence) removed
;; ...for use in random-permutation
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

;; random-permutation ~ based on given pseudocode
( defun random-permutation ( lst )
   ( cond
      (( null lst )
         ( )   
      )
      ( t
         ( setf element ( pick lst ))
         ( setf remainder ( take-first element lst ))
         ( cons element ( random-permutation remainder ))
      )
   )
)