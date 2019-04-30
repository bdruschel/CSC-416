;; TASK 6: THREE QUARTERS / FAR CORNERS (SOLUTION)
;; By Brandon Druschel
;; CSC 416

;; x3 Rectangle length: 2.865" (quarter diameter * 3)
;; Rectangle Height: 0.955" (quarter diameter)

;; Diagonal Length: sqrt(w^2 + h^2) --> sqrt(0.955^2 + 2.865^2) = ~3.01998

(setf coin-diameter 0.955)
(setf rect-width coin-diameter)
(setf rect-length (* coin-diameter 3))
(setf rect-diagonal (sqrt (+ (expt rect-width 2) (expt rect-length 2) )))