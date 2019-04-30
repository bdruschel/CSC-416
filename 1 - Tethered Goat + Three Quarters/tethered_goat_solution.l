;; TASK 7: GOAT TETHERING PROBLEM (SOLUTION)
;; By Brandon Druschel
;; CSC 416

;; barn length = 100 
;; barn width = 70 
;; rope length = 107 <-- Circular radius

;; rope length around width = 107 - 70 = 37 <-- partial radius 1
;; rope length around length = 107-100 = 7 <-- partial radius 2

;; Area 1 = 0.75 * pi * 107^2 = ~26976.0707
;; Area 2 = 0.25 * pi * 37^2 = ~1075.21009
;; Area 3 = 0.25 * pi * 7^2 = ~38.4845

;; Total Area = Area 1 + Area 2 + Area 3 = ~28089.7653

(setf barn-length 100)
(setf barn-width 70)
(setf rope-length 107)

(setf rope-length-around-width (- rope-length barn-width) )
(setf rope-length-around-length (- rope-length barn-length) )

(setf area-1 (* 0.75 pi (expt rope-length 2)) )
(setf area-2 (* 0.25 pi (expt rope-length-around-width 2)) )
(setf area-3 (* 0.25 pi (expt rope-length-around-length 2)) )

(setf total-area (+ area-1 area-2 area-3) )

