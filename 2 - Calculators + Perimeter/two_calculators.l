;; Brandon Druschel - 9/22/18
;;;; TASK III: LR Infix Calculator

( defun calculator-LR (&aux operator1 operator2 operands result1 result2)
   ( format t "Expression? ")
   ( setf input (list (read) (read) (read) (read) (read) ))
   ( setf operator1 (second input) )
   ( setf operator2 (fourth input) )
   ( setf operands (list (first input) (third input) ))
   ( setf third_operand (fifth input) )
   ( setf result1 (apply operator1 operands ))
   ( setf result2 (apply operator2 (list result1 third_operand) ))
   ( format t "Result: ~A~%" result2 )
   ( calculator-LR )
)

;;;; TASK VI: Implementation Featuring funcall

( defun calculator-RL (&aux operator1 operator2 operand1 operand2 operand3 result1 result2)
   ( format t "Expression? ")
   ( setf input (list (read) (read) (read) (read) (read) ))
   ( setf operator1 (second input) )
   ( setf operator2 (fourth input) )
   ( setf operand1 (first input) )
   ( setf operand2 (third input) )
   ( setf operand3 (fifth input) )
   ( setf result1 (funcall operator2 operand2 operand3 ))
   ( setf result2 (funcall operator1 operand1 result1 ))
   ( format t "Result: ~A~%" result2 )
   ( calculator-RL )
)
   