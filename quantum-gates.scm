;;; quantum-gates.scm
;;; MIT Scheme sample: display common quantum gate matrices
;;; (C) Tsubasa Kato - 4/20/2026 12:55PM JST - Inspire Search Corp.
(define i (make-rectangular 0 1))

;;; ------------------------------------------------------------
;;; Basic helpers
;;; ------------------------------------------------------------

(define (square x) (* x x))

(define (format-number x)
  (cond ((real? x)
         x)
        (else
         x)))

(define (print-cell x)
  (write (format-number x)))

(define (print-row row)
  (display "[ ")
  (let loop ((xs row))
    (cond ((null? xs)
           (display "]")
           (newline))
          (else
           (print-cell (car xs))
           (if (not (null? (cdr xs)))
               (display "  "))
           (loop (cdr xs))))))

(define (print-matrix m)
  (let loop ((rows m))
    (if (not (null? rows))
        (begin
          (print-row (car rows))
          (loop (cdr rows))))))

(define (show-gate name description matrix)
  (newline)
  (display "========================================")
  (newline)
  (display name)
  (newline)
  (display description)
  (newline)
  (display "----------------------------------------")
  (newline)
  (print-matrix matrix))

;;; ------------------------------------------------------------
;;; Constants
;;; ------------------------------------------------------------

(define sqrt2 (sqrt 2))
(define inv-sqrt2 (/ 1 sqrt2))

(define pi 3.141592653589793)

;;; ------------------------------------------------------------
;;; 1-qubit gates
;;; ------------------------------------------------------------

(define I-gate
  '((1 0)
    (0 1)))

(define X-gate
  '((0 1)
    (1 0)))

(define Y-gate
  (list
   (list 0 (- i))
   (list i 0)))

(define Z-gate
  '((1 0)
    (0 -1)))

(define H-gate
  (list
   (list inv-sqrt2 inv-sqrt2)
   (list inv-sqrt2 (- inv-sqrt2))))

(define S-gate
  (list
   (list 1 0)
   (list 0 i)))

(define Sdg-gate
  (list
   (list 1 0)
   (list 0 (- i))))

(define T-gate
  (list
   (list 1 0)
   (list 0 (exp (* i (/ pi 4))))))

(define Tdg-gate
  (list
   (list 1 0)
   (list 0 (exp (* (- i) (/ pi 4))))))

;;; ------------------------------------------------------------
;;; Parameterized 1-qubit rotation gates
;;; ------------------------------------------------------------

(define (Rx theta)
  (let ((c (cos (/ theta 2)))
        (s (sin (/ theta 2))))
    (list
     (list c (* (- i) s))
     (list (* (- i) s) c))))

(define (Ry theta)
  (let ((c (cos (/ theta 2)))
        (s (sin (/ theta 2))))
    (list
     (list c (- s))
     (list s c))))

(define (Rz theta)
  (list
   (list (exp (* (- i) (/ theta 2))) 0)
   (list 0 (exp (* i (/ theta 2))))))

;;; ------------------------------------------------------------
;;; 2-qubit gates
;;; Basis order: |00>, |01>, |10>, |11>
;;; ------------------------------------------------------------

(define CNOT-gate
  '((1 0 0 0)
    (0 1 0 0)
    (0 0 0 1)
    (0 0 1 0)))

(define CZ-gate
  '((1 0 0 0)
    (0 1 0 0)
    (0 0 1 0)
    (0 0 0 -1)))

(define SWAP-gate
  '((1 0 0 0)
    (0 0 1 0)
    (0 1 0 0)
    (0 0 0 1)))

;;; ------------------------------------------------------------
;;; 3-qubit gate
;;; Basis order: |000>, |001>, |010>, |011>, |100>, |101>, |110>, |111>
;;; ------------------------------------------------------------

(define Toffoli-gate
  '((1 0 0 0 0 0 0 0)
    (0 1 0 0 0 0 0 0)
    (0 0 1 0 0 0 0 0)
    (0 0 0 1 0 0 0 0)
    (0 0 0 0 1 0 0 0)
    (0 0 0 0 0 1 0 0)
    (0 0 0 0 0 0 0 1)
    (0 0 0 0 0 0 1 0)))

;;; ------------------------------------------------------------
;;; Demo runner
;;; ------------------------------------------------------------

(define (show-common-gates)
  (show-gate "I gate"
             "Identity gate"
             I-gate)

  (show-gate "X gate"
             "Pauli-X / bit flip"
             X-gate)

  (show-gate "Y gate"
             "Pauli-Y"
             Y-gate)

  (show-gate "Z gate"
             "Pauli-Z / phase flip"
             Z-gate)

  (show-gate "H gate"
             "Hadamard gate"
             H-gate)

  (show-gate "S gate"
             "Phase gate"
             S-gate)

  (show-gate "S† gate"
             "Inverse phase gate"
             Sdg-gate)

  (show-gate "T gate"
             "pi/8 gate"
             T-gate)

  (show-gate "T† gate"
             "Inverse pi/8 gate"
             Tdg-gate)

  (show-gate "Rx(pi/2)"
             "Rotation around X axis by pi/2"
             (Rx (/ pi 2)))

  (show-gate "Ry(pi/2)"
             "Rotation around Y axis by pi/2"
             (Ry (/ pi 2)))

  (show-gate "Rz(pi/2)"
             "Rotation around Z axis by pi/2"
             (Rz (/ pi 2)))

  (show-gate "CNOT gate"
             "Controlled-NOT"
             CNOT-gate)

  (show-gate "CZ gate"
             "Controlled-Z"
             CZ-gate)

  (show-gate "SWAP gate"
             "Swap two qubits"
             SWAP-gate)

  (show-gate "Toffoli gate"
             "CCX / controlled-controlled-NOT"
             Toffoli-gate)

  'done)

(show-common-gates)
