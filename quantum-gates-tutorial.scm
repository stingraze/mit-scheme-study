;;; quantum-gates-interactive.scm
;;; Interactive educational MIT Scheme script for common quantum gates
;;; (C) Tsubasa Kato - 4/20/2026 - 13:01PM JST - Created with help of Chat GPT
(define i (make-rectangular 0 1))
(define pi 3.141592653589793)
(define sqrt2 (sqrt 2))
(define inv-sqrt2 (/ 1 sqrt2))

;;; ------------------------------------------------------------
;;; Basic utilities
;;; ------------------------------------------------------------

(define (pow2 n)
  (if (= n 0)
      1
      (* 2 (pow2 (- n 1)))))

(define (repeat value n)
  (if (= n 0)
      '()
      (cons value (repeat value (- n 1)))))

(define (basis-vector dim index)
  (define (loop pos)
    (if (= pos dim)
        '()
        (cons (if (= pos index) 1 0)
              (loop (+ pos 1)))))
  (loop 0))

(define (dot-product xs ys)
  (if (null? xs)
      0
      (+ (* (car xs) (car ys))
         (dot-product (cdr xs) (cdr ys)))))

(define (matrix-times-vector m v)
  (if (null? m)
      '()
      (cons (dot-product (car m) v)
            (matrix-times-vector (cdr m) v))))

(define (print-row row)
  (display "[ ")
  (define (loop xs)
    (cond ((null? xs)
           (display "]")
           (newline))
          (else
           (write (car xs))
           (if (not (null? (cdr xs)))
               (display "  "))
           (loop (cdr xs)))))
  (loop row))

(define (print-matrix m)
  (define (loop rows)
    (if (not (null? rows))
        (begin
          (print-row (car rows))
          (loop (cdr rows)))))
  (loop m))

(define (print-vector v)
  (display "[ ")
  (define (loop xs)
    (cond ((null? xs)
           (display "]"))
          (else
           (write (car xs))
           (if (not (null? (cdr xs)))
               (display "  "))
           (loop (cdr xs)))))
  (loop v))

(define (member-symbol? x xs)
  (cond ((null? xs) #f)
        ((eq? x (car xs)) #t)
        (else (member-symbol? x (cdr xs)))))

(define (line)
  (display "--------------------------------------------------")
  (newline))

;;; ------------------------------------------------------------
;;; Common states
;;; ------------------------------------------------------------

(define ket0  '(1 0))
(define ket1  '(0 1))

(define ket00 '(1 0 0 0))
(define ket01 '(0 1 0 0))
(define ket10 '(0 0 1 0))
(define ket11 '(0 0 0 1))

(define ket000 '(1 0 0 0 0 0 0 0))
(define ket001 '(0 1 0 0 0 0 0 0))
(define ket010 '(0 0 1 0 0 0 0 0))
(define ket011 '(0 0 0 1 0 0 0 0))
(define ket100 '(0 0 0 0 1 0 0 0))
(define ket101 '(0 0 0 0 0 1 0 0))
(define ket110 '(0 0 0 0 0 0 1 0))
(define ket111 '(0 0 0 0 0 0 0 1))

;;; ------------------------------------------------------------
;;; Gate definitions
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

(define RX90-gate (Rx (/ pi 2)))
(define RY90-gate (Ry (/ pi 2)))
(define RZ90-gate (Rz (/ pi 2)))

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

(define CCX-gate
  '((1 0 0 0 0 0 0 0)
    (0 1 0 0 0 0 0 0)
    (0 0 1 0 0 0 0 0)
    (0 0 0 1 0 0 0 0)
    (0 0 0 0 1 0 0 0)
    (0 0 0 0 0 1 0 0)
    (0 0 0 0 0 0 0 1)
    (0 0 0 0 0 0 1 0)))

;;; ------------------------------------------------------------
;;; Gate catalog
;;; entry = (name qubits description intuition matrix)
;;; ------------------------------------------------------------

(define gate-catalog
  (list
   (list 'I    1
         "Identity gate"
         "Leaves the state unchanged."
         I-gate)

   (list 'X    1
         "Pauli-X gate"
         "Bit flip: swaps |0> and |1>."
         X-gate)

   (list 'Y    1
         "Pauli-Y gate"
         "Like a bit+phase operation with imaginary amplitudes."
         Y-gate)

   (list 'Z    1
         "Pauli-Z gate"
         "Phase flip: leaves |0> alone and flips the sign of |1>."
         Z-gate)

   (list 'H    1
         "Hadamard gate"
         "Creates superposition from computational basis states."
         H-gate)

   (list 'S    1
         "Phase gate"
         "Adds a 90-degree phase to |1>."
         S-gate)

   (list 'SDG  1
         "Inverse phase gate"
         "Undo of S."
         Sdg-gate)

   (list 'T    1
         "T gate"
         "Adds a 45-degree phase to |1>."
         T-gate)

   (list 'TDG  1
         "Inverse T gate"
         "Undo of T."
         Tdg-gate)

   (list 'RX90 1
         "Rotation around X by pi/2"
         "Continuous rotation gate; this is a quarter-turn around X."
         RX90-gate)

   (list 'RY90 1
         "Rotation around Y by pi/2"
         "Continuous rotation gate; this is a quarter-turn around Y."
         RY90-gate)

   (list 'RZ90 1
         "Rotation around Z by pi/2"
         "Continuous rotation gate; this is a quarter-turn around Z."
         RZ90-gate)

   (list 'CNOT 2
         "Controlled-NOT gate"
         "Flips the target qubit only when the control qubit is 1."
         CNOT-gate)

   (list 'CZ   2
         "Controlled-Z gate"
         "Adds a phase only to |11>."
         CZ-gate)

   (list 'SWAP 2
         "SWAP gate"
         "Exchanges the two qubits."
         SWAP-gate)

   (list 'CCX  3
         "Toffoli / CCX gate"
         "Flips the target only when both control qubits are 1."
         CCX-gate)))

(define (gate-name entry)        (list-ref entry 0))
(define (gate-qubits entry)      (list-ref entry 1))
(define (gate-description entry) (list-ref entry 2))
(define (gate-intuition entry)   (list-ref entry 3))
(define (gate-matrix entry)      (list-ref entry 4))

(define (find-gate name)
  (define (loop xs)
    (cond ((null? xs) #f)
          ((eq? name (gate-name (car xs))) (car xs))
          (else (loop (cdr xs)))))
  (loop gate-catalog))

;;; ------------------------------------------------------------
;;; Educational displays
;;; ------------------------------------------------------------

(define (show-banner)
  (newline)
  (display "Quantum Gates Tutor for MIT Scheme")
  (newline)
  (display "Type numbers for menu choices, and gate names like X, H, CNOT, or CCX.")
  (newline)
  (display "No quotes are needed.")
  (newline)
  (line))

(define (show-menu)
  (newline)
  (display "Menu")
  (newline)
  (display "  1 - List available gates")
  (newline)
  (display "  2 - Inspect a gate")
  (newline)
  (display "  3 - Take a quiz")
  (newline)
  (display "  4 - Show quick notes")
  (newline)
  (display "  5 - Exit")
  (newline)
  (line)
  (display "Enter choice: "))

(define (list-gates)
  (newline)
  (display "Available gates")
  (newline)
  (line)
  (define (loop xs)
    (if (not (null? xs))
        (begin
          (display "  ")
          (write (gate-name (car xs)))
          (display "  - ")
          (display (gate-description (car xs)))
          (display "  [")
          (display (gate-qubits (car xs)))
          (display " qubit")
          (if (> (gate-qubits (car xs)) 1) (display "s"))
          (display "]")
          (newline)
          (loop (cdr xs)))))
  (loop gate-catalog))

(define (show-1q-demo matrix)
  (newline)
  (display "Action on basis states")
  (newline)
  (line)

  (display "|0> -> ")
  (print-vector (matrix-times-vector matrix ket0))
  (newline)

  (display "|1> -> ")
  (print-vector (matrix-times-vector matrix ket1))
  (newline))

(define (show-2q-demo matrix)
  (newline)
  (display "Action on computational basis states")
  (newline)
  (line)

  (display "|00> -> ")
  (print-vector (matrix-times-vector matrix ket00))
  (newline)

  (display "|01> -> ")
  (print-vector (matrix-times-vector matrix ket01))
  (newline)

  (display "|10> -> ")
  (print-vector (matrix-times-vector matrix ket10))
  (newline)

  (display "|11> -> ")
  (print-vector (matrix-times-vector matrix ket11))
  (newline))

(define (show-3q-demo matrix)
  (newline)
  (display "Selected examples")
  (newline)
  (line)

  (display "|101> -> ")
  (print-vector (matrix-times-vector matrix ket101))
  (newline)

  (display "|110> -> ")
  (print-vector (matrix-times-vector matrix ket110))
  (newline)

  (display "|111> -> ")
  (print-vector (matrix-times-vector matrix ket111))
  (newline))

(define (inspect-gate)
  (newline)
  (list-gates)
  (newline)
  (display "Type a gate name: ")
  (let ((name (read)))
    (let ((entry (find-gate name)))
      (if (not entry)
          (begin
            (newline)
            (display "I do not know that gate.")
            (newline))
          (begin
            (newline)
            (line)
            (display "Gate: ")
            (write (gate-name entry))
            (newline)
            (display "Description: ")
            (display (gate-description entry))
            (newline)
            (display "Idea: ")
            (display (gate-intuition entry))
            (newline)
            (display "Qubits: ")
            (display (gate-qubits entry))
            (newline)
            (line)
            (display "Matrix")
            (newline)
            (print-matrix (gate-matrix entry))
            (cond ((= (gate-qubits entry) 1)
                   (show-1q-demo (gate-matrix entry)))
                  ((= (gate-qubits entry) 2)
                   (show-2q-demo (gate-matrix entry)))
                  ((= (gate-qubits entry) 3)
                   (show-3q-demo (gate-matrix entry)))))))))

;;; ------------------------------------------------------------
;;; Quiz
;;; ------------------------------------------------------------

;;; question = (prompt accepted-symbols explanation)
(define quiz-questions
  (list
   (list
    "Which gate swaps |0> and |1>?"
    '(X)
    "X is the bit-flip gate.")

   (list
    "Which gate creates superposition from |0>?"
    '(H)
    "H maps |0> to (|0> + |1>) / sqrt(2).")

   (list
    "Which 2-qubit gate flips the target when the control is 1?"
    '(CNOT)
    "CNOT means controlled-NOT.")

   (list
    "Which gate exchanges two qubits?"
    '(SWAP)
    "SWAP trades the positions of the two qubits.")

   (list
    "Which gate mainly adds a sign flip to |1> while leaving |0> alone?"
    '(Z)
    "Z is the phase-flip gate in the computational basis.")))

(define (ask-question q)
  (let ((prompt (list-ref q 0))
        (answers (list-ref q 1))
        (explanation (list-ref q 2)))
    (newline)
    (display prompt)
    (newline)
    (display "Your answer: ")
    (let ((ans (read)))
      (if (member-symbol? ans answers)
          (begin
            (display "Correct. ")
            (display explanation)
            (newline)
            1)
          (begin
            (display "Not quite. ")
            (display explanation)
            (display " Expected one of: ")
            (write answers)
            (newline)
            0)))))

(define (take-quiz)
  (newline)
  (display "Quantum gate quiz")
  (newline)
  (display "Answer using symbols like X, H, CNOT, SWAP, Z.")
  (newline)
  (line)

  (define (loop qs score total)
    (if (null? qs)
        (begin
          (newline)
          (display "Quiz complete.")
          (newline)
          (display "Score: ")
          (display score)
          (display " / ")
          (display total)
          (newline)
          (cond ((= score total)
                 (display "Excellent. You have the basics down."))
                ((>= score 3)
                 (display "Nice. You understand the main ideas."))
                (else
                 (display "Good start. Inspect a few gates again and retry.")))
          (newline))
        (loop (cdr qs)
              (+ score (ask-question (car qs)))
              total)))

  (loop quiz-questions 0 (length quiz-questions)))

;;; ------------------------------------------------------------
;;; Quick notes
;;; ------------------------------------------------------------

(define (show-notes)
  (newline)
  (display "Quick notes")
  (newline)
  (line)
  (display "1. A quantum gate is a matrix that transforms a state vector.")
  (newline)
  (display "2. One-qubit states have 2 amplitudes, two-qubit states have 4, three-qubit states have 8.")
  (newline)
  (display "3. X is a bit flip, Z is a phase flip, and H creates superposition.")
  (newline)
  (display "4. S and T are phase gates.")
  (newline)
  (display "5. CNOT and CCX are controlled gates.")
  (newline)
  (display "6. Rotation gates like RX90, RY90, and RZ90 belong to continuous families.")
  (newline))

;;; ------------------------------------------------------------
;;; Main loop
;;; ------------------------------------------------------------

(define (main-loop)
  (show-banner)
  (define (loop)
    (show-menu)
    (let ((choice (read)))
      (cond ((= choice 1)
             (list-gates)
             (loop))
            ((= choice 2)
             (inspect-gate)
             (loop))
            ((= choice 3)
             (take-quiz)
             (loop))
            ((= choice 4)
             (show-notes)
             (loop))
            ((= choice 5)
             (newline)
             (display "Goodbye.")
             (newline)
             'done)
            (else
             (newline)
             (display "Please enter 1, 2, 3, 4, or 5.")
             (newline)
             (loop)))))
  (loop))

(main-loop)
