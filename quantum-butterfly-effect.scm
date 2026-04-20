;;; quantum-butterfly-fixed.scm
;;; Interactive MIT Scheme toy simulator for "butterfly-effect-like" behavior
;;; (C) Tsubasa Kato - Inspire Search Corp. 4/20/2026 - Created with the help of ChatGPT (GPT 5.4)
;;; Fixed timing version:
;;; - uses real-time-clock
;;; - uses internal-time/seconds->ticks
;;; - schedules against absolute wall-clock deadlines

(define i (make-rectangular 0 1))
(define pi 3.141592653589793)
(define sqrt2 (sqrt 2))
(define inv-sqrt2 (/ 1 sqrt2))

;;; ------------------------------------------------------------
;;; Utilities
;;; ------------------------------------------------------------

(define (sum-list xs)
  (if (null? xs)
      0
      (+ (car xs) (sum-list (cdr xs)))))

(define (complex-mag2 z)
  (+ (* (real-part z) (real-part z))
     (* (imag-part z) (imag-part z))))

(define (vector-norm v)
  (sqrt (sum-list (map complex-mag2 v))))

(define (normalize-state v)
  (let ((n (vector-norm v)))
    (if (= n 0)
        v
        (map (lambda (x) (/ x n)) v))))

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

(define (transpose m)
  (if (null? (car m))
      '()
      (cons (map car m)
            (transpose (map cdr m)))))

(define (matrix-times-matrix a b)
  (let ((bt (transpose b)))
    (map (lambda (row)
           (map (lambda (col)
                  (dot-product row col))
                bt))
         a)))

(define (row-scale s row)
  (map (lambda (x) (* s x)) row))

(define (tensor2 a b)
  (let* ((ar0 (car a))
         (ar1 (cadr a))
         (a00 (car ar0))
         (a01 (cadr ar0))
         (a10 (car ar1))
         (a11 (cadr ar1))
         (br0 (car b))
         (br1 (cadr b))
         (r1 (append (row-scale a00 br0) (row-scale a01 br0)))
         (r2 (append (row-scale a00 br1) (row-scale a01 br1)))
         (r3 (append (row-scale a10 br0) (row-scale a11 br0)))
         (r4 (append (row-scale a10 br1) (row-scale a11 br1))))
    (list r1 r2 r3 r4)))

(define (probabilities state)
  (map complex-mag2 state))

(define (prob-distance p q)
  (if (null? p)
      0
      (+ (abs (- (car p) (car q)))
         (prob-distance (cdr p) (cdr q)))))

(define (print-vector label v)
  (display label)
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
  (loop v))

(define (line)
  (display "------------------------------------------------------------")
  (newline))

;;; ------------------------------------------------------------
;;; Timing helpers -- FIXED
;;; ------------------------------------------------------------

(define (seconds->ticks s)
  (internal-time/seconds->ticks s))

(define (now-ticks)
  (real-time-clock))

(define (wait-until-tick target-tick)
  (let loop ()
    (if (< (now-ticks) target-tick)
        (loop)
        'done)))

;;; ------------------------------------------------------------
;;; Gates
;;; ------------------------------------------------------------

(define I-gate
  '((1 0)
    (0 1)))

(define H-gate
  (list
   (list inv-sqrt2 inv-sqrt2)
   (list inv-sqrt2 (- inv-sqrt2))))

(define S-gate
  (list
   (list 1 0)
   (list 0 i)))

(define T-gate
  (list
   (list 1 0)
   (list 0 (exp (* i (/ pi 4))))))

(define (Rx theta)
  (let ((c (cos (/ theta 2)))
        (s (sin (/ theta 2))))
    (list
     (list c (* (- i) s))
     (list (* (- i) s) c))))

(define (Rz theta)
  (list
   (list (exp (* (- i) (/ theta 2))) 0)
   (list 0 (exp (* i (/ theta 2))))))

(define CNOT-gate
  '((1 0 0 0)
    (0 1 0 0)
    (0 0 0 1)
    (0 0 1 0)))

(define SWAP-gate
  '((1 0 0 0)
    (0 0 1 0)
    (0 1 0 0)
    (0 0 0 1)))

;;; ------------------------------------------------------------
;;; Butterfly model
;;; ------------------------------------------------------------

(define seed-a
  (normalize-state (list 1 0 0 0)))

(define epsilon 0.001)
(define seed-b
  (normalize-state (list 1 0 0 (* i epsilon))))

(define (feedback-angle state)
  (let ((a0 (list-ref state 0))
        (a1 (list-ref state 1))
        (a2 (list-ref state 2))
        (a3 (list-ref state 3)))
    (+ 0.35
       (* 1.70 (real-part a0))
       (* 1.10 (imag-part a1))
       (* -0.90 (real-part a2))
       (* 1.25 (imag-part a3)))))

(define (butterfly-unitary theta)
  (let* ((u1 (tensor2 H-gate I-gate))
         (u2 (tensor2 S-gate T-gate))
         (u3 (tensor2 (Rz theta) (Rx (/ theta 2))))
         (u4 SWAP-gate)
         (u5 CNOT-gate))
    (matrix-times-matrix
     u5
     (matrix-times-matrix
      u4
      (matrix-times-matrix
       u3
       (matrix-times-matrix u2 u1))))))

(define (evolve-one-step state)
  (let* ((theta (feedback-angle state))
         (u (butterfly-unitary theta))
         (next (normalize-state (matrix-times-vector u state))))
    (list next theta)))

(define (show-step-report t total state-a theta-a state-b theta-b)
  (let ((pa (probabilities state-a))
        (pb (probabilities state-b)))
    (display "t = ")
    (display t)
    (display " / ")
    (display total)
    (display " sec")
    (newline)

    (display "theta A = ")
    (write theta-a)
    (newline)

    (display "theta B = ")
    (write theta-b)
    (newline)

    (print-vector "P(A) = " pa)
    (print-vector "P(B) = " pb)

    (display "butterfly score = ")
    (write (prob-distance pa pb))
    (newline)
    (line)))

(define (run-butterfly seconds)
  (newline)
  (display "Starting butterfly demo")
  (newline)
  (display "Duration: ")
  (display seconds)
  (display " second")
  (if (> seconds 1) (display "s"))
  (newline)
  (line)

  (let ((start-tick (now-ticks)))
    (define (loop t state-a state-b)
      (if (> t seconds)
          (begin
            (display "Demo complete.")
            (newline)
            'done)
          (let* ((step-a (evolve-one-step state-a))
                 (next-a (car step-a))
                 (theta-a (cadr step-a))
                 (step-b (evolve-one-step state-b))
                 (next-b (car step-b))
                 (theta-b (cadr step-b))
                 (target-tick (+ start-tick (seconds->ticks t))))
            (show-step-report t seconds next-a theta-a next-b theta-b)
            (wait-until-tick target-tick)
            (loop (+ t 1) next-a next-b))))
    (loop 1 seed-a seed-b)))

;;; ------------------------------------------------------------
;;; UI
;;; ------------------------------------------------------------

(define (show-explanation)
  (newline)
  (display "Explanation")
  (newline)
  (line)
  (display "This script is a toy butterfly-effect simulator built from quantum gates.")
  (newline)
  (display "It uses repeated quantum gates plus state-dependent feedback.")
  (newline)
  (display "That feedback makes the evolution sensitive to tiny differences.")
  (newline)
  (display "The displayed butterfly score is the distance between measurement-probability distributions.")
  (newline))

(define (show-gate-recipe)
  (newline)
  (display "Gate recipe")
  (newline)
  (line)
  (display "U(theta) = CNOT * SWAP * (Rz(theta) ⊗ Rx(theta/2)) * (S ⊗ T) * (H ⊗ I)")
  (newline)
  (display "At every second, theta is recomputed from the current state.")
  (newline))

(define (show-menu)
  (newline)
  (display "Quantum Butterfly Menu")
  (newline)
  (display "  1 - Run fixed 10-second demo")
  (newline)
  (display "  2 - Run custom demo (1..500 seconds)")
  (newline)
  (display "  3 - Explain the model")
  (newline)
  (display "  4 - Show gate recipe")
  (newline)
  (display "  5 - Exit")
  (newline)
  (line)
  (display "Enter choice: "))

(define (read-custom-seconds)
  (display "Enter number of seconds (1..500): ")
  (let ((n (read)))
    (if (and (integer? n) (>= n 1) (<= n 500))
        n
        (begin
          (display "Invalid value. Using 10 seconds.")
          (newline)
          10))))

(define (main-loop)
  (newline)
  (display "Quantum Butterfly Effect Tutor")
  (newline)
  (display "Type menu numbers and press return.")
  (newline)

  (define (loop)
    (show-menu)
    (let ((choice (read)))
      (cond ((= choice 1)
             (run-butterfly 10)
             (loop))
            ((= choice 2)
             (run-butterfly (read-custom-seconds))
             (loop))
            ((= choice 3)
             (show-explanation)
             (loop))
            ((= choice 4)
             (show-gate-recipe)
             (loop))
            ((= choice 5)
             (newline)
             (display "Goodbye.")
             (newline)
             'done)
            (else
             (display "Please choose 1, 2, 3, 4, or 5.")
             (newline)
             (loop)))))
  (loop))

(main-loop)
