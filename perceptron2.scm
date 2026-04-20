;(C)Tsubasa Kato 2026/4/20 - Made with help of Chat GPT (GPT-5.4)
(define (step x)
  (if (>= x 0) 1 0))

(define (dot xs ys)
  (if (null? xs)
      0
      (+ (* (car xs) (car ys))
         (dot (cdr xs) (cdr ys)))))

(define (predict weights bias inputs)
  (step (+ (dot weights inputs) bias)))

(define and-weights '(1 1))
(define and-bias -1.5)

(define (show-and a b)
  (display "AND(")
  (display a)
  (display ", ")
  (display b)
  (display ") => ")
  (display (predict and-weights and-bias (list a b)))
  (newline))
(newline)
(show-and 0 0)
(show-and 0 1)
(show-and 1 0)
(show-and 1 1)

'done
