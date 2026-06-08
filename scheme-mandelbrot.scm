;; mandelbrot.scm

(define width 80)
(define height 40)
(define max-iter 100)

(define (mandel cr ci)
  (let loop ((zr 0.0)
             (zi 0.0)
             (i 0))
    (if (or (= i max-iter)
            (> (+ (* zr zr) (* zi zi)) 4.0))
        i
        (let ((zr2 (+ (- (* zr zr) (* zi zi)) cr))
              (zi2 (+ (* 2.0 zr zi) ci)))
          (loop zr2 zi2 (+ i 1))))))

(define (pixel-char i)
  (cond ((= i max-iter) #\#)
        ((> i 50)       #\*)
        ((> i 20)       #\+)
        ((> i 10)       #\.)
        (else           #\space)))

(define (draw)
  (let y-loop ((y 0))
    (if (< y height)
        (begin
          (let x-loop ((x 0))
            (if (< x width)
                (let* ((cr (/ (* (- x (/ width 2.0)) 4.0) width))
                       (ci (/ (* (- y (/ height 2.0)) 4.0) width))
                       (i (mandel cr ci)))
                  (write-char (pixel-char i))
                  (x-loop (+ x 1)))))
          (newline)
          (y-loop (+ y 1))))))

(draw)
