(import (srfi 1)
        (srfi 8)
        (srfi 43))

(define (dot* v1 v2)
  (vector-fold
   (lambda (index x y) (+ x y))
   0 
   (vector-map
    (lambda (i a b)
      (* a b))
    v1 v2)))

(define (sigmoid x)
  (/ 1 (+ 1 (exp (- x)))))

(define (tanh x)
  (let ((a (exp x))
        (b (exp (- x))))
    (/ (- a b) (+ a b))))

(define (predict ws x . f)
  (define (y ws x)
    (let ((f (if (null? f) sigmoid (car f))))
      (fold
       (lambda (a b)
         (f (dot* a b)))
       x
       ws)))
  (receive (ws w) (split-at ws (- (length ws) 1))
    (vector-map
     (lambda (i a)
       (* a (y ws x)))
     (car w))))

