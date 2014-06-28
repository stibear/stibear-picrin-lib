(import (srfi 8)
        (srfi 43))

(define (dot* v1 v2)
  (vector-fold
   (lambda (index x y) (+ x y))
   0
   (vector-map
    (lambda (i a b)
      (* a b))
    v1 v2)))

(define (predict w x)
  (let ((dotp (dot* w x)))
    (if (negative? dotp)
        (values -1 dotp)
        (values 1 dotp))))

(define *c* 0.5)
(define *counter* 100)

(define (train w x t . c)
  (let ((c (if (null? c) *c* c)))
    (receive (res dotp) (predict w x)
             (if (positive? (* dotp t))
                 w
                 (vector-map
                  (lambda (i a b) (+ a b))
                  w
                  (vector-map (lambda (i a) (* t c a)) x))))))


;; test case (and)
(define test-and-x
  '(#(1 1 1)
    #(1 1 0)
    #(1 0 1)
    #(1 0 0)))

(define first-weight
  '#(1 0 0))

(define test-and-t
  '(1 -1 -1 -1))

(define (percep x)
  (let ((weight first-weight))
    (let loop1 ((j 0))
      (when (< j *counter*)
            (let ((tmp weight))
              (let loop2 ((i 0))
                (when (< i (length test-and-x))
                      (set! tmp
                            (train tmp
                                   (list-ref test-and-x i)
                                   (list-ref test-and-t i)))
                      (loop2 (+ i 1))))
              (when (not (vector= = tmp weight))
                    (set! weight tmp)
                    (loop1 (+ j 1))))))
    (values (predict weight x)
            weight)))

(percep #(1 1 1)) ; => 1
(percep #(1 1 0)) ; => -1
(percep #(1 0 1)) ; => -1
(percep #(1 0 0)) ; => -1
