(define-library (srfi 17)
  (import (scheme base)
          (srfi 1)
          (picrin dictionary))
  
  (define-syntax update!
    (syntax-rules ()
      ((_ (proc args ...) val)
       ((setter proc) args ... val))
      ((_ var val)
       (set! var val))))
  
  ;; now, dictionary will take only symbols as its keys
  (define setter-dict
    (let ((dict (dictionary)))
      (dictionary-set! dict car set-car!)
      (dictionary-set! dict cdr set-cdr!)
      (dictionary-set! dict caar (lambda (p v) (set-car! (car p) v)))
      (dictionary-set! dict cadr (lambda (p v) (set-car! (cdr p) v)))
      (dictionary-set! dict cdar (lambda (p v) (set-cdr! (car p) v)))
      (dictionary-set! dict cddr (lambda (p v) (set-cdr! (cdr p) v)))
      (dictionary-set! dict vector-ref vector-set!)
      (dictionary-set! dict string-ref string-set!)
      (dictionary-set! dict bytevector-u8-ref bytevector-u8-set!)
      (dictionary-set! dict list-ref list-set!)
      dict))

  (define setter
    (letrec ((setter
              (lambda (proc)
                (dictionary-ref dict proc)))
             (set-setter!
              (lambda (proc setter)
                (dictionary-set! setter-dict proc setter))))
      (set-setter! setter set-setter!)
      setter))

  (define (getter-with-setter get set)
    (let ((proc (lambda args (apply get args))))
      (update! (setter proc) set)
      proc))

  (export update!
          setter
          getter-with-setter))
