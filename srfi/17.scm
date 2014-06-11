(define-library (srfi 17)
  (import (scheme base)
          (srfi 1))
  
  (define-syntax update!
    (syntax-rules ()
      ((_ (proc args ...) val)
       ((setter proc) args ... val))
      ((_ var val)
       (set! var val))))

  (define setter-alist
    `((,car . ,set-car!)
      (,cdr . ,set-cdr!)
      (,caar . ,(lambda (p v) (set-car! (car p) v)))
      (,cadr . ,(lambda (p v) (set-car! (cdr p) v)))
      (,cdar . ,(lambda (p v) (set-cdr! (car p) v)))
      (,cddr . ,(lambda (p v) (set-cdr! (cdr p) v)))
      (,vector-ref . ,vector-set!)
      (,string-ref . ,string-set!)
      (,bytevector-u8-ref . ,bytevector-u8-set!)
      (,list-ref . ,list-set!)))

  (define setter
    (letrec ((setter
              (lambda (proc)
                (let ((probe (assv proc setter-alist)))
                  (if probe
                      (cdr probe)
                      (error "No setter for " proc)))))
             (set-setter!
              (lambda (proc setter)
                (update! setter-alist
                         (alist-cons proc setter setter-alist)))))
      (set-setter! setter set-setter!)
      setter))

  (define (getter-with-setter get set)
    (let ((proc (lambda args (apply get args))))
      (update! (setter proc) set)
      proc))

  (export update!
          setter
          getter-with-setter))
