(define-library (srfi 17)

  (define-library (rename set!)
    (import (scheme base))
    (export (rename set! set!%)
	    define-syntax
	    syntax-rules
	    lambda
	    if
	    quote
	    begin))

  (import (rename set!)
	  (srfi 1))
  
  (define-syntax set!
    (syntax-rules ()
      ((_ (sttr arg ...) value)
       ((setter sttr) arg ... value))
      ((_ v1 v2)
       (set!% v1 v2))))

  (define setter-alist
    '((car . set-car!)
      (cdr . set-cdr!)
      (caar . (lambda (p v) (set-car! (car p) v)))
      (cadr . (lambda (p v) (set-car! (cdr p) v)))
      (cdar . (lambda (p v) (set-cdr! (car p) v)))
      (cddr . (lambda (p v) (set-cdr! (cdr p) v)))
      (vector-ref . vector-set!)
      (string-ref . string-set!)
      (bytevector-u8-ref . bytevector-u8-set!)
      (list-ref . list-set!)))

  (define setter
    (letrec ((setter
	      (lambda (proc)
		(let ((probe (assv proc setter-alist)))
		  (if probe
		      (cdr probe)
		      (error "No setter for " proc)))))
	     (set-setter!
	      (lambda (proc setter)
		(set! setter-alist
		      (alist-cons proc setter setter-alist)))))
      (set-setter! setter set-setter!)
      setter))

  (define (getter-with-setter get set)
    (let ((proc (lambda args (apply get args))))
      (set! (setter proc) set)
      proc))

  (export set!
	  setter
	  getter-with-setter))
