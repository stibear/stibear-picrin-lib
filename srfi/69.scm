(define-library (srfi 69)
  (import (scheme base))

  (define *default-bound* (- (expt 2 29) 3))
  
  (define modulo floor-remainder)

  (define (%string-hash s ch-conv bound)
    (let ((strlen (string-length s)))
      (letrec ((rec
                (lambda (hash index)
                  (if (>= index strlen)
                      (modulo hash bound)
                      (rec (modulo (+ (* 37 hash)
                                      (char->integer
                                       (ch-conv (string-ref s index))))
                                   *default-bound*)
                           (+ index 1))))))
        (rec 31 0))))

  (define (string-hash s . maybe-bound)
    (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
      (%string-hash s (lambda (x) x) bound)))

  (define (string-ci-hash s . maybe-bound)
    (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
      (%string-hash s char-downcase bound)))

  (define (symbol-hash s . maybe-bound)
    (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
      (%string-hash (symbol->string s) (lambda (x) x) bound)))

  ;; After implemented CLOS, this should be implemented as GF
  (define (hash obj . maybe-bound)
    (let ((bound (if (null? maybe-bound) *default-bound* (car maybe-bound))))
      (cond ((integer? obj) (modulo obj bound))
            ((string? obj) (string-hash obj bound))
            ((symbol? obj) (symbol-hash obj bound))
            ((real? obj) (modulo (+ (numerator obj) (denominator obj)) bound))
            ((number? obj)
             (modulo (+ (hash (real-part obj)) (* 3 (hash (imag-part obj))))
                     bound))
            ((char? obj) (modulo (char->integer obj) bound))
            ((vector? obj) (vector-hash obj bound))
            ((pair? obj) (modulo (+ (hash (car obj)) (* 3 (hash (cdr obj))))
                                 bound))
            ((null? obj) 0)
            ((not obj) 0)
            ((procedure? obj) (error "hash: procedures cannot be hashed" obj))
            (else 1))))

  (define hash-by-identity hash)

  (define (vector-hash v bound)
    (let ((len (vector-length v)))
      (letrec ((rec
                (lambda (hashv index)
                  (if (>= index len)
                      (modulo hashv bound)
                      (rec (modulo (+ (* 257 hashv) (hash (vector-ref v index)))
                                   *default-bound*)
                           (+ index 1))))))
        (rec 571 0))))

  (define %make-hash-node cons)
  (define %hash-node-set-value! set-cdr!)
  (define %hash-node-key car)
  (define %hash-node-value cdr)

  (define-record-type <srfi-hash-table>
    (%make-hash-table size hash compare associate entries)
    hash-table?
    (size hash-table-size hash-table-set-size!)
    (hash hash-table-hash-function)
    (compare hash-table-equivalence-function)
    (associate hash-table-association-function)
    (entries hash-table-entries hash-table-set-entries!))

  (define *default-table-size* 64)

  (define (appropriate-hash-function-for comparison)
    (or (and (eq? comparison eq?) hash-by-identity)
        (and (eq? comparison string=?) string-hash)
        (and (eq? comparison string-ci=?) string-ci-hash)
        hash)))
