;;;
;;; stream.scm - SICPの問題演習
;;;
;;;   Author: Ryota Wada
;;;     Date: 2012-06-23T05:04:24+09:00.
;;;

(define-macro (my-delay exp)
  `(lambda () ,exp))
(define (my-force obj)
  (obj))
;;
(define (memo-proc proc)
  (let ((already-run? #f)
        (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))
(define-macro (my-memo-delay exp)
  (lambda ()
    exp))
;;
(define (stream-car s)
  (car s))
(define (stream-cdr s)
  (my-force (cdr s)))
(define-macro (cons-stream a b)
  `(cons ,a (my-delay ,b)))
;;
(define stream-null? null?)
(define the-empty-stream '())
;;
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
;; (define (stream-map proc s)
;;   (if (stream-null? s)
;;       the-empty-stream
;;       (cons-stream (proc (stream-car s))
;;                    (stream-map proc (stream-cdr s)))))
(define (stream-map proc . args)
  (if (stream-null? (car args))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car args))
       (apply stream-map
              (cons proc (map stream-cdr args))))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x)
  (newline)
  (display x))
;;
(define (stream-filter pred s)
  (cond ((stream-null? s) the-empty-stream)
        ((pred (stream-car s))
         (cons-stream (stream-car s)
                      (stream-filter pred
                                     (stream-cdr s))))
        (else (stream-filter pred (stream-cdr s)))))
;;
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (+ low 1) high))))
;;

;;
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))
;;
(define (divisible? x y)
  (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))
;;
(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs
  (fibgen 0 1))
;;
(define (sieve s)
  (cons-stream (stream-car s)
               (sieve (stream-filter (lambda (x)
                                       (not (divisible? x
                                                        (stream-car s))))
                                     (stream-cdr s)))))
(define primes
  (sieve (integers-starting-from 2)))
;; p.194
(define ones (cons-stream 1 ones))
;;
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers-v2
  (cons-stream 1
               (add-streams ones integers-v2)))
;;
(define fibs-v2
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs-v2)
                                         fibs-v2))))
;;
(define (scale-stream s factor)
  (stream-map (lambda (x)
                (* x factor))
              s))
(define double
  (cons-stream 1 (scale-stream double 2)))
;;
(define primes-v2
  (cons-stream
   2
   (stream-filter primes? (integers-starting-from 3))))
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n)
           #t)
          ((divisible? n (stream-car ps))
           #f)
          (else (iter (stream-cdr ps)))))
  (iter primes-v2))
;;
(define q.3.53 (cons-stream 1 (add-streams q.3.53 q.3.53)))
;;
(define (mul-streams s1 s2)
  (stream-map * s1 s2))
(define factorials
  (cons-stream 1 (mul-streams integers factorials)))
;; p.194
(define (partial-sums s)
  (define (iter i)
    )
  (iter 1))
;;

;;; signal processing

;; p.203
(define (integral integrand init-value dt)
  (define int
    (cons-stream init-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)
;;

;; p.205
(define (solve f y0 dt)
  (define y (integral dy y0 dt))
  (define dy (stream-map f y))
  y)
;;
(define (integral delayed-integrand init-value dt)
  (define int
    (cons-stream init-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)
(define (solve-v2 f y0 dt)
  (define y (integral (my-delay dy) y0 dt))
  (define dy (stream-map f y))
  y)
