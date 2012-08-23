#lang racket
(require "contract.rkt")

(define-struct dv (vec-length next-avail-pos vec) #:mutable)
  
(define (dv:make size)
  (make-dv size 0 (make-vector size)))

(define (dv:length dv) (dv-next-avail-pos dv))

(define (dv:remove-last a-dv)
  (match a-dv
    [(struct dv (_ used vec))
     (set-dv-next-avail-pos! a-dv (sub1 used))
     (vector-set! vec (sub1 used) 0)]))

(define (dv:ref a-dv pos)
  (match a-dv
    [(struct dv (_ _ vec))
     (vector-ref vec pos)]))

(define (dv:set! a-dv pos new-val)
  (match a-dv
    [(struct dv (_ _ vec))
     (vector-set! vec pos new-val)]))

(define (dv:append a-dv item)
  (match a-dv
    [(struct dv (real used vec))
     (if (used . < . real)
         (begin (set-dv-next-avail-pos! a-dv (add1 used))
                (vector-set! vec used item))
         (let ([new-vec 
                (build-vector
                 (* 2 real)
                 (lambda (i) 
                   (if (i . < . used)
                       (vector-ref vec i)
                       0)))])
           (set-dv-vec! a-dv new-vec)
           (set-dv-vec-length! a-dv (* 2 real))
           (dv:append a-dv item)))]))

(define (non-empty-dv? dv)
  ((dv:length dv) . > . 0))

#|Contracts|#
(define vec-length/c natural-number/c)

(define vec/c vector?)

(define vec-pos/c natural-number/c)

(define dv/c (struct/dc dv 
                        [vec-length vec-length/c] 
                        [next-avail-pos (vec) (and/c natural-number/c (<=/c (vector-length vec)))]
                        [vec vec/c]))

(define dv:length/c (-> dv? natural-number/c))

(define dv:make/c (-> natural-number/c dv?))

(define dv:remove-last/c (-> dv? void))

(define dv:ref/c (->i ([a-dv dv?] 
                       [pos (a-dv) (and/c (<=/c (dv:length a-dv) vec-pos/c))])
                      [_ any/c]))

;;Uncomment the following provide and provide/contract* forms to demonstrate the problem.
(provide
 (contract-out [dv? (any/c . -> . boolean?)]
               [dv:make dv:make/c]
               [dv:length dv:length/c]
               [dv:remove-last dv:remove-last/c]
               [dv:ref dv:ref/c]))

(provide/contract*
 #;[dv? (any/c . -> . boolean?)]
 #;[dv:make (exact-nonnegative-integer? . -> . dv?)]
 #;[dv:length (dv? . -> . exact-nonnegative-integer?)]
 #;[dv:remove-last (non-empty-dv? . -> . void?)]
 #;[dv:ref (->d ([dv dv?] [pos exact-nonnegative-integer?]) () 
              #:pre-cond (pos . < . (dv:length dv))
              [r any/c])]
 [dv:set! (->d ([dv dv?] [pos exact-nonnegative-integer?] [val any/c]) () 
               #:pre-cond (pos . < . (dv:length dv))
               [r void])]
 [dv:append (dv? any/c . -> . void)])

;;One of this form or the two immediately above need to be commented out. 
#;(provide/contract*
   [dv? (any/c . -> . boolean?)]
   [dv:make (exact-nonnegative-integer? . -> . dv?)]
   [dv:length (dv? . -> . exact-nonnegative-integer?)]
   [dv:remove-last (non-empty-dv? . -> . void?)]
   [dv:ref (->d ([dv dv?] [pos exact-nonnegative-integer?]) () 
                #:pre-cond (pos . < . (dv:length dv))
                [r any/c])]
   [dv:set! (->d ([dv dv?] [pos exact-nonnegative-integer?] [val any/c]) () 
                 #:pre-cond (pos . < . (dv:length dv))
                 [r void])]
   [dv:append (dv? any/c . -> . void)])
