;;; JitterLisp (almost -*- Scheme -*- for Emacs) test with Peano encoding.

;;; Copyright (C) 2017, 2018 Luca Saiu
;;; Written by Luca Saiu

;;; This file is part of the Jittery Lisp language implementation, distributed as
;;; an example along with Jitter under the same license.

;;; Jitter is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; Jitter is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with Jitter.  If not, see <http://www.gnu.org/licenses/>. */


;;;; Peano fundamental operations.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define peano-zero
  '())

(define (peano-zero? p)
  (null? p))

(define (peano-successor p)
  (cons 'i p))

(define (peano-predecessor p)
  (cdr p))




;;;; Peano recursive operations.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (peano-+ p1 p2)
  (if (peano-zero? p1)
      p2
      (peano-+ (peano-predecessor p1) (peano-successor p2))))

(define (peano-- p1 p2)
  (if (peano-zero? p2)
      p1
      (peano-- (peano-predecessor p1) (peano-predecessor p2))))

(define (peano-* p1 p2)
  (if (peano-zero? p2)
      peano-zero
      (peano-+ p1 (peano-* p1 (peano-predecessor p2)))))

(define (peano-/-acc p1 p2 a)
  (if (peano-> p2 p1)
      a
      (peano-/-acc (peano-- p1 p2) p2 (peano-successor a))))
(define (peano-/ p1 p2)
  (peano-/-acc p1 p2 peano-zero))

(define (peano-remainder p1 p2)
  (peano-- p1
           (peano-* p2 (peano-/-acc p1 p2 peano-zero))))

(define (peano-= p1 p2)
  (cond ((peano-zero? p1)
         (peano-zero? p2))
        ((peano-zero? p2)
         #f)
        (#t
         (peano-= (peano-predecessor p1) (peano-predecessor p2)))))

(define (peano-< p1 p2)
  (cond ((peano-zero? p1)
         (not (peano-zero? p2)))
        ((peano-zero? p2)
         #f)
        (#t
         (peano-< (peano-predecessor p1) (peano-predecessor p2)))))

(define (peano-<= p1 p2)
  (if (peano-= p1 p2)
      #t
      (peano-< p1 p2)))

(define (peano-> p1 p2)
  (peano-< p2 p1))
(define (peano->= p1 p2)
  (peano-<= p2 p1))




;;;; More complex operations.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (peano-fact p)
  (if (peano-zero? p)
      (peano-successor peano-zero)
      (peano-* p (peano-fact (peano-predecessor p)))))

(define (peano-fibo p)
  (if (peano-< p peano-2)
      p
      (peano-+ (peano-fibo (peano-- p peano-2))
               (peano-fibo (peano-- p peano-1)))))
(define (fibo n)
  (if (< n 2)
      n
      (+ (fibo (- n 2))
         (fibo (- n 1)))))

(define (peano-expt b e)
  (if (peano-zero? e)
      (peano-successor peano-zero)
      (peano-* b (peano-expt b (peano-predecessor e)))))

(define (peano-tak x y z)
  (if (peano-<= x y)
      y
      (peano-tak (peano-tak (peano-predecessor x) y z)
                 (peano-tak (peano-predecessor y) z x)
                 (peano-tak (peano-predecessor z) x y))))

(define (tak x y z)
  (if (<= x y)
      y
      (tak (tak (1- x) y z)
           (tak (1- y) z x)
           (tak (1- z) x y))))




;;;; Peano<->fixnum conversion.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (peano->fixnum-acc p a)
  (if (null? p)
      a
      (peano->fixnum-acc (peano-predecessor p) (1+ a))))
(define (peano->fixnum p)
  (peano->fixnum-acc p 0))

(define (fixnum->peano-acc n a)
  (if (zero? n)
      a
      (fixnum->peano-acc (1- n) (peano-successor a))))
(define (fixnum->peano n)
  (fixnum->peano-acc n peano-zero))

(define peano-0
  (fixnum->peano 0))
(define peano-1
  (fixnum->peano 1))
(define peano-2
  (fixnum->peano 2))




;;;; Printing.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show x)
  (display x)
  (newline))




;;;; Main.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(show (peano->fixnum (fixnum->peano 10)))
(show (peano->fixnum (peano-remainder (fixnum->peano 10)
                                      (fixnum->peano 4))))
(show (peano->fixnum (peano-fact (fixnum->peano 6))))
(show (peano->fixnum (peano-expt (fixnum->peano 10)
                                 (fixnum->peano 4))))
(show (peano->fixnum (peano-remainder (peano-expt (fixnum->peano 10)
                                                  (fixnum->peano 4))
                                      (fixnum->peano 7))))
(newline)

;; This is a good way of stressing the GC.
(show (tak 14 8 2))
(show (peano->fixnum (peano-tak (fixnum->peano 14) (fixnum->peano 8) (fixnum->peano 2))))
