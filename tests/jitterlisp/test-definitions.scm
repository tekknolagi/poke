;;; JitterLisp (-*- Scheme -*- for the purposes of Emacs) -- test definitions.

;;; Copyright (C) 2017, 2018 Luca Saiu
;;; Written by Luca Saiu

;;; This file is part of the JitterLisp language implementation, distributed as
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


;;;; Classic recursive procedures, also rewritten as iterative.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (fibo n)
  (if (< n 2)
      n
      (+ (fibo (- n 2))
         (fibo (- n 1)))))

(define-constant (euclid a b)
  (cond ((= a b)
         a)
        ((< a b)
         (euclid a (- b a)))
        (#t
         (euclid (- a b) b))))

(define-constant (euclid-i a b)
  (while (<> a b)
    (if (< a b)
        (set! b (- b a))
        (set! a (- a b))))
  a)

(define-constant (fact n)
  (if (zero? n)
      1
      (* n (fact (- n 1)))))

(define-constant (fact-tail-recursive-helper n acc)
  (if (zero? n)
      acc
      (fact-tail-recursive-helper (- n 1)
                                  (* acc n))))
(define-constant (fact-tail-recursive n)
  (fact-tail-recursive-helper n 1))

(define-constant (fact-i n)
  (let ((res 1))
    (while (not (zero? n))
      (set! res (* res n))
      (set! n (- n 1)))
    res))

(define-constant (gauss n)
  (if (zero? n)
      0
      (+ n (gauss (- n 1)))))

(define-constant (gauss-tail-recursive-helper n acc)
  (if (zero? n)
      acc
      (gauss-tail-recursive-helper (- n 1)
                                   (+ acc n))))
(define-constant (gauss-tail-recursive n)
  (gauss-tail-recursive-helper n 0))

(define-constant (gauss-i n)
  (let ((res 0))
    (while (not (zero? n))
      (set! res (+ res n))
      (set! n (- n 1)))
    res))

(define-constant (count a)
  (if (zero? a)
      0
      (count (- a 1))))

(define-constant (count-i a)
  (while (not (zero? a))
    (set! a (- a 1)))
  a)

(define-constant (count2 a b)
  (if (zero? a)
      b
      (count2 (- a 1) (+ b 1))))

(define-constant (count2-i a b)
  (while (not (zero? a))
    (set! a (- a 1))
    (set! b (+ b 1)))
  b)




;;;; A test of complex conditionals, using and and case.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (month->days m)
  (unless (and (>= m 1) (<= m 13))
    (error `(the month ,m is not between 1 and 12)))
  (case m
    ((9 4 6 11) 30)
    ((2) 28)
    (else 31)))




;;;; Two mutually recursive procedures.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (even?-tail-recursive n)
  (cond ((zero? n)
         #t)
        ((= n 1)
         #f)
        (#t
         (odd?-tail-recursive (- n 1)))))

(define-constant (odd?-tail-recursive n)
  (cond ((zero? n)
         #f)
        ((= n 1)
         #t)
        (#t
         (even?-tail-recursive (- n 1)))))




;;;; Compile a few predefined procedures.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Compile a few predefined procedures -- only a few.  This way I will
;;; have a lot of interplay between interpreted and compiled code, which
;;; is useful to test.

(map interpreted-closure-compile!
     (list 1+ 1- cdr))




;;;; Compile everything we defined here.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(dolist (name '(fibo
                euclid euclid-i
                fact
                fact-tail-recursive-helper fact-tail-recursive
                fact-i
                gauss
                gauss-tail-recursive-helper gauss-tail-recursive
                gauss-i
                count
                count-i
                count2
                count2-i
                month->days
                even?-tail-recursive odd?-tail-recursive))
  (interpreted-closure-compile! (symbol-global name)))
