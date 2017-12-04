;;; JitterLisp (almost -*- Scheme -*- for Emacs) -- predefined library.

;;; Copyright (C) 2017 Luca Saiu
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

;;(define-macro (my-define n . stuff) (if (symbol? n) `(define ,n ,@stuff) `(define-inlinable ,n ,@stuff)))
;;(define-macro (my-define n . stuff) `(define ,n ,@stuff))

;;;; singleton.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (singleton x)
  (cons x '()))




;;;; last-cons.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (last-cons-iterative xs)
  (if (null? xs)
      (error)
      (begin
        (while (not (null? (cdr xs)))
          (set! xs (cdr xs)))
        xs)))

(define (last-cons-tail-recursive-non-null xs)
  (if (null? (cdr xs))
      xs
      (last-cons-tail-recursive-non-null (cdr xs))))
(define (last-cons-tail-recursive xs)
  (if (null? xs)
      (error)
      (last-cons-tail-recursive-non-null xs)))

(define (last-cons xs)
  (last-cons-iterative xs))





;;;; last.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (last-iterative xs)
  (car (last-cons-iterative xs)))

(define (last-tail-recursive xs)
  (car (last-cons-tail-recursive xs)))

(define (last xs)
  (last-iterative xs))




;;;; length.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (length-iterative xs)
  (let ((res 0))
    (while (not (null? xs))
      (set! res (1+ res))
      (set! xs (cdr xs)))
    res))

(define (length-non-tail-recursive xs)
  (if (null? xs)
    0
    (1+ (length-non-tail-recursive (cdr xs)))))

(define (length-tail-recursive-helper xs acc)
  (if (null? xs)
    acc
    (length-tail-recursive-helper (cdr xs) (1+ acc))))
(define (length-tail-recursive xs)
  (length-tail-recursive-helper xs 0))

(define (length xs)
  (length-iterative xs))




;;;; append-reversed.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (append-reversed-iterative xs ys)
  (let ((res ys))
    (while (not (null? xs))
      (set! res (cons (car xs) res))
      (set! xs (cdr xs)))
    res))

(define (append-reversed-tail-recursive xs ys)
  (if (null? xs)
      ys
      (append-reversed-tail-recursive (cdr xs)
                                      (cons (car xs) ys))))

(define (append-reversed xs ys)
  (append-reversed-iterative xs ys))




;;;; reverse.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reverse-iterative xs)
  (append-reversed-iterative xs '()))

;; This uses append instead of append-non-tail-recursive .
(define (reverse-non-tail-recursive xs)
  (if (null? xs)
      '()
      (append (reverse-non-tail-recursive (cdr xs))
              (singleton (car xs)))))

;; This uses append-non-tail-recursive instead of append .
(define (reverse-really-non-tail-recursive xs)
  (if (null? xs)
      '()
      (append-non-tail-recursive (reverse-non-tail-recursive (cdr xs))
                                 (singleton (car xs)))))

(define (reverse-tail-recursive-helper xs acc)
  (if (null? xs)
      acc
      (reverse-tail-recursive-helper (cdr xs)
                                     (cons (car xs) acc))))
(define (reverse-tail-recursive xs)
  (reverse-tail-recursive-helper xs '()))

(define (reverse xs)
  (reverse-iterative xs))




;;;; reverse!.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (reverse!-iterative xs)
  (if (null? xs)
      '()
      (let ((previous-list '())
            (the-cons xs)
            (following-list #f))
        (while (not (null? the-cons))
          (set! following-list (cdr the-cons))
          (set-cdr! the-cons previous-list)
          (set! previous-list the-cons)
          (set! the-cons following-list))
        previous-list)))

(define (reverse!-tail-recursive-helper outer-cons non-null-inner-list)
  (let ((old-non-null-inner-list-cdr (cdr non-null-inner-list)))
    ;; outer-cons: (A . non-null-inner-list) , actually (A . (B . ?))
    ;; non-null-inner-list: (B . ?)
    (set-cdr! non-null-inner-list outer-cons)
    (if (null? old-non-null-inner-list-cdr)
        non-null-inner-list
        (reverse!-tail-recursive-helper non-null-inner-list
                                        old-non-null-inner-list-cdr))))
(define (reverse!-tail-recursive xs)
  (if (null? xs)
      '()
      (reverse!-tail-recursive-helper '() xs)))

(define (reverse! xs)
  (reverse!-iterative xs))




;;;; append.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (append-iterative xs ys)
  (append-reversed-iterative (reverse-iterative xs) ys))

(define (append-non-tail-recursive xs ys)
  (if (null? xs)
      ys
      (cons (car xs)
            (append-non-tail-recursive (cdr xs) ys))))

(define (append-tail-recursive xs ys)
  (append-reversed-tail-recursive (reverse-tail-recursive xs)
                                  ys))

(define (append xs ys)
  (append-iterative xs ys))




;;;; append!.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (append!-iterative xs ys)
  (if (null? xs)
      ys
      (begin
        (set-cdr! (last-cons-iterative xs) ys)
        xs)))

(define (append!-tail-recursive xs ys)
  (if (null? xs)
      ys
      (begin
        (set-cdr! (last-cons-tail-recursive xs) ys)
        xs)))

(define (append! xs ys)
  (append!-iterative xs ys))




;;;; list-copy.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These return a shallow-copy of the given list: only the spine is
;;; duplicated.

(define (list-copy-iterative xs)
  (if (null? xs)
      '()
      (let* ((res (cons (car xs) #f))
             (last-cons res)
             (new-cons #f))
        (set! xs (cdr xs))
        (while (not (null? xs))
          (set! new-cons (cons (car xs) #f))
          (set-cdr! last-cons new-cons)
          (set! last-cons new-cons)
          (set! xs (cdr xs)))
        (set-cdr! last-cons '())
        res)))

(define (list-copy-tail-recursive xs)
  (reverse!-tail-recursive (reverse-tail-recursive xs)))

(define (list-copy-non-tail-recursive xs)
  (if (null? xs)
      '()
      (cons (car xs)
            (list-copy-non-tail-recursive (cdr xs)))))

(define (list-copy xs)
  (list-copy-iterative xs))




;;;; map-reversed.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map-reversed-iterative f xs)
  (let ((res '()))
    (while (not (null? xs))
      (set! res (cons (f (car xs)) res))
      (set! xs (cdr xs)))
    res))

(define (map-reversed-tail-recursive-helper f xs acc)
  (if (null? xs)
      acc
      (map-reversed-tail-recursive-helper f
                                          (cdr xs)
                                          (cons (f (car xs))
                                                acc))))
(define (map-reversed-tail-recursive f xs)
  (map-reversed-tail-recursive-helper f xs '()))

(define (map-reversed f xs)
  (map-reversed-iterative f xs))




;;;; map.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map-iterative f xs)
  (reverse!-iterative (map-reversed-iterative f xs)))

(define (map-non-tail-recursive f xs)
  (if (null? xs)
      '()
      (cons (f (car xs))
            (map-non-tail-recursive f (cdr xs)))))

(define (map-tail-recursive f xs)
  (reverse!-tail-recursive (map-reversed-tail-recursive f xs)))

(define (map f xs)
  (map-iterative f xs))




;;;; exists?.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: rewrite using cond when I have it.  A return form would also be nice
;; here.
(define (exists?-iterative p xs)
  (let ((res #f)
        (go-on #t))
    (while go-on
      (if (null? xs)
          (set! go-on #f)
          (if (p (car xs))
              (begin
                (set! res #t)
                (set! go-on #f))
              (set! xs (cdr xs)))))
    res))

;; FIXME: rewrite using cond or or when I have them.
(define (exists?-tail-recursive p xs)
  (if (null? xs)
      #f
      (if (p (car xs))
          #t
          (exists?-tail-recursive p (cdr xs)))))

(define (exists? p xs)
  (exists?-iterative p xs))




;;;; for-all?.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: rewrite using cond when I have it.  A return form would also be nice
;; here.
(define (for-all?-iterative p xs)
  (let ((res #t)
        (go-on #t))
    (while go-on
      (if (null? xs)
          (set! go-on #f)
          (if (not (p (car xs)))
              (begin
                (set! res #f)
                (set! go-on #f))
              (set! xs (cdr xs)))))
    res))

;; FIXME: rewrite using cond or and when I have them.
(define (for-all?-tail-recursive p xs)
  (if (null? xs)
      #t
      (if (p (car xs))
          (for-all?-tail-recursive p (cdr xs))
          #f)))

(define (for-all? p xs)
  (for-all?-iterative p xs))




;;;; range-reversed.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (range-reversed-iterative a b)
  (let ((res '()))
    (while (<= a b)
      (set! res (cons a res))
      (set! a (1+ a)))
    res))

(define (range-reversed-non-tail-recursive a b)
  (if (> a b)
      '()
      (cons b (range-reversed-non-tail-recursive a (1- b)))))

(define (range-reversed-non-tail-recursive a b)
  (if (> a b)
      '()
      (cons b (range-reversed-non-tail-recursive a (1- b)))))

(define (range-reversed-tail-recursive-helper a b acc)
  (if (> a b)
      acc
      (range-reversed-tail-recursive-helper (1+ a) b (cons a acc))))
(define (range-reversed-tail-recursive a b)
  (range-reversed-tail-recursive-helper a b '()))

(define (range-reversed a b)
  (range-reversed-iterative a b))




;;;; range.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (range-iterative a b)
  (let ((res '()))
    (while (<= a b)
      (set! res (cons b res))
      (set! b (1- b)))
    res))

(define (range-non-tail-recursive a b)
  (if (> a b)
      '()
      (cons a (range-non-tail-recursive (1+ a) b))))

(define (range-tail-recursive-helper a b acc)
  (if (> a b)
      acc
      (range-tail-recursive-helper a (1- b) (cons b acc))))
(define (range-tail-recursive a b)
  (range-tail-recursive-helper a b '()))

(define (range a b)
  (range-iterative a b))




;;;; iota.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (iota-iterative n)
  (let ((res '()))
    (while (> n 0)
      (set! n (1- n))
      (set! res (cons n res)))
    res))

(define (iota-tail-recursive-helper n acc)
  (if (< n 0)
      acc
      (iota-tail-recursive-helper (1- n) (cons n acc))))
(define (iota-tail-recursive n)
  (iota-tail-recursive-helper (1- n) '()))

(define (iota-non-tail-recursive n)
  (range-non-tail-recursive 0 (1- n)))

(define (iota n)
  (iota-iterative n))




;;;; insert.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: rewrite with cond when I have it.  A return or break form would also
;;; be nice here.
(define (insert-iterative x xs)
  (let ((smaller-elements-reversed '())
        (go-on #t))
    (while go-on
      (if (null? xs)
          (set! go-on #f)
          (if (<= x (car xs))
              (set! go-on #f)
              (begin
                (set! smaller-elements-reversed
                      (cons (car xs) smaller-elements-reversed))
                (set! xs (cdr xs))))))
    (append-reversed-iterative smaller-elements-reversed
                               (cons x xs))))

;;; FIXME: rewrite with cond when I have it.
(define (insert-tail-recursive-helper x xs smaller-elements-reversed)
  (if (null? xs)
      (append-reversed-tail-recursive smaller-elements-reversed
                                      (singleton x))
      (if (<= x (car xs))
          (append-reversed-tail-recursive smaller-elements-reversed
                                          (cons x xs))
          (insert-tail-recursive-helper x
                                        (cdr xs)
                                        (cons (car xs)
                                              smaller-elements-reversed)))))
(define (insert-tail-recursive x xs)
  (insert-tail-recursive-helper x xs '()))

;;; FIXME: rewrite with cond when I have it.
(define (insert-non-tail-recursive x xs)
  (if (null? xs)
      (singleton x)
      (if (<= x (car xs))
          (cons x xs)
          (cons (car xs)
                (insert-non-tail-recursive x (cdr xs))))))

(define (insert x xs)
  (insert-iterative x xs))




;;;; insert!.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Destructively turn (A . B) into (x . (A . B)).
(define (insert-as-first! x a-cons)
  (let ((new-cons (cons (car a-cons) (cdr a-cons))))
    (set-car! a-cons x)
    (set-cdr! a-cons new-cons)))

;;; Destructively turn (A . B) into (A . (x . B)).
(define (insert-as-second! x a-cons)
  (let ((new-cons (cons x (cdr a-cons))))
    (set-cdr! a-cons new-cons)))

(define (insert!-iterative-non-null x xs)
  (let ((go-on #t))
    (while go-on
      ;; Here I can still assume that xs is not ().
      (if (< x (car xs))
          (begin
            (insert-as-first! x xs)
            (set! go-on #f))
          (if (null? (cdr xs))
              (begin
                (insert-as-second! x xs)
                (set! go-on #f))
              (set! xs (cdr xs)))))))
(define (insert!-iterative x xs)
  (if (null? xs)
      (singleton x)
      (begin
        (insert!-iterative-non-null x xs)
        xs)))

(define (insert!-tail-recursive-non-null x xs)
  (if (< x (car xs))
      (insert-as-first! x xs)
      (if (null? (cdr xs))
          (insert-as-second! x xs)
          (insert!-tail-recursive-non-null x (cdr xs)))))
(define (insert!-tail-recursive x xs)
  (if (null? xs)
      (singleton x)
      (begin
        (insert!-tail-recursive-non-null x xs)
        xs)))

(define (insert! x xs)
  (insert!-iterative x xs))




;;;; insertion-sort.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (insertion-sort-iterative xs)
  (let ((res '()))
    (while (not (null? xs))
      (set! res (insert!-iterative (car xs) res))
      (set! xs (cdr xs)))
    res))

(define (insertion-sort-tail-recursive-helper xs acc)
  (if (null? xs)
      acc
      (insertion-sort-tail-recursive-helper (cdr xs)
                                            (insert!-tail-recursive (car xs)
                                                                    acc))))
(define (insertion-sort-tail-recursive xs)
  (insertion-sort-tail-recursive-helper xs '()))

(define (insertion-sort xs)
  (insertion-sort-iterative xs))




;;;; sort.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sort xs)
  (insertion-sort xs))




;;;; Scratch.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define xs (reverse! (iota 10000000)))
