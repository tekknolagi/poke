;;; JitterLisp (let's say -*- Scheme -*- for the purposes of Emacs) -- library.

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




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Force the library to be run at most once.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Fail if the library has been loaded already.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (defined? 'jitterlisp-library-loaded)
    (error '(you are trying to load the library more than once))
    #t)




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Arithmetic and number library.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; Parity: even? and odd?.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (even? n)
  (zero? (remainder n 2)))

(define-constant (odd? n)
  (not (zero? (remainder n 2))))




;;;; number?.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (number? n)
  (fixnum? n)) ;; There is only one number type right now.




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Conses.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; Composed cons selectors.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; I define these early because some composed selectors are used within the
;;; implementation of high-level macros.

;; Length 2.
(define-constant (caar x) (car (car x)))
(define-constant (cadr x) (car (cdr x)))
(define-constant (cdar x) (cdr (car x)))
(define-constant (cddr x) (cdr (cdr x)))

;; Length 3.
(define-constant (caaar x) (car (caar x)))
(define-constant (caadr x) (car (cadr x)))
(define-constant (cadar x) (car (cdar x)))
(define-constant (caddr x) (car (cddr x)))
(define-constant (cdaar x) (cdr (caar x)))
(define-constant (cdadr x) (cdr (cadr x)))
(define-constant (cddar x) (cdr (cdar x)))
(define-constant (cdddr x) (cdr (cddr x)))

;; Length 4.
(define-constant (caaaar x) (car (caaar x)))
(define-constant (caaadr x) (car (caadr x)))
(define-constant (caadar x) (car (cadar x)))
(define-constant (caaddr x) (car (caddr x)))
(define-constant (cadaar x) (car (cdaar x)))
(define-constant (cadadr x) (car (cdadr x)))
(define-constant (caddar x) (car (cddar x)))
(define-constant (cadddr x) (car (cdddr x)))
(define-constant (cdaaar x) (cdr (caaar x)))
(define-constant (cdaadr x) (cdr (caadr x)))
(define-constant (cdadar x) (cdr (cadar x)))
(define-constant (cdaddr x) (cdr (caddr x)))
(define-constant (cddaar x) (cdr (cdaar x)))
(define-constant (cddadr x) (cdr (cdadr x)))
(define-constant (cdddar x) (cdr (cddar x)))
(define-constant (cddddr x) (cdr (cdddr x)))




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Quasiquoting.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; quasiquote-procedure.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Define temporary list functions of a few different arities.  It is
;;; not worth the trouble to define list as a variadic macro without
;;; high-level macros or quasiquoting.
(define-constant (list0)         '())
(define-constant (list1 x)       (cons x '()))
(define-constant (list2 x y)     (cons x (cons y '())))
(define-constant (list3 x y z)   (cons x (cons y (cons z '()))))
(define-constant (list4 x y z t) (cons x (cons y (cons z (cons t '())))))

(define-constant (qq-append xs ys)
  (if (list? xs)
      (append xs ys)
      (error '(unquote-splicing argument evaluates to non-list))))

(define-constant (qq-atom x)
  ;; `',x
  (list2 'quote
         x))

(define-constant (qq-recursive x depth)
  (if (cons? x)
      (qq-recursive-cons (car x) (cdr x) depth)
      (qq-atom x)))

;;; ~/r6rs.pdf, §11.17 "Quasiquotation".

(define-constant (qq-recursive-cons x-car x-cdr depth)
  (cond ((eq? x-car 'quasiquote)
         ;;`(list 'quasiquote ,(qq-recursive (car x-cdr) (1+ depth)))
         (list3 'list2
                ''quasiquote
                (qq-recursive (car x-cdr) (1+ depth))))
        ((eq? x-car 'unquote)
         ;; FIXME: decide what to do with (cdr x-cdr).
         (if (zero? depth)
             (car x-cdr)
             ;; `(cons 'unquote ,(qq-recursive x-cdr (1- depth)))
             (list3 'cons
                    ''unquote
                    (qq-recursive x-cdr (1- depth)))))
        ((eq? x-car 'unquote-splicing)
         ;; FIXME: decide what to do with (cdr x-cdr).  This case is
         ;; probably different from the one above.  Look at R7RS as well.
         (if (zero? depth)
             (begin
               ;; There are several possibilities in this case.
               ;; FIXME: consider them, including returning (car x-cdr).
               (error '(invalid context for unquote-splicing)))
             (begin
               ;; `(cons 'unquote-splicing ,(qq-recursive x-cdr (1- depth)))
               (list3 'cons
                      ''unquote-splicing
                      (qq-recursive x-cdr (1- depth))))))
        (#t
         ;;`(append ,(qq-recursive-as-car x-car depth)
         ;;         ,(qq-recursive x-cdr depth))
         (list3 'append ;; no qq-append here: qq-recursive-as-car returns a list.
                (qq-recursive-as-car x-car depth)
                (qq-recursive x-cdr depth)))))

;;; Return an s-expression evaluating to a singleton list containing the
;;; given object.
(define-constant (qq-sigleton-expression expression)
  (list2 'list1
         expression))

;;; Return an s-expression evaluating to something equivalent to a variadic
;;; call to the list function of the given arguments.
(define-constant (qq-variadic-list-expression args)
  (if (null? args)
      ''()
      ;;`(cons ,(car args) ,(qq-variadic-list-expression (cdr args)))))
      (list3 'cons
             (car args)
             (qq-variadic-list-expression (cdr args)))))

;;; Same as qq-variadic-list-expression but for a variadic append.
(define-constant (qq-variadic-append-expression args)
  (if (null? args)
      ''()
      ;;`(append ,(car args) ,(qq-variadic-append-expression (cdr args)))))
      (list3 'qq-append
             (car args)
             (qq-variadic-append-expression (cdr args)))))

;;; The car of a quasiquoted cons expands to a *list*, to be combined
;;; to the expansion of the cdr by appending, not consing.  This allows
;;; for simpler handling of unquote-splicing; when not splicing we just
;;; generate a singleton list.
(define-constant (qq-recursive-as-car x depth)
  (if (cons? x)
      (qq-recursive-cons-as-car (car x) (cdr x) depth)
      ;; `(list ,(qq-atom x))
      (qq-sigleton-expression (qq-atom x))))

;;; Expand a cons which is the car of a bigger quasiquoted s-expression;
;;; therefore, expand to a list as qq-recursive-as-car does.
(define-constant (qq-recursive-cons-as-car x-car x-cdr depth)
  (cond ((eq? x-car 'quasiquote)
         ;; (qq-sigleton-expression `(cons 'quasiquote ,(qq-recursive x-cdr (1+ depth))))
         (qq-sigleton-expression (list3 'cons
                                        ''quasiquote
                                        (qq-recursive x-cdr (1+ depth)))))
        ((eq? x-car 'unquote)
         (if (zero? depth)
             (qq-variadic-list-expression x-cdr)
             ;; `(list (cons 'unquote ,(qq-recursive x-cdr (1- depth))))
             (list2 'list1
                    (list3 'cons
                           ''unquote
                           (qq-recursive x-cdr (1- depth))))))
        ((eq? x-car 'unquote-splicing)
         (if (zero? depth)
             (qq-variadic-append-expression x-cdr)
             ;;`(list (cons 'unquote-splicing ,(qq-recursive x-cdr (1- depth))))
             (list2 'list1
                    (list3 'cons
                           ''unquote-splicing
                           (qq-recursive x-cdr (1- depth))))))
        (#t
         ;;`(list (append ,(qq-recursive-as-car x-car depth)
         ;;               ,(qq-recursive x-cdr depth))))
         (list2 'list1
                (list3 'append
                       (qq-recursive-as-car x-car depth)
                       (qq-recursive x-cdr depth))))))

;;; Quasiquoting as a procedure.
(define-constant (quasiquote-procedure x)
  (qq-recursive x 0))




;;;; quasiquote.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Now we can finally define quasiquote as a (low-level) macro.  It is trivial:
;;; low-level-macro-args is bound to all of the arguments of quasiquote , and we
;;; want to fail if those arguments are not a singleton list.  If the arguments
;;; are a singleton list we just call quasiquote-procedure on their car.
(define-constant quasiquote
  (low-level-macro
    (cond ((null? low-level-macro-args)
           (error '(quasiquote with zero arguments)))
          ((non-cons? low-level-macro-args)
           (error '(quasiquote arguments not a list)))
          ((non-null? (cdr low-level-macro-args))
           (error '(quasiquote arguments more than one or not a list)))
          (#t
           (quasiquote-procedure (car low-level-macro-args))))))




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; List library.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; singleton.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (singleton x)
  (cons x '()))




;;;; list?.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: a return statement would be nice here.
(define-constant (list?-iterative xs)
  (let* ((go-on #t)
         (res #t))
    (while go-on
      (cond ((null? xs)
             (set! go-on #f))
            ((non-cons? xs)
             (set! res #f)
             (set! go-on #f))
            (#t
             (set! xs (cdr xs)))))
    res))

(define-constant (list?-tail-recursive xs)
  (cond ((null? xs)
         #t)
        ((cons? xs)
         (list?-tail-recursive (cdr xs)))
        (#t
         #f)))

(define-constant (list? xs)
  (list?-iterative xs))




;;;; replicate.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (replicate-iterative n x)
  (let* ((res '()))
    (while (> n 0)
      (set! res (cons x res))
      (set! n (1- n)))
    res))

(define-constant (replicate-tail-recursive-helper n x acc)
  (if (zero? n)
      acc
      (replicate-tail-recursive-helper (1- n) x (cons x acc))))
(define-constant (replicate-tail-recursive n x)
  (replicate-tail-recursive-helper n x '()))

(define-constant (replicate-non-tail-recursive n x)
  (if (zero? n)
      '()
      (cons x (replicate-non-tail-recursive (1- n) x))))

(define-constant (replicate n x)
  (replicate-iterative n x))

;; Just an alias.
(define-constant (make-list n x)
  (replicate n x))




;;;; last-cons.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (last-cons-iterative xs)
  (if (null? xs)
      (error '(last-cons-iterative: () argument))
      (begin
        (while (not (null? (cdr xs)))
          (set! xs (cdr xs)))
        xs)))

(define-constant (last-cons-tail-recursive-non-null xs)
  (if (null? (cdr xs))
      xs
      (last-cons-tail-recursive-non-null (cdr xs))))
(define-constant (last-cons-tail-recursive xs)
  (if (null? xs)
      (error)
      (last-cons-tail-recursive-non-null xs)))

(define-constant (last-cons xs)
  (last-cons-iterative xs))





;;;; last.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (last-iterative xs)
  (car (last-cons-iterative xs)))

(define-constant (last-tail-recursive xs)
  (car (last-cons-tail-recursive xs)))

(define-constant (last xs)
  (last-iterative xs))




;;;; length.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (length-iterative xs)
  (let* ((res 0))
    (while (non-null? xs)
      (set! res (1+ res))
      (set! xs (cdr xs)))
    res))

(define-constant (length-non-tail-recursive xs)
  (if (null? xs)
      0
      (1+ (length-non-tail-recursive (cdr xs)))))

(define-constant (length-tail-recursive-helper xs acc)
  (if (null? xs)
      acc
      (length-tail-recursive-helper (cdr xs) (1+ acc))))
(define-constant (length-tail-recursive xs)
  (length-tail-recursive-helper xs 0))

(define-constant (length xs)
  (length-iterative xs))




;;;; append-reversed.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (append-reversed-iterative xs ys)
  (let* ((res ys))
    (while (non-null? xs)
      (set! res (cons (car xs) res))
      (set! xs (cdr xs)))
    res))

(define-constant (append-reversed-tail-recursive xs ys)
  (if (null? xs)
      ys
      (append-reversed-tail-recursive (cdr xs)
                                      (cons (car xs) ys))))

(define-constant (append-reversed xs ys)
  (append-reversed-iterative xs ys))




;;;; reverse.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (reverse-iterative xs)
  (append-reversed-iterative xs '()))

;; This uses append instead of append-non-tail-recursive .
(define-constant (reverse-non-tail-recursive xs)
  (if (null? xs)
      '()
      (append (reverse-non-tail-recursive (cdr xs))
              (singleton (car xs)))))

;; This uses append-non-tail-recursive instead of append .
(define-constant (reverse-really-non-tail-recursive xs)
  (if (null? xs)
      '()
      (append-non-tail-recursive (reverse-non-tail-recursive (cdr xs))
                                 (singleton (car xs)))))

(define-constant (reverse-tail-recursive-helper xs acc)
  (if (null? xs)
      acc
      (reverse-tail-recursive-helper (cdr xs)
                                     (cons (car xs) acc))))
(define-constant (reverse-tail-recursive xs)
  (reverse-tail-recursive-helper xs '()))

(define-constant (reverse xs)
  (reverse-iterative xs))




;;;; reverse!.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (reverse!-iterative xs)
  (if (null? xs)
      '()
      (let* ((previous-list '())
             (the-cons xs)
             (following-list #f))
        (while (not (null? the-cons))
          (set! following-list (cdr the-cons))
          (set-cdr! the-cons previous-list)
          (set! previous-list the-cons)
          (set! the-cons following-list))
        previous-list)))

(define-constant (reverse!-tail-recursive-helper outer-cons non-null-inner-list)
  (let* ((old-non-null-inner-list-cdr (cdr non-null-inner-list)))
    ;; outer-cons: (A . non-null-inner-list) , actually (A . (B . ?))
    ;; non-null-inner-list: (B . ?)
    (set-cdr! non-null-inner-list outer-cons)
    (if (null? old-non-null-inner-list-cdr)
        non-null-inner-list
        (reverse!-tail-recursive-helper non-null-inner-list
                                        old-non-null-inner-list-cdr))))
(define-constant (reverse!-tail-recursive xs)
  (if (null? xs)
      '()
      (reverse!-tail-recursive-helper '() xs)))

(define-constant (reverse! xs)
  (reverse!-iterative xs))




;;;; append.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (append-iterative xs ys)
  (append-reversed-iterative (reverse-iterative xs) ys))

(define-constant (append-non-tail-recursive xs ys)
  (if (null? xs)
      ys
      (cons (car xs)
            (append-non-tail-recursive (cdr xs) ys))))

(define-constant (append-tail-recursive xs ys)
  (append-reversed-tail-recursive (reverse-tail-recursive xs)
                                  ys))

(define-constant (append xs ys)
  (append-iterative xs ys))




;;;; append!.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (append!-iterative xs ys)
  (if (null? xs)
      ys
      (begin
        (set-cdr! (last-cons-iterative xs) ys)
        xs)))

(define-constant (append!-tail-recursive xs ys)
  (if (null? xs)
      ys
      (begin
        (set-cdr! (last-cons-tail-recursive xs) ys)
        xs)))

(define-constant (append! xs ys)
  (append!-iterative xs ys))




;;;; flatten-reversed.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (flatten-reversed-iterative list-of-lists)
  (let* ((res '()))
    (while (non-null? list-of-lists)
      (set! res (append-iterative (car list-of-lists) res))
      (set! list-of-lists (cdr list-of-lists)))
    res))

(define-constant (flatten-reversed-tail-recursive-helper reversed-list acc)
  (if (null? reversed-list)
      acc
      (flatten-reversed-tail-recursive-helper
          (cdr reversed-list)
          (append-tail-recursive (car reversed-list) acc))))
(define-constant (flatten-reversed-tail-recursive reversed-list)
  (flatten-reversed-tail-recursive-helper reversed-list '()))

(define-constant (flatten-reversed reversed-list)
  (flatten-reversed-iterative reversed-list))




;;;; flatten-reversed!.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (flatten-reversed!-iterative list-of-lists)
  (let* ((res '()))
    (while (non-null? list-of-lists)
      (set! res (append!-iterative (car list-of-lists) res))
      (set! list-of-lists (cdr list-of-lists)))
    res))

(define-constant (flatten-reversed! list-of-lists)
  (flatten-reversed!-iterative list-of-lists))




;;;; flatten.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (flatten-iterative list-of-lists)
  (flatten-reversed-iterative (reverse-iterative list-of-lists)))

(define-constant (flatten-tail-recursive list-of-lists)
  (flatten-reversed-tail-recursive (reverse-tail-recursive list-of-lists)))

(define-constant (flatten-non-tail-recursive list-of-lists)
  (if (null? list-of-lists)
      '()
      (append-non-tail-recursive
          (car list-of-lists)
          (flatten-non-tail-recursive (cdr list-of-lists)))))

(define-constant (flatten list-of-lists)
  (flatten-iterative list-of-lists))




;;;; flatten!.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (flatten!-iterative list-of-lists)
  (flatten-reversed!-iterative (reverse!-iterative list-of-lists)))

(define-constant (flatten!-tail-recursive list-of-lists)
  (flatten-reversed-tail-recursive (reverse!-tail-recursive list-of-lists)))

(define-constant (flatten!-non-tail-recursive list-of-lists)
  (if (null? list-of-lists)
      '()
      ;; I don't have an append!-non-tail-recursive, as it doesn't seem very
      ;; reasonable.
      (append! (car list-of-lists)
               (flatten!-non-tail-recursive (cdr list-of-lists)))))

(define-constant (flatten! list-of-lists)
  (flatten!-iterative list-of-lists))




;;;; list-copy.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These return a shallow-copy of the given list: only the spine is
;;; duplicated.

(define-constant (list-copy-iterative xs)
  (if (null? xs)
      '()
      (let* ((res (cons (car xs) #f))
             (last-cons res)
             (new-cons #f))
        (set! xs (cdr xs))
        (while (non-null? xs)
          (set! new-cons (cons (car xs) #f))
          (set-cdr! last-cons new-cons)
          (set! last-cons new-cons)
          (set! xs (cdr xs)))
        (set-cdr! last-cons '())
        res)))

(define-constant (list-copy-tail-recursive xs)
  (reverse!-tail-recursive (reverse-tail-recursive xs)))

(define-constant (list-copy-non-tail-recursive xs)
  (if (null? xs)
      '()
      (cons (car xs)
            (list-copy-non-tail-recursive (cdr xs)))))

(define-constant (list-copy xs)
  (list-copy-iterative xs))




;;;; cdr-or-nil.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (cdr-or-nil xs)
  (if (null? xs)
      ()
      (cdr xs)))




;;;; nth-cons-or-nil.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (nth-cons-or-nil-iterative n xs)
  ;; A break or return form would be useful here.  Even better I could also
  ;; iterate on an and condition, but we have no and macro yet.
  (let* ((go-on #t))
    (while go-on
      (cond ((zero? n)
             (set! go-on #f))
            ((null? xs)
             (set! go-on #f))
            (#t
             (set! n (1- n))
             (set! xs (cdr xs)))))
    xs))

(define-constant (nth-cons-or-nil-tail-recursive n xs)
  (cond ((zero? n)
         xs)
        ((null? xs)
         ())
        (#t
         (nth-cons-or-nil-tail-recursive (1- n) (cdr xs)))))

(define-constant (nth-cons-or-nil n xs)
  (nth-cons-or-nil-iterative n xs))




;;;; nth-cons.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (nth-cons n xs)
  (let* ((c (nth-cons-or-nil n xs)))
    (if (null? c)
        (error '(nth-cons: list too short))
        c)))




;;;; nth.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (nth-iterative xs n)
  (while (non-zero? n)
    (set! xs (cdr xs))
    (set! n (1- n)))
  (car xs))

(define-constant (nth-tail-recursive xs n)
  (if (zero? n)
      (car xs)
      (nth-tail-recursive (cdr xs) (1- n))))

(define-constant (nth xs n)
  (nth-iterative xs n))




;;;; take, take-reversed.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A break or return form would be useful here.
(define-constant (take-reversed-iterative xs n)
  (let* ((res '())
         (go-on #t))
    (while go-on
      (cond ((null? xs)
             (set! go-on #f))
            ((zero? n)
             (set! go-on #f))
            (#t
             (set! res (cons (car xs) res))
             (set! xs (cdr xs))
             (set! n (1- n)))))
    res))
(define-constant (take-iterative xs n)
  (reverse!-iterative (take-reversed-iterative xs n)))

(define-constant (take-tail-recursive xs n)
  (reverse!-tail-recursive (take-reversed-tail-recursive xs n)))

(define-constant (take-non-tail-recursive xs n)
  (cond ((zero? n)
         '())
        ((null? xs)
         '())
        (#t
         (cons (car xs) (take-non-tail-recursive (cdr xs) (1- n))))))

(define-constant (take-reversed-tail-recursive-helper xs n acc)
  (cond ((zero? n)
         acc)
        ((null? xs)
         acc)
        (#t
         (take-reversed-tail-recursive-helper (cdr xs)
                                              (1- n)
                                              (cons (car xs) acc)))))
(define-constant (take-reversed-tail-recursive xs n)
  (take-reversed-tail-recursive-helper xs n '()))

(define-constant (take-reversed xs n)
  (take-reversed-iterative xs n))

(define-constant (take xs n)
  (take-iterative xs n))




;;;; take!
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (take!-iterative xs n)
  (if (zero? n)
      ()
      (let* ((n-1-th-cons-or-nil (nth-cons-or-nil-iterative (1- n) xs)))
        (if (non-null? n-1-th-cons-or-nil)
            (set-cdr! n-1-th-cons-or-nil ()))
        xs)))

(define-constant (take!-tail-recursive-helper xs n)
  (cond ((= n 1)
         (if (null? xs)
             'do-nothing
             (set-cdr! xs '())))
        ((null? xs))
        (#t
         (take!-tail-recursive-helper (cdr xs) (1- n)))))
(define-constant (take!-tail-recursive xs n)
  (if (zero? n)
      '()
      (begin
        (take!-tail-recursive-helper xs n)
        xs)))

(define-constant (take! xs n)
  (take!-iterative xs n))




;;;; drop.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A break or return form would be useful here.
(define-constant (drop-iterative xs n)
  (let* ((go-on #t))
    (while go-on
      (cond ((null? xs)
             (set! go-on #f))
            ((zero? n)
             (set! go-on #f))
            (#t
             (set! xs (cdr xs))
             (set! n (1- n)))))
    xs))

(define-constant (drop-tail-recursive xs n)
  (cond ((zero? n)
         xs)
        ((null? xs)
         '())
        (#t
         (drop-tail-recursive (cdr xs) (1- n)))))

(define-constant (drop xs n)
  (drop-iterative xs n))




;;;; drop!.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (drop! xs n)
  (if (zero? n)
      xs
      (let* ((n-1-th-cons-or-nil (nth-cons-or-nil (1- n) xs)))
        (if (non-null? n-1-th-cons-or-nil)
            (let* ((old-cdr (cdr n-1-th-cons-or-nil)))
              (set-cdr! n-1-th-cons-or-nil ())
              old-cdr)
            ()))))




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; High-level macros.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; At this point I have a good implementation for the fundamental list
;;; procedures, which lets me use quasiquoting in macros.



;;;; destructuring-bind-procedure.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Rationale: Every low-level macro has exactly *one* formal parameter, always
;;; named low-level-macro-args and bound to the entire macro call cdr.  Thru
;;; destructuring-bind we can turn convenient high-level macros with named
;;; formals into low-level macros, by binding each formal to a
;;; low-level-macro-args component.
;;;
;;; The mapping from high-level to low-level macros is epsilon-style; at least,
;;; I designed it the first time for epsilon.  I wouldn't be very surprised if
;;; somebody had the same idea before, which may be the reason why
;;; destructuring-bind exists in Common Lisp -- I discovered it after devising
;;; my own mechanism, which originally had a different name but behaved
;;; identically.
;;; Update: the answer may be in http://www.lispworks.com/documentation/HyperSpec/Issues/iss130_w.htm
;;; I've discovered Emacs Lisp's seq-let , less powerful but conceptually very
;;; similar, only now in 2017.

;;; Return the result of destructuring-bind with the given formal template bound
;;; to low-level-macro-args or some sub-component of it.  The result is code,
;;; not executed by this function: of course we cannot do that until we have an
;;; actual value for low-level-macro-args .
;;; Notice that component may be evaluated multiple times in the returned code,
;;; and therefore should be a literal or a variable.  This is ensured out of
;;; this recursive procedure, by calling it with an appropriate actual.
(define-constant (destructuring-bind-recursive formals-template
                                      component
                                      body-forms)
  (cond ((null? formals-template)
         ;; There is nothing to bind in the template.  Return code to check
         ;; that there are also no actuals, and then either proceeds or fails.
         `(if (null? ,component)
              (begin
                ,@body-forms)
              (error `(destructuring-bind: excess actuals: ,,component))))
        ((symbol? formals-template)
         ;; The macro template is dotted, or this is a recursive call on a
         ;; template car: in either case bind one variable to every actual.
         `(let* ((,formals-template ,component))
            ,@body-forms))
        ((cons? formals-template)
         ;; Bind both the car and the cdr.  For efficiency's sake name the two
         ;; sub-components in the generated code.
         (let* ((car-name (gensym))
                (cdr-name (gensym)))
           `(let* ((,car-name (car ,component))
                   (,cdr-name (cdr ,component)))
              ,(destructuring-bind-recursive
                  (car formals-template)
                  car-name
                  ;; The inner quasiquoting serves to make a (singleton) list of
                  ;; the body forms.
                  `(,(destructuring-bind-recursive (cdr formals-template)
                                                   cdr-name
                                                   body-forms))))))
        ((vector? formals-template)
         (error `(vector ,formals-template in macro formals template)))
        (#t
         ;; The template is, hopefully, something which can be compared with eq?
         ;; .  Return code checking that it's equal to the actual and in that
         ;; case proceeds without binding anything.
         `(if (eq? ,formals-template ,component)
              (begin
                ,@body-forms)
              (error `(non-matching template argument: ,formals-template
                                    ,component))))))

;;; The args argument represents "actuals" in a symbolic form; their values
;;; may not necessarily be known yet.
;;; Example:
;;; (destructuring-bind-procedure
;;;   '(a b)
;;;   'some-arguments
;;;   '((display a) (display b)))
;;; This would return code binding a and b as local variable to the car and
;;; cadr of some-arguments, assumed to be bound, and display them.
(define-constant (destructuring-bind-procedure formals-template args body-forms)
  (let* ((args-value-name (gensym)))
    `(let* ((,args-value-name ,args))
       ,(destructuring-bind-recursive formals-template
                                      args-value-name
                                      body-forms))))

;; FIXME: check that the formals-template doesn't require non-linear bindings.




;;;; destructuring-bind.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This will be convenient to define high-level macros on top of low-level
;;; macros, by destructuring the one low-level macro argument.

;;; Arguments: template structure . body-forms Evaluate structure and locally
;;; bind its components with the variables in the template; return the result of
;;; evaluating the body forms with the bindings visible.
(define-constant destructuring-bind
  (low-level-macro
    (let* ((template (car low-level-macro-args))
           (structure (cadr low-level-macro-args))
           (body-forms (cddr low-level-macro-args)))
      (destructuring-bind-procedure template structure body-forms))))




;;;; High-level macros.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Expand to a form evaluating to a high-level macro.
;;; Arguments: formals . body-forms
;;; Scheme-style, where formals can be an improper list; however there is no
;;; macro name here.
(define-constant macro
  (low-level-macro
    (let* ((macro-formals (car low-level-macro-args))
           (macro-body-forms (cdr low-level-macro-args)))
      `(low-level-macro
         (destructuring-bind ,macro-formals
                             low-level-macro-args
                             ,@macro-body-forms)))))

;;; Globally define a high-level named macro.
;;; Arguments: (name . formals) . body-forms
;;; Scheme-style, where formals can be an improper list.
(define-constant define-macro
  (macro ((macro-name . macro-formals) . macro-body-forms)
    `(define ,macro-name
       (macro ,macro-formals ,@macro-body-forms))))

;;; High-level macros are now usable.




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; More advanced list library.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; alist functions.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (alist? x)
  (cond ((null? x)
         #t)
        ((non-cons? x)
         #f)
        ((cons? (car x))
         (alist? (cdr x)))
        (#t
         #f)))

(define-constant (assq key alist)
  (cond ((null? alist)
         #f)
        ((eq? (caar alist) key)
         (car alist))
        (#t
         (assq key (cdr alist)))))

(define-constant (rassq value alist)
  (cond ((null? alist)
         #f)
        ((eq? (cdar alist) value)
         (car alist))
        (#t
         (rassq value (cdr alist)))))

(define-constant (del-assq object alist)
  (cond ((null? alist)
         '())
        ((eq? (caar alist) object)
         (del-assq object (cdr alist)))
        (#t
         (cons (car alist) (del-assq object (cdr alist))))))

;;; An obvious extension of del-assq, returning a copy of the alist with the
;;; bindings for all of the given keys removed.
(define-constant (del-assq-list objects alist)
  (if (null? objects)
      alist
      (del-assq-list (cdr objects)
                     (del-assq (car objects) alist))))

;; FIXME: implement del-assq! .

(define-constant (alist-copy alist)
  (let* ((res '())
         (first-cons #f))
    (while (non-null? alist)
      (set! first-cons (car alist))
      (set! res (cons (cons (car first-cons) (cdr first-cons)) res))
      (set! alist (cdr alist)))
    (reverse! res)))

(define-constant (alist-get key alist)
  (let* ((a-cons (assq key alist)))
    (if (cons? a-cons)
        (cdr a-cons)
        (error `(alist-get: key ,key not found in alist ,alist)))))




;;;; zip-reversed.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (zip-reversed-iterative as bs)
  (let* ((res ()))
    (while (non-null? as)
      (if (null? bs)
          (error '(zip-reversed-iterative: first list longer))
          (begin
            (set! res (cons (cons (car as) (car bs))
                            res))
            (set! as (cdr as))
            (set! bs (cdr bs)))))
    (if (non-null? bs)
        (error '(zip-reversed-iterative: second list longer)))
    res))

(define-constant (zip-reversed-tail-recursive-helper as bs acc)
  (cond ((null? as)
         (if (non-null? bs)
             (error '(zip-non-tail-recursive: second list longer))
             acc))
        ((null? bs)
         (error '(zip-tail-recursive: first list longer)))
        (#t
         (zip-reversed-tail-recursive-helper (cdr as)
                                             (cdr bs)
                                             (cons (cons (car as)
                                                         (car bs))
                                                   acc)))))
(define-constant (zip-reversed-tail-recursive as bs)
  (zip-reversed-tail-recursive-helper as bs ()))

(define-constant (zip-reversed as bs)
  (zip-reversed-iterative as bs))




;;;; zip.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (zip-iterative as bs)
  (reverse! (zip-reversed-iterative as bs)))

(define-constant (zip-tail-recursive as bs)
  (reverse!-tail-recursive (zip-reversed-tail-recursive as bs)))

(define-constant (zip-non-tail-recursive as bs)
  (cond ((null? as)
         (if (non-null? bs)
             (error '(zip-non-tail-recursive: second list longer))
             ()))
        ((null? bs)
         (error '(zip-non-tail-recursive: first list longer)))
        (#t
         (cons (cons (car as) (car bs))
               (zip-non-tail-recursive (cdr as) (cdr bs))))))

(define-constant (zip as bs)
  (zip-iterative as bs))




;;;; unzip-reversed.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (unzip-reversed-iterative as)
  (let* ((cars ())
         (cdrs ()))
    (while (non-null? as)
      (set! cars (cons (caar as) cars))
      (set! cdrs (cons (cdar as) cdrs))
      (set! as (cdr as)))
    (cons cars cdrs)))

(define-constant (unzip-reversed-tail-recursive-helper as acc-cars acc-cdrs)
  (if (null? as)
      (cons acc-cars acc-cdrs)
      (unzip-reversed-tail-recursive-helper (cdr as)
                                            (cons (caar as) acc-cars)
                                            (cons (cdar as) acc-cdrs))))
(define-constant (unzip-reversed-tail-recursive as)
  (unzip-reversed-tail-recursive-helper as () ()))

(define-constant (unzip-reversed as)
  (unzip-reversed-iterative as))




;;;; unzip.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (unzip-iterative as)
  (let* ((cons-reversed (unzip-reversed-iterative as)))
    (cons (reverse! (car cons-reversed))
          (reverse! (cdr cons-reversed)))))

(define-constant (unzip-tail-recursive as)
  (let* ((cons-reversed (unzip-reversed-tail-recursive as)))
    (cons (reverse! (car cons-reversed))
          (reverse! (cdr cons-reversed)))))

(define-constant (unzip-non-tail-recursive as)
  (if (null? as)
      '(() . ())
      (let* ((unzipped-cdr (unzip-non-tail-recursive (cdr as))))
        (cons (cons (caar as) (car unzipped-cdr))
              (cons (cdar as) (cdr unzipped-cdr))))))

(define-constant (unzip as)
  (unzip-iterative as))




;;;; map!.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (map!-iterative f xs)
  (let* ((res xs))
    (while (non-null? xs)
      (set-car! xs (f (car xs)))
      (set! xs (cdr xs)))
    res))

(define-constant (map!-tail-recursive-helper f xs)
  (if (null? xs)
      'done
      (begin
        (set-car! xs (f (car xs)))
        (map!-tail-recursive-helper f (cdr xs)))))
(define-constant (map!-tail-recursive f xs)
  (map!-tail-recursive-helper f xs)
  xs)

(define-constant (map! f xs)
  (map!-iterative f xs))




;;;; map-reversed.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (map-reversed-iterative f xs)
  (let* ((res '()))
    (while (non-null? xs)
      (set! res (cons (f (car xs)) res))
      (set! xs (cdr xs)))
    res))

(define-constant (map-reversed-tail-recursive-helper f xs acc)
  (if (null? xs)
      acc
      (map-reversed-tail-recursive-helper f
                                          (cdr xs)
                                          (cons (f (car xs))
                                                acc))))
(define-constant (map-reversed-tail-recursive f xs)
  (map-reversed-tail-recursive-helper f xs '()))

(define-constant (map-reversed f xs)
  (map-reversed-iterative f xs))




;;;; map.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (map-iterative f xs)
  (reverse!-iterative (map-reversed-iterative f xs)))

(define-constant (map-non-tail-recursive f xs)
  (if (null? xs)
      '()
      (cons (f (car xs))
            (map-non-tail-recursive f (cdr xs)))))

(define-constant (map-tail-recursive f xs)
  (reverse!-tail-recursive (map-reversed-tail-recursive f xs)))

(define-constant (map f xs)
  (map-iterative f xs))




;;;; fold-left.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (fold-left-iterative f x xs)
  (while (non-null? xs)
    (set! x (f x (car xs)))
    (set! xs (cdr xs)))
  x)

(define-constant (fold-left-iterative-reversed-f reversed-f x xs)
  (while (non-null? xs)
    (set! x (reversed-f (car xs) x))
    (set! xs (cdr xs)))
  x)

(define-constant (fold-left-tail-recursive f x xs)
  (if (null? xs)
      x
      (fold-left-tail-recursive f
                                (f x (car xs))
                                (cdr xs))))

(define-constant (fold-left-tail-recursive-reversed-f reversed-f x xs)
  (if (null? xs)
      x
      (fold-left-tail-recursive-reversed-f reversed-f
                                           (reversed-f (car xs) x)
                                           (cdr xs))))

(define-constant (fold-left f x xs)
  (fold-left-iterative f x xs))




;;;; fold-right.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (fold-right-iterative f xs y)
  (fold-left-iterative-reversed-f f
                                  y
                                  (reverse-iterative xs)))

(define-constant (fold-right-tail-recursive f xs y)
  (fold-left-tail-recursive-reversed-f f
                                       y
                                       (reverse-tail-recursive xs)))

(define-constant (fold-right-non-tail-recursive f xs y)
  (if (null? xs)
      y
      (f (car xs)
         (fold-right-non-tail-recursive f (cdr xs) y))))

(define-constant (fold-right f xs y)
  (fold-right-iterative f xs y))




;;;; fold-right!.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (fold-right!-iterative f xs y)
  (fold-left-iterative-reversed-f f
                                  y
                                  (reverse!-iterative xs)))

(define-constant (fold-right! f xs y)
  (fold-right!-iterative f xs y))




;;;; exists?.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: a return form would also be nice here.
(define-constant (exists?-iterative p xs)
  (let* ((res #f)
         (go-on #t))
    (while go-on
      (cond ((null? xs)
             (set! go-on #f))
            ((p (car xs))
             (begin
               (set! res #t)
               (set! go-on #f)))
            (#t
             (set! xs (cdr xs)))))
    res))

(define-constant (exists?-tail-recursive p xs)
  (cond ((null? xs)
         #f)
        ((p (car xs))
         #t)
        (#t
         (exists?-tail-recursive p (cdr xs)))))

(define-constant (exists? p xs)
  (exists?-iterative p xs))




;;;; for-all?.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: a return form would be nice here.
(define-constant (for-all?-iterative p xs)
  (let* ((res #t)
         (go-on #t))
    (while go-on
      (cond ((null? xs)
             (set! go-on #f))
            ((not (p (car xs)))
             (begin
               (set! res #f)
               (set! go-on #f)))
            (#t
             (set! xs (cdr xs)))))
    res))

(define-constant (for-all?-tail-recursive p xs)
  (cond ((null? xs)
         #t)
        ((p (car xs))
         (for-all?-tail-recursive p (cdr xs)))
        (#t
         #f)))

(define-constant (for-all? p xs)
  (for-all?-iterative p xs))


;;;; filter, filter-reversed.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (filter-reversed-iterative p xs)
  (let* ((res '()))
    (while (non-null? xs)
      (if (p (car xs))
          (set! res (cons (car xs) res))
          'do-nothing)
      (set! xs (cdr xs)))
    res))

(define-constant (filter-iterative p xs)
  (reverse!-iterative (filter-reversed-iterative p xs)))

(define-constant (filter-non-tail-recursive p xs)
  (cond ((null? xs)
         '())
        ((p (car xs))
         (cons (car xs) (filter-non-tail-recursive p (cdr xs))))
        (#t
         (filter-non-tail-recursive p (cdr xs)))))

(define-constant (filter-reversed-tail-recursive-helper p xs acc)
  (cond ((null? xs)
         acc)
        ((p (car xs))
         (filter-reversed-tail-recursive-helper p (cdr xs) (cons (car xs) acc)))
        (#t
         (filter-reversed-tail-recursive-helper p (cdr xs) acc))))
(define-constant (filter-reversed-tail-recursive p xs)
  (filter-reversed-tail-recursive-helper p xs '()))

(define-constant (filter-tail-recursive p xs)
  (reverse!-tail-recursive (filter-reversed-tail-recursive p xs)))

(define-constant (filter-reversed p xs)
  (filter-reversed-tail-recursive p xs))

(define-constant (filter p xs)
  (filter-tail-recursive p xs))





;;;; range-reversed.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (range-reversed-iterative a b)
  (let* ((res '()))
    (while (<= a b)
      (set! res (cons a res))
      (set! a (1+ a)))
    res))

(define-constant (range-reversed-non-tail-recursive a b)
  (if (> a b)
      '()
      (cons b (range-reversed-non-tail-recursive a (1- b)))))

(define-constant (range-reversed-tail-recursive-helper a b acc)
  (if (> a b)
      acc
      (range-reversed-tail-recursive-helper (1+ a) b (cons a acc))))
(define-constant (range-reversed-tail-recursive a b)
  (range-reversed-tail-recursive-helper a b '()))

(define-constant (range-reversed a b)
  (range-reversed-iterative a b))




;;;; range.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (range-iterative a b)
  (let* ((res '()))
    (while (<= a b)
      (set! res (cons b res))
      (set! b (1- b)))
    res))

(define-constant (range-non-tail-recursive a b)
  (if (> a b)
      '()
      (cons a (range-non-tail-recursive (1+ a) b))))

(define-constant (range-tail-recursive-helper a b acc)
  (if (> a b)
      acc
      (range-tail-recursive-helper a (1- b) (cons b acc))))
(define-constant (range-tail-recursive a b)
  (range-tail-recursive-helper a b '()))

(define-constant (range a b)
  (range-iterative a b))




;;;; iota.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (iota-iterative n)
  (let* ((res '()))
    (while (> n 0)
      (set! n (1- n))
      (set! res (cons n res)))
    res))

(define-constant (iota-tail-recursive-helper n acc)
  (if (< n 0)
      acc
      (iota-tail-recursive-helper (1- n) (cons n acc))))
(define-constant (iota-tail-recursive n)
  (iota-tail-recursive-helper (1- n) '()))

(define-constant (iota-non-tail-recursive n)
  (range-non-tail-recursive 0 (1- n)))

(define-constant (iota n)
  (iota-iterative n))




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Sorting.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; insert.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: a return or break form would be nice here.
(define-constant (insert-iterative x xs)
  (let* ((smaller-elements-reversed '())
         (go-on #t))
    (while go-on
      (cond ((null? xs)
             (set! go-on #f))
            ((<= x (car xs))
             (set! go-on #f))
            (#t
             (begin
               (set! smaller-elements-reversed
                     (cons (car xs) smaller-elements-reversed))
               (set! xs (cdr xs))))))
    (append-reversed-iterative smaller-elements-reversed
                               (cons x xs))))

(define-constant (insert-tail-recursive-helper x xs smaller-elements-reversed)
  (cond ((null? xs)
         (append-reversed-tail-recursive smaller-elements-reversed
                                         (singleton x)))
        ((<= x (car xs))
         (append-reversed-tail-recursive smaller-elements-reversed
                                         (cons x xs)))
        (#t
         (insert-tail-recursive-helper x
                                       (cdr xs)
                                       (cons (car xs)
                                             smaller-elements-reversed)))))
(define-constant (insert-tail-recursive x xs)
  (insert-tail-recursive-helper x xs '()))

(define-constant (insert-non-tail-recursive x xs)
  (cond ((null? xs)
         (singleton x))
        ((<= x (car xs))
         (cons x xs))
        (#t
         (cons (car xs)
               (insert-non-tail-recursive x (cdr xs))))))

(define-constant (insert x xs)
  (insert-iterative x xs))




;;;; insert!.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Destructively turn (A . B) into (x . (A . B)).
(define-constant (insert-as-first! x a-cons)
  (let* ((new-cons (cons (car a-cons) (cdr a-cons))))
    (set-car! a-cons x)
    (set-cdr! a-cons new-cons)))

;;; Destructively turn (A . B) into (A . (x . B)).
(define-constant (insert-as-second! x a-cons)
  (let* ((new-cons (cons x (cdr a-cons))))
    (set-cdr! a-cons new-cons)))

(define-constant (insert!-iterative-non-null x xs)
  (let* ((go-on #t))
    (while go-on
      ;; Here I can still assume that xs is not ().
      (cond ((< x (car xs))
             (begin
               (insert-as-first! x xs)
               (set! go-on #f)))
            ((null? (cdr xs))
             (begin
               (insert-as-second! x xs)
               (set! go-on #f)))
            (#t
             (set! xs (cdr xs)))))))
(define-constant (insert!-iterative x xs)
  (if (null? xs)
      (singleton x)
      (begin
        (insert!-iterative-non-null x xs)
        xs)))

(define-constant (insert!-tail-recursive-non-null x xs)
  (cond ((< x (car xs))
         (insert-as-first! x xs))
        ((null? (cdr xs))
         (insert-as-second! x xs))
        (#t
         (insert!-tail-recursive-non-null x (cdr xs)))))
(define-constant (insert!-tail-recursive x xs)
  (if (null? xs)
      (singleton x)
      (begin
        (insert!-tail-recursive-non-null x xs)
        xs)))

(define-constant (insert! x xs)
  (insert!-iterative x xs))




;;;; insertion-sort.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (insertion-sort-iterative xs)
  (let* ((res '()))
    (while (non-null? xs)
      (set! res (insert!-iterative (car xs) res))
      (set! xs (cdr xs)))
    res))

(define-constant (insertion-sort-tail-recursive-helper xs acc)
  (if (null? xs)
      acc
      (insertion-sort-tail-recursive-helper (cdr xs)
                                            (insert!-tail-recursive (car xs)
                                                                    acc))))
(define-constant (insertion-sort-tail-recursive xs)
  (insertion-sort-tail-recursive-helper xs '()))

(define-constant (insertion-sort xs)
  (insertion-sort-iterative xs))




;;;; sort.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (sort xs)
  (insertion-sort xs))




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Higher order.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; identity.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (identity x)
  x)




;;;; compose.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (compose-procedure f g)
  (lambda (x) (f (g x))))

(define-constant (compose-eta f g x)
  (f (g x)))

(define-constant (square-function f)
  (lambda (x) (f (f x))))

(define-constant (square-function-eta f x)
  (f (f x)))




;;;; iterate.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (iterate-iterative-post f n)
  (lambda (x)
    (let* ((n n))
      (while (> n 0)
        (set! x (f x))
        (set! n (1- n)))
      x)))

(define-constant (iterate-iterative-pre f n)
  (let* ((res identity))
    (while (> n 0)
      (set! res (compose-procedure res f))
      (set! n (1- n)))
    res))

(define-constant (iterate-eta f n x)
  (if (zero? n)
      x
      (iterate-eta f (1- n) (f x))))
(define-constant (iterate-tail-recursive-post f n)
  (lambda (x) (iterate-eta f n x)))

;; This builds a function which is even faster than the iterative versions (at
;; least on the naïf JitterLisp interpreter).
(define-constant (iterate-squaring-pre f n)
  ;; This uses the same idea of exponentiation by squaring; the advantage here
  ;; is the very small number of built closures, only O(lg n).  Recursion depth
  ;; is also logarithmic, so non-tail calls are not a problem here.
  (cond ((zero? n)
         identity)
        ((= n 1)
         ;; An important case to optimize, in order to avoid compositions
         ;; between f and the identity, which would be executed many times
         ;; when the iterated function is eventually called.
         f)
        ((even? n)
         (let* ((f^n/2 (iterate-squaring-pre f (quotient n 2))))
           (square-function f^n/2)))
        (#t
         (compose-procedure f (iterate-squaring-pre f (1- n))))))

(define-constant (iterate-squaring-eta f n x)
  ;; This uses the same idea of exponentiation by squaring; the advantage here
  ;; is the very small number of built closures, only O(lg n).
  (cond ((zero? n)
         x)
        ((even? n)
         (iterate-squaring-eta (square-function f) (quotient n 2) x))
        (#t
         (iterate-squaring-eta (square-function f) (quotient n 2) (f x)))))
(define-constant (iterate-squaring-post f n)
  (lambda (x) (iterate-squaring-eta f n x)))

(define-constant (iterate-tail-recursive-pre-helper f n acc)
  (if (zero? n)
      acc
      (iterate-tail-recursive-pre-helper f (1- n) (compose-procedure acc f))))
(define-constant (iterate-tail-recursive-pre f n)
  (iterate-tail-recursive-pre-helper f n identity))

(define-constant (iterate f n)
  (iterate-squaring-pre f n))




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Stuff to move.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The code below is okay, but should be moved up.


;;;; High-level syntax: one-way conditionals.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (when condition . body-forms)
  `(if ,condition
       (begin ,@body-forms)
       (begin)))

(define-macro (unless condition . body-forms)
  `(if ,condition
       (begin)
       (begin ,@body-forms)))




;;;; Variadic boolean connectives.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (or . clauses)
  (cond ((null? clauses)
         '#f)
        ((non-cons? clauses)
         (error '(or: non-list arguments)))
        ((null? (cdr clauses))
         (car clauses))
        (#t
         `(if ,(car clauses)
              '#t
              (or ,@(cdr clauses))))))

(define-macro (and . clauses)
  (cond ((null? clauses)
         '#t)
        ((non-cons? clauses)
         (error '(and: non-list arguments)))
        ((null? (cdr clauses))
         (car clauses))
        (#t
         `(if ,(car clauses)
              (and ,@(cdr clauses))
              '#f))))




;;;; High-level syntax: let.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Rewrite something like
;;;   (let ((a 1) (b 2) foo))
;;; into something like
;;;   (let* ((fresh-1 1) (fresh-2 2) (a fresh-1) (b fresh-2)) foo)
;;; .  The redundant bindings will be optimized away by AST rewriting.
(define-macro (let bindings . body-forms)
  (let* ((fresh-variables (map (lambda (irrelevant) (gensym))
                               bindings))
         (fresh-variable-bindings (zip fresh-variables
                                       (map cdr bindings)))
         (user-variable-bindings (zip (map car bindings)
                                      (map singleton fresh-variables))))
    `(let* ,(append fresh-variable-bindings
                    user-variable-bindings)
       ,@body-forms)))

;;; Optimization [FIXME: remove this when I implement AST rewriting]:
;;; Translate a one-binding let into a one-binding let*.  There's no need for a
;;; special case for a zero-binding let.
(define-constant unoptimized-let let)
(define-macro (let bindings . body-forms)
  (if (= (length bindings) 1)
      `(let* ,bindings ,@body-forms)
      `(unoptimized-let ,bindings ,@body-forms)))




;;;; High-level syntax: letrec.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (letrec bindings . body-forms)
  `(let* ,(map (lambda (binding) `(,(car binding) (undefined)))
               bindings)
     ,@(map (lambda (binding) `(set! ,(car binding) ,@(cdr binding)))
            bindings)
     ,@body-forms))




;;;; High-level syntax: named let.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (named-let loop-name bindings . body-forms)
  `(letrec ((,loop-name (lambda ,(map car bindings) ,@body-forms)))
     (,loop-name ,@(map (lambda (binding) `(begin ,@(cdr binding)))
                        bindings))))




;;;; High-level syntax: Scheme-style let, either named or not.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Redefine the let form to expand to either the previously defined let form or
;;; named-let, according to the shape of the first argument.  In order to do
;;; this we first have to save the previous let macro, to be reused in one of
;;; the two cases.
(define-constant previous-let
  let)
(define-macro (let first-argument . other-arguments)
  (if (symbol? first-argument)
      `(named-let ,first-argument ,@other-arguments)
      `(previous-let ,first-argument ,@other-arguments)))




;;;; Variadic procedure composition.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Variadic procedure composition.
(define-macro (compose . args)
  (cond ((null? args)
         'identity)
        ((null? (cdr args))
         (car args))
        (#t
         `(compose-procedure ,(car args)
                             (compose ,@(cdr args))))))

;;; Sometimes it is convenient to write variadic-composition procedures
;;; in the order they are executed.  This is equivalent to compose with
;;; its arguments in the opposite order.
;;; The arguments of this macro are still evaluated left-to-right, in
;;; the order they are written in the call.
(define-macro (compose-pipeline . args)
  (let ((procedure-names (map (lambda (useless) (gensym)) args)))
    `(let* ,(zip procedure-names
                 (map singleton args))
       (compose ,@(reverse procedure-names)))))




;;;; Variadic list operations.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (improper-list final-element . other-elements)
  (if (null? other-elements)
      final-element
      `(cons ,(car other-elements)
             (improper-list ,final-element ,@(cdr other-elements)))))

(define-macro (list . elements)
  `(improper-list () ,@elements))

(define-macro (circular-list first-element . other-elements)
  (let* ((res-name (gensym))
         (cdr-name (gensym)))
    `(let* ((,res-name (cons ,first-element #f))
            (,cdr-name (improper-list ,res-name ,@other-elements)))
       (set-cdr! ,res-name ,cdr-name)
       ,res-name)))






;;;; High-level syntax: case form.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: use let-macro
(define-macro (case-variable-matches? variable . literals)
  `(or ,@(map (lambda (a-literal)
                `(eq? ,variable ,a-literal))
              literals)))

;; FIXME: use let-macro
(define-macro (case-variable variable . clauses)
  (if (null? clauses)
      '(begin)
      `(if (case-variable-matches? ,variable ,@(caar clauses))
           (begin ,@(cdar clauses))
           (case-variable ,variable ,@(cdr clauses)))))

(define-macro (case discriminand . clauses)
  (let ((discriminand-variable (gensym)))
    `(let ((,discriminand-variable ,discriminand))
       (case-variable ,discriminand-variable ,@clauses))))




;;;; High-level syntax: sequencing forms.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (begin1 first-form . more-forms)
  (let ((result-variable (gensym)))
    `(let ((,result-variable ,first-form))
       ,@more-forms
       ,result-variable)))

(define-macro (begin2 first-form second-form . more-forms)
  (let ((result-variable (gensym)))
    `(begin
       ,first-form
       (let ((,result-variable ,second-form))
         ,@more-forms
         ,result-variable))))




;;;; High-level syntax: looping forms.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (dotimes (variable limit . result-forms) . body-forms)
  (let ((limit-name (gensym)))
    `(let* ((,limit-name ,limit)
            (,variable 0))
       (while (< ,variable ,limit-name)
         ,@body-forms
         (set! ,variable (1+ ,variable)))
       ,@result-forms)))

(define-macro (dotimesdown (variable times . result-forms) . body-forms)
  (let ((times-name (gensym)))
    `(let ((,variable ,times))
       (while (> ,variable 0)
         (set! ,variable (1- ,variable))
         ,@body-forms)
       ,@result-forms)))

(define-macro (dolist (variable list . result-forms) . body-forms)
  (let ((list-name (gensym)))
    `(let* ((,list-name ,list)
            ;; This is currently faster than binding ,variable inside the loop.
            (,variable (undefined)))
       (while (non-null? ,list-name)
         (set! ,variable (car ,list-name))
         ,@body-forms
         (set! ,list-name (cdr ,list-name)))
       ,@result-forms)))

(define-macro (do bindings (end-condition . result-forms) . body-forms)
  `(let (,@(map (lambda (binding)
                  `(,(car binding) ,(cadr binding)))
               bindings))
     (while (not ,end-condition)
       ,@body-forms
       ,@(flatten (map (lambda (binding)
                         (if (null? (cddr binding))
                             '()
                             `((set! ,(car binding) ,@(cddr binding)))))
                       bindings)))
     ,@result-forms))




;;;; Variadic arithmetic.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: nested quasiquoting is already difficult to handle here.  I can do the
;; same thing more simply with higher-order procedures, using simple macro
;; wrappers on top of fold-left and fold-right.
(define-macro (define-right-nested-variadic-extension operator original-name
                neutral)
  (let ((operands-name (gensym)))
    `(define-macro (,operator . ,operands-name)
       (cond ((null? ,operands-name)
              ,neutral)
             ((null? (cdr ,operands-name))
              (car ,operands-name))
             (#t
              `(,',original-name
                ,(car ,operands-name)
                (,',operator ,@(cdr ,operands-name))))))))

(define-macro (define-associative-variadic-extension operator
                original-name neutral)
  `(define-right-nested-variadic-extension ,operator ,original-name ,neutral))

(define-associative-variadic-extension + primordial-+ 0)
(define-associative-variadic-extension * primordial-* 1)

(define-macro (- . operands)
  (cond ((null? operands)
         (error '(-: no arguments)))
        ((null? (cdr operands))
         `(negate ,@operands))
        (#t
         `(primordial-- ,(car operands)
                        (+ ,@(cdr operands))))))

(define-macro (/ . operands)
  (cond ((null? operands)
         (error '(/: no arguments)))
        ((null? (cdr operands))
         `(primordial-/ 1 ,@operands))
        (#t
         `(primordial-/ ,(car operands)
                        (* ,@(cdr operands))))))




;;;; Making non-variadic lambdas from possibly variadic operators.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is a convenient way to syntactically generate a lambda from a macro
;;; name.
;;; Example:
;;;   (lambda-wrapper 2 +)
;;;   expands to something equivalent to
;;;   (lambda (a b) (+ a b)) .
(define-macro (lambda-wrapper arity rator)
  (let ((formals (map (lambda (_) (gensym))
                      (iota arity))))
    `(lambda ,formals
       (,rator ,@formals))))




;;;; Variadic list operations.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant previous-append append)

;; FIXME: rename the append operator above in quasiquoting functions before
;; renaming this to append.
;; Calls in code which is already in AST form will be affected otherwise.
(define-right-nested-variadic-extension variadic-append previous-append ())




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Sets as lists.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Sets implemented as unordered lists without duplicates, elements compared
;;; with eq? .

(define-constant set-empty
  ())

(define-constant (set-empty? xs)
  (null? xs))

(define-constant (set-has? xs x)
  (and (non-null? xs)
       (or (eq? (car xs) x)
           (set-has? (cdr xs) x))))

(define-constant (set-without-helper xs x reversed-left-part)
  (cond ((null? xs)
         reversed-left-part)
        ((eq? (car xs) x)
         (append! reversed-left-part (cdr xs)))
        (#t
         (set-without-helper (cdr xs) x (cons (car xs) reversed-left-part)))))
(define-constant (set-without xs x)
  (set-without-helper xs x ()))

(define-constant (set-with xs x)
  (cons x (set-without xs x)))

(define-constant (set-singleton x)
  (singleton x))

(define-constant (set-unite-procedure xs ys)
  (if (null? ys)
      xs
      (set-unite-procedure (set-with xs (car ys)) (cdr ys))))

(define-associative-variadic-extension set-unite
  set-unite-procedure set-empty)

(define-constant (set-subtract xs ys)
  (if (null? ys)
      xs
      (set-subtract (set-without xs (car ys)) (cdr ys))))

(define-constant (set-intersect-helper xs ys acc)
  (if (null? ys)
      acc
      (let* ((car-ys (car ys))
             (new-acc (if (set-has? xs car-ys)
                          (cons car-ys acc)
                          acc)))
        (set-intersect-helper xs
                              (cdr ys)
                              new-acc))))
(define-constant (set-intersect-procedure xs ys)
  (set-intersect-helper xs ys ()))

(define-associative-variadic-extension set-intersect
  set-intersect-procedure set-empty)




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; AST optimization.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; Tentative: free variables in an AST.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return a set-as-list of the variables occurring free in the given AST.
(define-constant (ast-free ast)
  (cond ((ast-literal? ast)
         set-empty)
        ((ast-variable? ast)
         (set-singleton (ast-variable-name ast)))
        ((ast-define? ast)
         ;; The defined variable is global, and doesn't enter the picture at
         ;; all.
         (ast-free (ast-define-body ast)))
        ((ast-if? ast)
         (set-unite (ast-free (ast-if-condition ast))
                    (ast-free (ast-if-then ast))
                    (ast-free (ast-if-else ast))))
        ((ast-set!? ast)
         ;; An assigned variable *is* a reference in the sense of this
         ;; procedure, differently from a defined global.  Of course the
         ;; modified binding may be global, but the variable is free.
         (set-with (ast-free (ast-set!-body ast))
                   (ast-set!-name ast)))
        ((ast-while? ast)
         (set-unite (ast-free (ast-while-guard ast))
                    (ast-free (ast-while-body ast))))
        ((ast-primitive? ast)
         (ast-free-list (ast-primitive-operands ast)))
        ((ast-call? ast)
         (set-unite (ast-free (ast-call-operator ast))
                    (ast-free-list (ast-call-operands ast))))
        ((ast-lambda? ast)
         (set-subtract (ast-free (ast-lambda-body ast))
                       (ast-lambda-formals ast)))
        ((ast-let? ast)
         (set-unite (ast-free (ast-let-bound-form ast))
                    (set-without (ast-free (ast-let-body ast))
                                 (ast-let-bound-name ast))))
        ((ast-sequence? ast)
         (set-unite (ast-free (ast-sequence-first ast))
                    (ast-free (ast-sequence-second ast))))))

;;; Return a set-as-list of the free variables in the given list of ASTs.
(define-constant (ast-free-list asts)
  (if (null? asts)
      set-empty
      (set-unite (ast-free (car asts))
                 (ast-free-list (cdr asts)))))




;;;; Free variables in an AST.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return non-#f iff the given variable occurs free in the given AST.
(define-constant (ast-has-free? ast x)
  (cond ((ast-literal? ast)
         #f)
        ((ast-variable? ast)
         (eq? (ast-variable-name ast) x))
        ((ast-define? ast)
         ;; The defined variable is global, and doesn't enter the picture.
         (ast-has-free? (ast-define-body ast) x))
        ((ast-if? ast)
         (or (ast-has-free? (ast-if-condition ast) x)
             (ast-has-free? (ast-if-then ast) x)
             (ast-has-free? (ast-if-else ast) x)))
        ((ast-set!? ast)
         ;; An assigned variable *is* a reference in the sense of this
         ;; procedure, differently from a defined global.  Of course the
         ;; modified binding may be global, but the variable is free.
         (or (eq? (ast-set!-name ast) x)
             (ast-has-free? (ast-set!-body ast) x)))
        ((ast-while? ast)
         (or (ast-has-free? (ast-while-guard ast) x)
             (ast-has-free? (ast-while-body ast) x)))
        ((ast-primitive? ast)
         (ast-has-free?-list (ast-primitive-operands ast) x))
        ((ast-call? ast)
         (or (ast-has-free? (ast-call-operator ast) x)
             (ast-has-free?-list (ast-call-operands ast) x)))
        ((ast-lambda? ast)
         (if (set-has? (ast-lambda-formals ast) x)
             #f
             (ast-has-free? (ast-lambda-body ast) x)))
        ((ast-let? ast)
         (if (eq? (ast-let-bound-name ast) x)
             (ast-has-free? (ast-let-bound-form ast) x)
             (or (ast-has-free? (ast-let-bound-form ast) x)
                 (ast-has-free? (ast-let-body ast) x))))
        ((ast-sequence? ast)
         (or (ast-has-free? (ast-sequence-first ast) x)
             (ast-has-free? (ast-sequence-second ast) x)))))

;;; An extension of ast-has-free? to a list of ASTs.
(define-constant (ast-has-free?-list asts x)
  (and (non-null? asts)
       (or (ast-has-free? (car asts) x)
           (ast-has-free?-list (cdr asts) x))))




;;;; Tentative: assigned variables in an AST.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A set! form counts as an assignment; a define form doesn't, as a
;;; define'd variable in JitterLisp is always a global, and globals
;;; can't be renamed.

;;; Return a set-as-list of the free variables being set! in the given AST.
(define-constant (ast-assigned ast)
  (cond ((ast-literal? ast)
         set-empty)
        ((ast-variable? ast)
         set-empty)
        ((ast-define? ast)
         ;; The defined variable is global: see the comment above.
         (ast-assigned (ast-define-body ast)))
        ((ast-if? ast)
         (set-unite (ast-assigned (ast-if-condition ast))
                    (ast-assigned (ast-if-then ast))
                    (ast-assigned (ast-if-else ast))))
        ((ast-set!? ast)
         (set-with (ast-assigned (ast-set!-body ast))
                   (ast-set!-name ast)))
        ((ast-while? ast)
         (set-unite (ast-assigned (ast-while-guard ast))
                    (ast-assigned (ast-while-body ast))))
        ((ast-primitive? ast)
         (ast-assigned-list (ast-primitive-operands ast)))
        ((ast-call? ast)
         (set-unite (ast-assigned (ast-call-operator ast))
                    (ast-assigned-list (ast-call-operands ast))))
        ((ast-lambda? ast)
         (set-subtract (ast-assigned (ast-lambda-body ast))
                       (ast-lambda-formals ast)))
        ((ast-let? ast)
         (set-unite (ast-assigned (ast-let-bound-form ast))
                    (set-without (ast-assigned (ast-let-body ast))
                                 (ast-let-bound-name ast))))
        ((ast-sequence? ast)
         (set-unite (ast-assigned (ast-sequence-first ast))
                    (ast-assigned (ast-sequence-second ast))))))

;;; Return a set-as-list of the free variables being set! in the given list of
;;; ASTs.
(define-constant (ast-assigned-list asts)
  (if (null? asts)
      set-empty
      (set-unite (ast-assigned (car asts))
                 (ast-assigned-list (cdr asts)))))




;;;; Assigned variables in an AST.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A set! form counts as an assignment; a define form doesn't, as a
;;; define'd variable in JitterLisp is always a global, and globals
;;; can't be renamed.

;;; Return non-#f iff the a free occurrence of the given variable is set! in the
;;; given AST.
(define-constant (ast-has-assigned? ast x)
  (cond ((ast-literal? ast)
         #f)
        ((ast-variable? ast)
         #f)
        ((ast-define? ast)
         ;; The defined variable is global: see the comment above.
         (ast-has-assigned? (ast-define-body ast) x))
        ((ast-if? ast)
         (or (ast-has-assigned? (ast-if-condition ast) x)
             (ast-has-assigned? (ast-if-then ast) x)
             (ast-has-assigned? (ast-if-else ast) x)))
        ((ast-set!? ast)
         (or (eq? (ast-set!-name ast) x)
             (ast-has-assigned? (ast-set!-body ast) x)))
        ((ast-while? ast)
         (or (ast-has-assigned? (ast-while-guard ast) x)
             (ast-has-assigned? (ast-while-body ast) x)))
        ((ast-primitive? ast)
         (ast-has-assigned?-list (ast-primitive-operands ast) x))
        ((ast-call? ast)
         (or (ast-has-assigned? (ast-call-operator ast) x)
             (ast-has-assigned?-list (ast-call-operands ast) x)))
        ((ast-lambda? ast)
         (if (set-has? (ast-lambda-formals ast) x)
             #f
             (ast-has-assigned? (ast-lambda-body ast) x)))
        ((ast-let? ast)
         (if (eq? (ast-let-bound-name ast) x)
             (ast-has-assigned? (ast-let-bound-form ast) x)
             (or (ast-has-assigned? (ast-let-bound-form ast) x)
                 (ast-has-assigned? (ast-let-body ast) x))))
        ((ast-sequence? ast)
         (or (ast-has-assigned? (ast-sequence-first ast) x)
             (ast-has-assigned? (ast-sequence-second ast) x)))))

;;; An extension of ast-has-assigned? to a list of ASTs.
(define-constant (ast-has-assigned?-list asts x)
  (and (non-null? asts)
       (or (ast-has-assigned? (car asts) x)
           (ast-has-assigned?-list (cdr asts) x))))




;;;; AST alpha-conversion.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return an AST equal to the given one, except that every bound variable
;;; has been consistently renamed to a fresh identifier.
(define-constant (ast-alpha-convert ast)
  (ast-alpha-convert-with ast ()))

;;; A helper procedure for ast-alpha-convert, keeping track of which free
;;; variable variable is to be replaced with which new variable.  Recursive
;;; calls will build new alists extending the given one, with a fresh new
;;; variable associated to each inner bound variable.
(define-constant (ast-alpha-convert-with ast alist)
  (cond ((ast-literal? ast)
         ast)
        ((ast-variable? ast)
         (let ((pair (assq (ast-variable-name ast) alist)))
           (if pair
               (ast-variable (cdr pair))
               ast)))
        ((ast-define? ast)
         ;; Do not rename globally bound variables.
         (ast-define (ast-define-name ast)
                     (ast-alpha-convert-with (ast-define-body ast) alist)))
        ((ast-if? ast)
         (ast-if (ast-alpha-convert-with (ast-if-condition ast) alist)
                 (ast-alpha-convert-with (ast-if-then ast) alist)
                 (ast-alpha-convert-with (ast-if-else ast) alist)))
        ((ast-set!? ast)
         (let* ((old-name (ast-set!-name ast))
                (pair (assq old-name alist))
                (new-name (if pair (cdr pair) old-name)))
           (ast-set! new-name
                     (ast-alpha-convert-with (ast-set!-body ast) alist))))
        ((ast-while? ast)
         (ast-while (ast-alpha-convert-with (ast-while-guard ast) alist)
                    (ast-alpha-convert-with (ast-while-body ast) alist)))
        ((ast-primitive? ast)
         (ast-primitive (ast-primitive-operator ast)
                        (ast-alpha-convert-with-list
                            (ast-primitive-operands ast)
                            alist)))
        ((ast-call? ast)
         (ast-call (ast-alpha-convert-with (ast-call-operator ast) alist)
                   (ast-alpha-convert-with-list (ast-call-operands ast)
                                                alist)))
        ((ast-lambda? ast)
         (let* ((old-formals (ast-lambda-formals ast))
                (new-formals (map (lambda (useless) (gensym)) old-formals))
                (new-bindings (zip-reversed old-formals new-formals))
                (new-alist (append! new-bindings alist)))
           (ast-lambda new-formals
                       (ast-alpha-convert-with (ast-lambda-body ast)
                                               new-alist))))
        ((ast-let? ast)
         (let* ((old-bound-name (ast-let-bound-name ast))
                (new-bound-name (gensym))
                (new-alist (cons (cons old-bound-name new-bound-name) alist)))
           (ast-let new-bound-name
                    (ast-alpha-convert-with (ast-let-bound-form ast)
                                            alist) ;; Important: not new-alist
                    (ast-alpha-convert-with (ast-let-body ast)
                                            new-alist))))
        ((ast-sequence? ast)
         (ast-sequence (ast-alpha-convert-with (ast-sequence-first ast)
                                               alist)
                       (ast-alpha-convert-with (ast-sequence-second ast)
                                               alist)))))

;;; An extension of ast-alpha-convert-with to a list of ASTs: return the list
;;; of rewriten ASTs in order.
(define-constant (ast-alpha-convert-with-list asts alist)
  (map (lambda (ast) (ast-alpha-convert-with ast alist))
       asts))




;;;; Global constant folding.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return a copy of the given AST where every free occurrence of current global
;;; constants in replaced with its global value as a literal.  The additional
;;; argument is a set-of-list of the currently bound variables; of course bound
;;; variables just happening to share a name with a global constant must not be
;;; replaced.
(define-constant (ast-global-fold ast bounds)
  (cond ((ast-literal? ast)
         ast)
        ((ast-variable? ast)
         (let ((name (ast-variable-name ast)))
           (if (and (not (set-has? bounds name))
                    (defined? name)
                    (constant? name))
               (ast-literal (global-lookup name))
               ast)))
        ((ast-define? ast)
         (ast-define (ast-define-name ast)
                     (ast-global-fold (ast-define-body ast) bounds)))
        ((ast-if? ast)
         (ast-if (ast-global-fold (ast-if-condition ast) bounds)
                 (ast-global-fold (ast-if-then ast) bounds)
                 (ast-global-fold (ast-if-else ast) bounds)))
        ((ast-set!? ast)
         (ast-set! (ast-set!-name ast)
                   (ast-global-fold (ast-set!-body ast) bounds)))
        ((ast-while? ast)
         (ast-while (ast-global-fold (ast-while-guard ast) bounds)
                    (ast-global-fold (ast-while-body ast) bounds)))
        ((ast-primitive? ast)
         (ast-primitive (ast-primitive-operator ast)
                        (ast-global-fold-list (ast-primitive-operands ast)
                                              bounds)))
        ((ast-call? ast)
         (ast-call (ast-global-fold (ast-call-operator ast) bounds)
                   (ast-global-fold-list (ast-call-operands ast)
                                         bounds)))
        ((ast-lambda? ast)
         (let ((formals (ast-lambda-formals ast)))
           (ast-lambda formals
                       (ast-global-fold (ast-lambda-body ast)
                                        (set-unite formals bounds)))))
        ((ast-let? ast)
         (let* ((bound-name (ast-let-bound-name ast))
                (old-bound-form (ast-let-bound-form ast))
                (new-bound-form (ast-global-fold old-bound-form bounds))
                (old-body (ast-let-body ast))
                (new-body (ast-global-fold old-body
                                           (set-with bounds bound-name))))
           (ast-let bound-name
                    new-bound-form
                    new-body)))
        ((ast-sequence? ast)
         (ast-sequence (ast-global-fold (ast-sequence-first ast) bounds)
                       (ast-global-fold (ast-sequence-second ast)
                                        bounds)))))

;;; An extension of ast-global-fold to a list of ASTs: return the list
;;; of rewriten ASTs in order.
(define-constant (ast-global-fold-list asts bounds)
  (map (lambda (ast) (ast-global-fold ast bounds))
       asts))




;;;; Instantiation.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return a copy of the given AST where every free occurrence of the given
;;; variable is replaced with the given AST.  Error out if a free occurrence
;;; of the variable is assigned, in which case instantiation would be invalid.
;;; Of course there are other cases in which instantiation would yield an AST
;;; not equivalent to the original: this is not checked for.
(define-constant (ast-instantiate ast x r)
  (cond ((ast-literal? ast)
         ast)
        ((ast-variable? ast)
         (if (eq? (ast-variable-name ast) x)
             r
             ast))
        ((ast-define? ast)
         ;; Do not rename globally bound variables.
         (ast-define (ast-define-name ast)
                     (ast-instantiate (ast-define-body ast) x r)))
        ((ast-if? ast)
         (ast-if (ast-instantiate (ast-if-condition ast) x r)
                 (ast-instantiate (ast-if-then ast) x r)
                 (ast-instantiate (ast-if-else ast) x r)))
        ((ast-set!? ast)
         (let ((name (ast-set!-name ast)))
           (when (eq? name x)
             (error `(instantiating assigned variable ,x in ,ast)))
           (ast-set! name
                     (ast-instantiate (ast-set!-body ast) x r))))
        ((ast-while? ast)
         (ast-while (ast-instantiate (ast-while-guard ast) x r)
                    (ast-instantiate (ast-while-body ast) x r)))
        ((ast-primitive? ast)
         (ast-primitive (ast-primitive-operator ast)
                        (ast-instantiate-list (ast-primitive-operands ast) x r)))
        ((ast-call? ast)
         (ast-call (ast-instantiate (ast-call-operator ast) x r)
                   (ast-instantiate-list (ast-call-operands ast) x r)))
        ((ast-lambda? ast)
         (let ((formals (ast-lambda-formals ast)))
           (if (set-has? formals x)
               ast ;; x occurs bound: don't touch its uses.
               (ast-lambda formals
                           (ast-instantiate (ast-lambda-body ast) x r)))))
        ((ast-let? ast)
         ;; Always instantiate in the bound form.  Instantiate in the body
         ;; only if the let variable is not the one we are replacing.  Don't
         ;; rename the bound variable.
         (let* ((bound-name (ast-let-bound-name ast))
                (old-bound-form (ast-let-bound-form ast))
                (new-bound-form (ast-instantiate old-bound-form x r))
                (old-body (ast-let-body ast))
                (new-body (if (eq? bound-name x)
                              old-body
                              (ast-instantiate old-body x r))))
           (ast-let bound-name
                    new-bound-form
                    new-body)))
        ((ast-sequence? ast)
         (ast-sequence (ast-instantiate (ast-sequence-first ast) x r)
                       (ast-instantiate (ast-sequence-second ast) x r)))))

;;; An extension of ast-instantiate to a list of ASTs: return the list
;;; of rewriten ASTs in order.
(define-constant (ast-instantiate-list asts x r)
  (map (lambda (ast) (ast-instantiate ast x r))
       asts))




;;;; AST call simplification.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; A call where the operator is a closure literal (usually obtained by a
;;; previous optimization) can be easily rewritten into a let binding each
;;; formal as a local variable and having the lambda body as the let body,
;;; as long as the closure environment is empty.
;;; Some optimizations can probably be performed even with non-empty
;;; closure environments but this is not implemented yet, as the way
;;; environments are represented will change in the future.
;;;
;;; Most of the let forms introduced by this rewrite can be optimized away
;;; in a later pass.

;;; Return a rewritten call having the given closure as the original (literal)
;;; operator, and the ASTs in the given list as operands.
(define-constant (ast-simplify-call-helper closure actuals)
  (let ((environment (closure-environment closure))
        (formals (closure-formals closure))
        (body (closure-body closure)))
    (cond ((non-null? environment)
           ;; We currently don't rewrite if the environment is non-empty.
           (ast-call (ast-literal closure) actuals))
          ((<> (length formals) (length actuals))
           ;; This call will fail if reached: don't rewrite it.
           ;; FIXME: warn in a cleaner way.
           (display `(warning: invalid in-arity for call to ,closure
                               with actuals ,actuals))
           (newline)
           (ast-call (ast-literal closure) actuals))
          (#t
           ;; The environment is empty, and the argument number is correct:
           ;; rewrite into nested lets binding the closure formals to the call
           ;; actuals, and then evaluating the closure body.  alpha-convert the
           ;; closure content to avoid conflicts with local variables in the
           ;; caller.
           (let* ((new-formals (map (lambda (useless) (gensym)) formals))
                  (alist (zip formals new-formals))
                  (new-body (ast-alpha-convert-with body alist)))
             (ast-nested-let new-formals actuals new-body))))))

;;; Given a list of bound variables, a list of actuals and a body, build nested
;;; let ASTs, evaluating the actuals left-to-right.  This is similar to a let*
;;; in Lisp, with a different syntax.  Assume that the formals and the actual
;;; ASTs have the same length.
;;; We can afford simple nested bindings here since procedure formals are
;;; guaranteed to be all different.
(define-constant (ast-nested-let formals actual-asts body-ast)
  (if (null? formals)
      body-ast
      (ast-let (car formals) (car actual-asts)
               (ast-nested-let (cdr formals) (cdr actual-asts)
                               body-ast))))

;;; Return a copy of the given AST where calls to closure literals are
;;; rewritten into let forms where possible.  The AST is assumed to be
;;; already alpha-converted.
(define-constant (ast-simplify-calls ast)
  (cond ((ast-literal? ast)
         ast)
        ((ast-variable? ast)
         ast)
        ((ast-define? ast)
         (ast-define (ast-define-name ast)
                     (ast-simplify-calls (ast-define-body ast))))
        ((ast-if? ast)
         (ast-if (ast-simplify-calls (ast-if-condition ast))
                 (ast-simplify-calls (ast-if-then ast))
                 (ast-simplify-calls (ast-if-else ast))))
        ((ast-set!? ast)
         (ast-set! (ast-set!-name ast)
                   (ast-simplify-calls (ast-set!-body ast))))
        ((ast-while? ast)
         (ast-while (ast-simplify-calls (ast-while-guard ast))
                    (ast-simplify-calls (ast-while-body ast))))
        ((ast-primitive? ast)
         (ast-primitive (ast-primitive-operator ast)
                        (ast-simplify-calls-list (ast-primitive-operands ast))))
        ((ast-call? ast)
         (let (;; Simplifying the operator seems perfunctory if we consider the
               ;; kind of ASTs this procedure is used on.  Anyway it might be
               ;; profitable in the future, with ASTs of a different shape.
               (simplified-operator
                (ast-simplify-calls (ast-call-operator ast)))
               ;; Simplifying the operands is always reasonable, instead: they
               ;; may contain calls.
               (simplified-operands
                (ast-simplify-calls-list (ast-call-operands ast))))
           (if (and (ast-literal? simplified-operator)
                    (closure? (ast-literal-value simplified-operator)))
               (ast-simplify-call-helper (ast-literal-value simplified-operator)
                                         simplified-operands)
               (ast-call simplified-operator simplified-operands))))
        ((ast-lambda? ast)
         (ast-lambda (ast-lambda-formals ast)
                     (ast-simplify-calls (ast-lambda-body ast))))
        ((ast-let? ast)
         (ast-let (ast-let-bound-name ast)
                  (ast-simplify-calls (ast-let-bound-form ast))
                  (ast-simplify-calls (ast-let-body ast))))
        ((ast-sequence? ast)
         (ast-sequence (ast-simplify-calls (ast-sequence-first ast))
                       (ast-simplify-calls (ast-sequence-second ast))))))

;;; An extension of ast-simplify-calls to a list of ASTs: return the list
;;; of rewriten ASTs in order.
(define-constant (ast-simplify-calls-list asts)
  (map ast-simplify-calls asts))




;;;; AST optimization.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Return an equivalent rewritten version of the given AST, which is assumed to
;;; be alpha-converted, knowing that the given set-of-list of variables is
;;; bound.
;;; The most important optimizations are for the let case, which uses the helper
;;; below.
;;(define rewrite-counter 0)
(define-constant (ast-optimize-helper ast bounds)
;;(display bounds) (newline)
;;(display ast) (newline)
;;(display `(,rewrite-counter -th expansion -- bounds are ,(length bounds))) (newline) (set! rewrite-counter (1+ rewrite-counter))
  (cond ((ast-literal? ast)
         ast)
        ((ast-variable? ast)
         ast)
        ((ast-define? ast)
         (ast-define (ast-define-name ast)
                     (ast-optimize-helper (ast-define-body ast) bounds)))
        ((ast-if? ast)
         (let ((optimized-condition
                (ast-optimize-helper (ast-if-condition ast) bounds)))
           (cond ((and (ast-literal? optimized-condition)
                       (ast-literal-value optimized-condition))
                  ;; The condition has been simplified to non-#f.
                  (ast-optimize-helper (ast-if-then ast) bounds))
                 ((ast-literal? optimized-condition)
                  ;; The condition has been simplified to #f, since we didn't
                  ;; get to the previous clause.
                  (ast-optimize-helper (ast-if-else ast) bounds))
                 (#t
                  ;; Keep both branches.
                  (ast-if optimized-condition
                          (ast-optimize-helper (ast-if-then ast) bounds)
                          (ast-optimize-helper (ast-if-else ast) bounds))))))
        ((ast-set!? ast)
         (ast-set! (ast-set!-name ast)
                   (ast-optimize-helper (ast-set!-body ast) bounds)))
        ((ast-while? ast)
         (ast-while (ast-optimize-helper (ast-while-guard ast) bounds)
                    (ast-optimize-helper (ast-while-body ast) bounds)))
        ((ast-primitive? ast)
         ;; FIXME: evaluate the primitive at expansion time if the primitive
         ;; has no effect and all of its arguments are literal.
         (ast-primitive (ast-primitive-operator ast)
                        (ast-optimize-helper-list (ast-primitive-operands ast)
                                                  bounds)))
        ((ast-call? ast)
         (ast-call (ast-optimize-helper (ast-call-operator ast) bounds)
                   (ast-optimize-helper-list (ast-call-operands ast) bounds)))
        ((ast-lambda? ast)
         (let ((formals (ast-lambda-formals ast)))
           (ast-lambda formals
                       (ast-optimize-helper (ast-lambda-body ast)
                                            (set-unite bounds formals)))))
        ((ast-let? ast)
         (let ((bound-name (ast-let-bound-name ast)))
           ;; Notice that the let body is not optimized here: it will be
           ;; optimized by ast-optimize-let, after possibly performing
           ;; replacements into it.  Optimizing the bound form, instead, is
           ;; important: its shape will determine which optimizations are
           ;; possible.
           (ast-optimize-let bound-name
                             (ast-optimize-helper (ast-let-bound-form ast) bounds)
                             (ast-let-body ast)
                             bounds)))
        ((ast-sequence? ast)
         (let ((optimized-first (ast-optimize-helper (ast-sequence-first ast)
                                                     bounds))
               (optimized-second (ast-optimize-helper (ast-sequence-second ast)
                                                      bounds)))
           ;; If fhe first form in the sequence has no effect rewrite to the
           ;; second form only.
           ;; FIXME: generalize to no-effect primitives with no-effect actuals.
           (if (or (ast-literal? optimized-first)
                   (and (ast-variable? optimized-first)
                        (or (set-has? bounds
                                      (ast-variable-name optimized-first))
                            (constant? (ast-variable-name optimized-first)))))
               optimized-second
               (ast-sequence optimized-first
                             optimized-second))))))

;;; An extension of ast-optimize-helper to a list of ASTs: return the list
;;; of rewriten ASTs in order.
(define-constant (ast-optimize-helper-list asts bounds)
  (map (lambda (ast) (ast-optimize-helper ast bounds))
       asts))

;;; A helper for ast-optimize-helper in the let case, which is the most complex.
;;; This assumes that both subforms are alpha-converted.
;;; This procedure should be called with the bound form already optimized,
;;; in order to recognize the bound form shape and simplify the body as far
;;; as possible; however there is no need for the body to be already
;;; simplified -- it would introduce serious inefficiencies since the body
;;; is optimized again here, and doing this recursively would easily explode
;;; to quadratic behavior even assuming set operations to be O(1), which they
;;; are certainly not.
(define-constant (ast-optimize-let bound-name bound-form body bounds)
  (cond ;; FIXME (as the first case): let-sequence-be elimination.
        ((and (ast-literal? bound-form)
              (not (ast-has-assigned? body bound-name)))
         ;; The variable is bound to a literal, without being assigned:
         ;; fold the literal into the body and optimize it further.
         (let ((folded-body (ast-instantiate body bound-name bound-form)))
           (ast-optimize-helper folded-body bounds)))
        ((and (ast-variable? bound-form)
              (not (ast-has-assigned? body bound-name))
              (not (ast-has-assigned? body (ast-variable-name bound-form))))
         ;; The variable is bound to another variable, with neither being
         ;; assigned in the body.  We can reduce the entire let AST to a body
         ;; with the bound variable replaced by the other.  Here we rely on the
         ;; body being alpha-converted to be sure not to capture the substituted
         ;; variable.
         (let ((new-body (ast-instantiate body bound-name bound-form)))
           (ast-optimize-helper new-body bounds)))
        (#t
         ;; Default case: keep the let AST in our rewriting.
         (ast-let bound-name
                  bound-form
                  (ast-optimize-helper body (set-with bounds bound-name))))))
;;;????




;;;; AST optimization driver.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant (ast-optimize ast-0 bounds)
  (let* (;; Fold global constants into the AST.  This will introduce, in
         ;; particular, closure literals as operators.
         (_ (display `(ast-0: ,ast-0)) (newline))
         (ast-1 (ast-global-fold ast-0 bounds))
         (_ (display `(ast-1: ,ast-1)) (newline))
         ;; Alpha-convert everything: we are about to introduce new let
         ;; bindings, and we need to prevent conflicts.
         (ast-2 (ast-alpha-convert ast-1))
         (_ (display `(ast-2: ,ast-2)) (newline))
         ;; Translate calls to closures literals into let forms,
         ;; alpha-converting the inlined procedures.  This should make almost
         ;; all primitives explicit in the AST eliminating closure wrappers for
         ;; primitives, at the cost of introducing many redundant lets.
         ;; ast-3 will still be alpha-converted, with all bound variables
         ;; different from one another.
         (ast-3 (ast-simplify-calls ast-2))
         (_ (display `(ast-3: ,ast-3)) (newline))
         ;; Remove redundancy.
         (ast-4 (ast-optimize-helper ast-3 bounds))
         (_ (display `(ast-4: ,ast-4)) (newline))
         )
    ast-4))




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Tentative features, or experimentation just for fun.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; Streams.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-constant stream-empty
  '(#t . ()))

(define-constant (stream-ready? s)
  (car s))

(define-constant (stream-force! s)
  (unless (stream-ready? s)
    (set-cdr! s ((cdr s)))
    (set-car! s #t))
  (cdr s))

(define-constant (stream-null? s)
  (null? (stream-force! s)))
(define-constant (stream-non-null? s)
  (non-null? (stream-force! s)))
(define-constant (stream-car s)
  (car (stream-force! s)))
(define-constant (stream-cdr s)
  (cdr (stream-force! s)))
(define-constant (stream-set-car! s new-car)
  (set-car! (stream-force! s) new-car))
(define-constant (stream-set-cdr! s new-cdr)
  (set-cdr! (stream-force! s) new-cdr))

(define-macro (stream-cons x s)
  `(cons #f
         (lambda ()
           (cons ,x ,s))))

(define-macro (stream-delay stream-expression)
  `(cons #f
         (lambda ()
           (stream-force! ,stream-expression))))

(define-constant (stream-forever-1 x)
  (letrec ((res (stream-delay (stream-cons x res))))
    res))

(define-constant (stream-ones)
  (stream-forever-1 1))

(define-constant (stream-from from)
  (stream-cons from (stream-from (1+ from))))
(define-constant (stream-naturals)
  (stream-from 0))

(define-constant (stream-walk-elements procedure s)
  (while (stream-non-null? s)
    (procedure (stream-car s))
    (set! s (stream-cdr s))))

(define-constant (stream-print-elements s)
  (stream-walk-elements (lambda (x)
                          (display x)
                          (newline))
                        s))

(define-constant (stream-touch-elements s)
  (stream-walk-elements (lambda (x))
                        s))

(define-constant (stream-range a b)
  (if (> a b)
      stream-empty
      (stream-cons a (stream-range (1+ a) b))))

(define-constant (stream-append s1 s2)
  (stream-delay
    (if (stream-null? s1)
        s2
        (stream-cons (stream-car s1)
                     (stream-append (stream-cdr s1) s2)))))

(define-constant (stream-forever-stream s)
  (letrec ((res (stream-delay (stream-append s res))))
    res))

(define-constant (stream-filter p? s)
  (stream-delay
    (cond ((stream-null? s)
           stream-empty)
          ((p? (stream-car s))
           (stream-cons (stream-car s)
                        (stream-filter p? (stream-cdr s))))
          (#t
           (stream-filter p? (stream-cdr s))))))

(define-constant (stream-map f s)
  (stream-delay
    (if (stream-null? s)
        stream-empty
        (stream-cons (f (stream-car s))
                     (stream-map f (stream-cdr s))))))

(define-constant (stream-take s n)
  (stream-delay
    (cond ((zero? n)
           stream-empty)
          ((stream-null? s)
           stream-empty)
          (#t
           (stream-cons (stream-car s)
                        (stream-take (stream-cdr s) (1- n)))))))

(define-constant (stream-drop s n)
  (stream-delay
    (cond ((zero? n)
           s)
          ((stream-null? s)
           stream-empty)
          (#t
           (stream-drop (stream-cdr s) (1- n))))))

(define-constant (stream-fold-left f x xs)
  (if (stream-null? xs)
      x
      (stream-fold-left f
                        (f x (stream-car xs))
                        (stream-cdr xs))))




;;;; Variadic eval and macroexpand.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (eval form . optional-environment)
  (if (null? optional-environment)
      `(primordial-eval ,form '())
      `(primordial-eval ,form ,@optional-environment)))

(define-macro (macroexpand form . optional-environment)
  (if (null? optional-environment)
      `(primordial-macroexpand ,form '())
      `(primordial-macroexpand ,form ,@optional-environment)))




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; The library is now loaded.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is checked at startup to error out in case the library is being loaded
;;; more than once.  Make the defined symbol constant, so that it can't be
;;; undefined.
(define-constant jitterlisp-library-loaded
  #t)




;;;; Scratch.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fibo n)
  (if (< n 2)
      n
      (+ (fibo (- n 2))
         (fibo (- n 1)))))

;;; (define q (macroexpand '(let ((a a) (b a)) a))) q (ast-alpha-convert q)
;;; (define q (macroexpand '(f x (+ 2 3)))) q (ast-optimize q ())
;;; (ast-optimize (macroexpand '(cons 3 (begin2 (define x y) x 7))) ())

;;; An important test:
;;; (ast-optimize (closure-body fibo) '(n))

;;; Something which should get smaller:
;;; (ast-optimize (closure-body ast-simplify-calls) '(ast))

;;; These are interesting because of the sequence in the let bound form:
;;; (ast-optimize (macroexpand '(cons 3 (begin x 7))) ())
;;; (ast-optimize (macroexpand '(let ((a (newline) (newline) (newline))) y)) '())


;;; Primitive composition:
;; jitterlisp> (ast-optimize (macroexpand '(list 1 2)) '())
;; [let #<uninterned:0xe6e120> [primitive #<2-ary primitive cons> [literal 2] [literal ()]] [primitive #<2-ary primitive cons> [literal 1] [variable #<uninterned:0xe6e120>]]]

;;; Is this correct?  I'd say no.  We can move variables across effectful
;;; primitives only when we are sure that referencing them has no effect,
;;; which means that they must be bound or global constants.
;;; (ast-optimize (macroexpand '(begin1 a (display 1) b (display 2) c (display 3) d)) ())
;;; [sequence [primitive #<1-ary primitive display> [literal 1]] [sequence [variable b] [sequence [primitive #<1-ary primitive display> [literal 2]] [sequence [variable c] [sequence [primitive #<1-ary primitive display> [literal 3]] [sequence [variable d] [variable a]]]]]]]
;;
;; Other testcase which should not be simplified if b isn't bound or constant:
;;; jitterlisp> (ast-optimize (macroexpand '(let ((a b)) c)) '())
;;; [variable c]
;;
;;; Special case: this can be simplified even if a is unbound, because the
;;; bound variable is the same as the bound form.  This is now correct by
;;; accident.  I should check for this case.
;; (ast-optimize (macroexpand '(let ((a a)) (cons a a))) '())
;; [primitive #<2-ary primitive cons> [variable a] [variable a]]

