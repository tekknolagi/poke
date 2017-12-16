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




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Error handling.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define (error message)
  (display (cons 'error: (cons message '())))
  (newline)
  fail-by-looking-at-an-unbound-variable)




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Arithmetic and number library.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; Parity: even? and odd?.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (even? n)
  (zero? (remainder n 2)))

(define (odd? n)
  (not (zero? (remainder n 2))))

;;;; number?.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (number? n)
  (fixnum? n)) ;; There is only one number type right now.




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Conses.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; Composed cons selectors.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; I define these early because some composed selectors are used within the
;;; implementation of high-level macros.

;; Length 2.
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

;; Length 3.
(define (caaar x) (car (caar x)))
(define (caadr x) (car (cadr x)))
(define (cadar x) (car (cdar x)))
(define (caddr x) (car (cddr x)))
(define (cdaar x) (cdr (caar x)))
(define (cdadr x) (cdr (cadr x)))
(define (cddar x) (cdr (cdar x)))
(define (cdddr x) (cdr (cddr x)))

;; Length 4.
(define (caaaar x) (car (caaar x)))
(define (caaadr x) (car (caadr x)))
(define (caadar x) (car (cadar x)))
(define (caaddr x) (car (caddr x)))
(define (cadaar x) (car (cdaar x)))
(define (cadadr x) (car (cdadr x)))
(define (caddar x) (car (cddar x)))
(define (cadddr x) (car (cdddr x)))
(define (cdaaar x) (cdr (caaar x)))
(define (cdaadr x) (cdr (caadr x)))
(define (cdadar x) (cdr (cadar x)))
(define (cdaddr x) (cdr (caddr x)))
(define (cddaar x) (cdr (cdaar x)))
(define (cddadr x) (cdr (cdadr x)))
(define (cdddar x) (cdr (cddar x)))
(define (cddddr x) (cdr (cdddr x)))




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Quasiquoting and macros.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; quasiquote-procedure.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define temporary list functions of a few different arities.
(define (list0)         '())
(define (list1 x)       (cons x '()))
(define (list2 x y)     (cons x (cons y '())))
(define (list3 x y z)   (cons x (cons y (cons z '()))))
(define (list4 x y z t) (cons x (cons y (cons z (cons t '())))))

(define (qq-append xs ys)
  (if (list? xs)
      (append xs ys)
      (error '(unquote-splicing argument evaluates to non-list))))

(define (qq-atom x)
  ;; `',x
  (list2 'quote
         x))

(define (qq-recursive x depth)
  (if (cons? x)
      (qq-recursive-cons (car x) (cdr x) depth)
      (qq-atom x)))

;;; ~/r6rs.pdf, §11.17 "Quasiquotation".

(define (qq-recursive-cons x-car x-cdr depth)
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
(define (qq-sigleton-expression expression)
  (list2 'list1
         expression))

;;; Return an s-expression evaluating to something equivalent to a variadic
;;; call to the list function of the given arguments.
(define (qq-variadic-list-expression args)
  (if (null? args)
      ''()
      ;;`(cons ,(car args) ,(qq-variadic-list-expression (cdr args)))))
      (list3 'cons
             (car args)
             (qq-variadic-list-expression (cdr args)))))

;;; Same as qq-variadic-list-expression but for a variadic append.
(define (qq-variadic-append-expression args)
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
(define (qq-recursive-as-car x depth)
  (if (cons? x)
      (qq-recursive-cons-as-car (car x) (cdr x) depth)
      ;; `(list ,(qq-atom x))
      (qq-sigleton-expression (qq-atom x))))

;;; Expand a cons which is the car of a bigger quasiquoted s-expression;
;;; therefore, expand to a list as qq-recursive-as-car does.
(define (qq-recursive-cons-as-car x-car x-cdr depth)
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
(define (quasiquote-procedure x)
  (qq-recursive x 0))




;;;; quasiquote.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Now we can finally define quasiquote as a (low-level) macro.  It is trivial:
;;; low-level-macro-args is bound to all of the arguments of quasiquote , and we
;;; want to fail if those arguments are not a singleton list.  If the arguments
;;; are a singleton list we just call quasiquote-procedure on their car.
(define-low-level-macro quasiquote
  (cond ((null? low-level-macro-args)
         (error '(quasiquote with zero arguments)))
        ((non-cons? low-level-macro-args)
         (error '(quasiquote arguments not a list)))
        ((non-null? (cdr low-level-macro-args))
         (error '(quasiquote arguments more than one or not a list)))
        (#t
         (quasiquote-procedure (car low-level-macro-args)))))




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
(define (destructuring-bind-recursive formals-template
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
         `(let ((,formals-template ,component))
            ,@body-forms))
        ((cons? formals-template)
         ;; Bind both the car and the cdr.  For efficiency's sake name the two
         ;; sub-components in the generated code.
         (let ((car-name (gensym))
               (cdr-name (gensym)))
           `(let ((,car-name (car ,component))
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
(define (destructuring-bind-procedure formals-template args body-forms)
  (destructuring-bind-recursive formals-template args body-forms))

;; FIXME: check that the formals-template doesn't require non-linear bindings.




;;;; destructuring-bind.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This will be convenient to define high-level macros on top of low-level
;;; macros, by destructuring the one low-level macro argument.

;;; Arguments: template structure . body-forms Evaluate structure and locally
;;; bind its components with the variables in the template; return the result of
;;; evaluating the body forms with the bindings visible.
(define-low-level-macro destructuring-bind
  (let ((template (car low-level-macro-args))
        (structure (cadr low-level-macro-args))
        (body-forms (cddr low-level-macro-args)))
    (destructuring-bind-procedure template structure body-forms)))




;;;; High-level macros.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Arguments: (name . formals) . body
;;; Scheme-style, where formals can be an improper list.
(define-low-level-macro define-macro
  (let ((defined-thing (car low-level-macro-args))
        (macro-body-forms (cdr low-level-macro-args)))
    (let ((macro-name (car defined-thing))
          (macro-formals (cdr defined-thing)))
      `(define-low-level-macro ,macro-name
         (destructuring-bind ,macro-formals
                             low-level-macro-args
                             ,@macro-body-forms)))))




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; List library.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; singleton.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (singleton x)
  (cons x '()))




;;;; list?.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: a return statement would be nice here.
(define (list?-iterative xs)
  (let ((go-on #t)
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

(define (list?-tail-recursive xs)
  (cond ((null? xs)
         #t)
        ((cons? xs)
         (list?-tail-recursive (cdr xs)))
        (#t
         #f)))

(define (list? xs)
  (list?-iterative xs))




;;;; replicate.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (replicate-iterative n x)
  (let ((res '()))
    (while (> n 0)
      (set! res (cons x res))
      (set! n (1- n)))
    res))

(define (replicate-tail-recursive-helper n x acc)
  (if (zero? n)
      acc
      (replicate-tail-recursive-helper (1- n) x (cons x acc))))
(define (replicate-tail-recursive n x)
  (replicate-tail-recursive-helper n x '()))

(define (replicate-non-tail-recursive n x)
  (if (zero? n)
      '()
      (cons x (replicate-non-tail-recursive (1- n) x))))

(define (replicate n x)
  (replicate-iterative n x))

;; Just an alias.
(define (make-list n x)
  (replicate n x))




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




;;;; take, take-reversed.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A break or return form would be useful here.
(define (take-reversed-iterative n xs)
  (let ((res '())
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
(define (take-iterative n xs)
  (reverse!-iterative (take-reversed-iterative n xs)))

(define (take-tail-recursive n xs)
  (reverse! (take-reversed-tail-recursive n xs)))

(define (take-non-tail-recursive n xs)
  (cond ((zero? n)
         '())
        ((null? xs)
         '())
        (#t
         (cons (car xs) (take-non-tail-recursive (1- n) (cdr xs))))))

(define (take-reversed-tail-recursive-helper n xs acc)
  (cond ((zero? n)
         acc)
        ((null? xs)
         acc)
        (#t
         (take-reversed-tail-recursive-helper (1- n)
                                              (cdr xs)
                                              (cons (car xs) acc)))))
(define (take-reversed-tail-recursive n xs)
  (take-reversed-tail-recursive-helper n xs '()))

(define (take-reversed n xs)
  (take-reversed-iterative n xs))

(define (take n xs)
  (take-iterative n xs))




;;;; take!
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A break or return form would be useful here.
(define (take!-iterative n xs)
  (if (zero? n)
      '()
      (let ((rest xs)
            (go-on #t))
        (while go-on
          (cond ((= n 1)
                 (if (null? rest)
                     'do-nothing
                     (set-cdr! rest '()))
                 (set! go-on #f))
                ((null? rest)
                 (set! go-on #f))
                (#t
                 (set! rest (cdr rest))
                 (set! n (1- n)))))
        xs)))

(define (take!-tail-recursive-helper n xs)
  (cond ((= n 1)
         (if (null? xs)
             'do-nothing
             (set-cdr! xs '())))
        ((null? xs))
        (#t
         (take!-tail-recursive-helper (1- n) (cdr xs)))))
(define (take!-tail-recursive n xs)
  (if (zero? n)
      '()
      (begin
        (take!-tail-recursive-helper n xs)
        xs)))

(define (take! n xs)
  (take!-iterative n xs))




;;;; drop.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A break or return form would be useful here.
(define (drop-iterative n xs)
  (let ((go-on #t))
    (while go-on
      (cond ((null? xs)
             (set! go-on #f))
            ((zero? n)
             (set! go-on #f))
            (#t
             (set! xs (cdr xs))
             (set! n (1- n)))))
    xs))

(define (drop-tail-recursive n xs)
  (cond ((zero? n)
         xs)
        ((null? xs)
         '())
        (#t
         (drop-tail-recursive (1- n) (cdr xs)))))

(define (drop n xs)
  (drop-iterative n xs))




;;;; alist functions.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (alist? x)
  (cond ((null? x)
         #t)
        ((non-cons? x)
         #f)
        ((cons? (car x))
         (alist? (cdr x)))
        (#t
         #f)))

(define (assq key alist)
  (cond ((null? alist)
         #f)
        ((eq? (caar alist) key)
         (car alist))
        (#t
         (assq key (cdr alist)))))

(define (rassq value alist)
  (cond ((null? alist)
         #f)
        ((eq? (cdar alist) value)
         (car alist))
        (#t
         (rassq value (cdr alist)))))

(define (del-assq object alist)
  (cond ((null? alist)
         '())
        ((eq? (caar alist) object)
         (del-assq object (cdr alist)))
        (#t
         (cons (car alist) (del-assq object (cdr alist))))))

;; FIXME: implement del-assq! .

(define (alist-copy alist)
  (let ((res '())
        (first-cons #f))
    (while (non-null? alist)
      (set! first-cons (car alist))
      (set! res (cons (cons (car first-cons) (cdr first-cons)) res))
      (set! alist (cdr alist)))
    (reverse! res)))

(define (alist-get key alist)
  (let ((a-cons (assq key alist)))
    (if (cons? a-cons)
        (cdr a-cons)
        (error `(alist-get: key ,key not found in alist ,alist)))))




;;;; map!.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (map!-iterative f xs)
  (let ((res xs))
    (while (not (null? xs))
      (set-car! xs (f (car xs)))
      (set! xs (cdr xs)))
    res))

(define (map!-tail-recursive-helper f xs)
  (if (null? xs)
      'done
      (begin
        (set-car! xs (f (car xs)))
        (map!-tail-recursive-helper f (cdr xs)))))
(define (map!-tail-recursive f xs)
  (map!-tail-recursive-helper f xs)
  xs)

(define (map! f xs)
  (map!-iterative f xs))




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

;; FIXME: a return form would also be nice here.
(define (exists?-iterative p xs)
  (let ((res #f)
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

(define (exists?-tail-recursive p xs)
  (cond ((null? xs)
         #f)
        ((p (car xs))
         #t)
        (#t
         (exists?-tail-recursive p (cdr xs)))))

(define (exists? p xs)
  (exists?-iterative p xs))




;;;; for-all?.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: a return form would be nice here.
(define (for-all?-iterative p xs)
  (let ((res #t)
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

(define (for-all?-tail-recursive p xs)
  (cond ((null? xs)
         #t)
        ((p (car xs))
         (for-all?-tail-recursive p (cdr xs)))
        (#t
         #f)))

(define (for-all? p xs)
  (for-all?-iterative p xs))


;;;; filter, filter-reversed.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (filter-reversed-iterative p xs)
  (let ((res '()))
    (while (not (null? xs))
      (if (p (car xs))
          (set! res (cons (car xs) res))
          'do-nothing)
      (set! xs (cdr xs)))
    res))

(define (filter-iterative p xs)
  (reverse!-iterative (filter-reversed-iterative p xs)))

(define (filter-non-tail-recursive p xs)
  (cond ((null? xs)
         '())
        ((p (car xs))
         (cons (car xs) (filter-non-tail-recursive p (cdr xs))))
        (#t
         (filter-non-tail-recursive p (cdr xs)))))

(define (filter-reversed-tail-recursive-helper p xs acc)
  (cond ((null? xs)
         acc)
        ((p (car xs))
         (filter-reversed-tail-recursive-helper p (cdr xs) (cons (car xs) acc)))
        (#t
         (filter-reversed-tail-recursive-helper p (cdr xs) acc))))
(define (filter-reversed-tail-recursive p xs)
  (filter-reversed-tail-recursive-helper p xs '()))

(define (filter-tail-recursive p xs)
  (reverse!-tail-recursive (filter-reversed-tail-recursive p xs)))

(define (filter-reversed p xs)
  (filter-reversed-tail-recursive p xs))

(define (filter p xs)
  (filter-tail-recursive p xs))





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




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Sorting.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; insert.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; FIXME: a return or break form would be nice here.
(define (insert-iterative x xs)
  (let ((smaller-elements-reversed '())
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

(define (insert-tail-recursive-helper x xs smaller-elements-reversed)
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
(define (insert-tail-recursive x xs)
  (insert-tail-recursive-helper x xs '()))

(define (insert-non-tail-recursive x xs)
  (cond ((null? xs)
         (singleton x))
        ((<= x (car xs))
         (cons x xs))
        (#t
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
(define (insert!-iterative x xs)
  (if (null? xs)
      (singleton x)
      (begin
        (insert!-iterative-non-null x xs)
        xs)))

(define (insert!-tail-recursive-non-null x xs)
  (cond ((< x (car xs))
         (insert-as-first! x xs))
        ((null? (cdr xs))
         (insert-as-second! x xs))
        (#t
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




;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Higher order.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;; identity.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (identity x)
  x)




;;;; compose.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compose f g)
  (lambda (x) (f (g x))))

(define (compose-eta f g x)
  (f (g x)))

(define (square-function f)
  (lambda (x) (f (f x))))

(define (square-function-eta f x)
  (f (f x)))




;;;; iterate.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (iterate-iterative-post f n)
  (lambda (x)
    (let ((n n))
      (while (> n 0)
        (set! x (f x))
        (set! n (1- n)))
      x)))

(define (iterate-iterative-pre f n)
  (let ((res identity))
    (while (> n 0)
      (set! res (compose res f))
      (set! n (1- n)))
    res))

(define (iterate-eta f n x)
  (if (zero? n)
      x
      (iterate-eta f (1- n) (f x))))
(define (iterate-tail-recursive-post f n)
  (lambda (x) (iterate-eta f n x)))

;; This builds a function which is even faster than the iterative versions (at
;; least on the naïf JitterLisp interpreter).
(define (iterate-squaring-pre f n)
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
         (let ((f^n/2 (iterate-squaring-pre f (quotient n 2))))
           (square-function f^n/2)))
        (#t
         (compose f (iterate-squaring-pre f (1- n))))))

(define (iterate-squaring-eta f n x)
  ;; This uses the same idea of exponentiation by squaring; the advantage here
  ;; is the very small number of built closures, only O(lg n).
  (cond ((zero? n)
         x)
        ((even? n)
         (iterate-squaring-eta (square-function f) (quotient n 2) x))
        (#t
         (iterate-squaring-eta (square-function f) (quotient n 2) (f x)))))
(define (iterate-squaring-post f n)
  (lambda (x) (iterate-squaring-eta f n x)))

(define (iterate-tail-recursive-pre-helper f n acc)
  (if (zero? n)
      acc
      (iterate-tail-recursive-pre-helper f (1- n) (compose acc f))))
(define (iterate-tail-recursive-pre f n)
  (iterate-tail-recursive-pre-helper f n identity))

(define (iterate f n)
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




;;;; Variadic list operations.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (list . elements)
  (if (null? elements)
      '()
      `(cons ,(car elements) (list ,@(cdr elements)))))




;;;; High-level syntax: let*.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (let* bindings . body-forms)
  (if (null? bindings)
      `(begin ,@body-forms)
      `(let ((,(caar bindings) ,@(cdar bindings)))
         (let* ,(cdr bindings)
           ,@body-forms))))




;;;; High-level syntax: letrec.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (letrec bindings . body-forms)
  `(let ,(map (lambda (binding) `(,(car binding) (undefined)))
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

;;; Redefine let form to expand to either the primordial let or named-let,
;;; according to the shape of the first argument.  In order to do this we first
;;; have to save the oridinal let macro implemented in C, to be reused in one of
;;; the two cases.
(define primordial-let
  let)
(define-macro (let first-argument . other-arguments)
  (if (symbol? first-argument)
      `(named-let ,first-argument ,@other-arguments)
      `(primordial-let ,first-argument ,@other-arguments)))




;;;; High-level syntax: case.
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


;;;; Variadic arithmetic.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Keep primordial operations with different names.
(define primordial-+ +)
(define primordial-- -)
(define primordial-* *)
(define primordial-/ /)

;;; Reuse primordial names for variadic macros expanding to calls.

;; FIXME: nested quasiquoting is already difficult to handle here.  I can do the
;; same thing more simply with higher-order procedures, using simple macro
;; wrappers on top of fold-left and fold-right.
(define-macro (define-right-nested-variadic-extension operator original-name
                neutral original-is-primitive)
  (let ((operands-name (gensym))
        (possibly-primitive (if original-is-primitive '(primitive) '())))
    `(define-macro (,operator . ,operands-name)
       (cond ((null? ,operands-name)
              ,neutral)
             ((null? (cdr ,operands-name))
              (car ,operands-name))
             (#t
              `(,@',possibly-primitive
                       ,',original-name
                       ,(car ,operands-name)
                       (,',operator ,@(cdr ,operands-name))))))))

(define-macro (define-associative-variadic-extension operator
                original-name neutral original-is-primitive)
  `(define-right-nested-variadic-extension ,operator ,original-name
     ,neutral ,original-is-primitive))

(define-associative-variadic-extension + primordial-+ 0 #t)
(define-associative-variadic-extension * primordial-* 1 #t)

(define-macro (- . operands)
  (cond ((null? operands)
         (error '(-: no arguments)))
        ((null? (cdr operands))
         `(primitive negate ,@operands))
        (#t
         `(primitive primordial--
                     ,(car operands)
                     (+ ,@(cdr operands))))))

(define-macro (/ . operands)
  (cond ((null? operands)
         (error '(/: no arguments)))
        ((null? (cdr operands))
         `(primitive primordial-/ 1 ,@operands))
        (#t
         `(primitive primordial-/
                     ,(car operands)
                     (* ,@(cdr operands))))))

(define previous-append append)

;; FIXME: rename the append operator above in quasiquoting functions before
;; renaming this to append.
;; Calls in code which is already in AST form will be affected otherwise.
(define-right-nested-variadic-extension variadic-append previous-append () #f)
