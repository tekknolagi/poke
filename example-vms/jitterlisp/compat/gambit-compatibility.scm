;;; Gambit -*- Scheme -*- compatibility macros to run JitterLisp code.

;;; Copyright (C) 2017 Luca Saiu
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


;;;; Invoking.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This works for me:

;; cat ~/repos/jitter/example-vms/jitterlisp/compatibility/gambit-compatibility.scm ~/repos/jitter/example-vms/jitterlisp/jitterlisp-library.lisp | time -p gsi /dev/stdin




;;;; Predecessor and successor.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These are performance-critical and I use them a lot in JitterLisp, so it's
;; better not to introduce a procedure layer.
(define-macro (1- expression) `(- ,expression 1))
(define-macro (1+ expression) `(+ ,expression 1))




;;;; Iteration.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Define a while loop macro.
(define-macro (while guard . body-forms)
  (let ((loop-name (gensym)))
    `(let ,loop-name ()
       (if ,guard
           (begin
             ,@body-forms
             (,loop-name))))))
