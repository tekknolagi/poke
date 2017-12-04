;;; JitterLisp (almost -*- Scheme -*- for Emacs) Fibonacci example.

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


;;;; The Fibonacci sequence, recursive, exponential-time.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (fibo-scheme n)
  (if (< n 2)
      n
      (+ (fibo-scheme (- n 2))
         (fibo-scheme (- n 1)))))

(define (fibo-jitterlisp n)
  (if (primitive primitive-< n 2)
      n
      (primitive primitive-primordial-+
                 (fibo-jitterlisp (primitive primitive-primordial-- n 2))
                 (fibo-jitterlisp (primitive primitive-1- n)))))

(define fibo
  ;;fibo-jitterlisp
  fibo-scheme
  )




;;;; Main.
;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display (fibo 39))
(newline)
