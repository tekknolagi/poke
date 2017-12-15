/* Jittery Lisp: interpreter: naïve C version header.

   Copyright (C) 2017 Luca Saiu
   Written by Luca Saiu

   This file is part of the Jittery Lisp language implementation, distributed as
   an example along with Jitter under the same license.

   Jitter is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Jitter is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jitter.  If not, see <http://www.gnu.org/licenses/>. */


#ifndef JITTERLISP_EVAL_INTERPRETER_H_
#define JITTERLISP_EVAL_INTERPRETER_H_

#include "jitterlisp-sexpression.h"



/* Non-Jittery interpreter.
 * ************************************************************************** */

/* This is a naïf interpreter directly written in C not using a Jittery VM and
   intended as a baseline to compare against. */




/* Non-Jittery AST interpreter.
 * ************************************************************************** */

/* Return the result of the evaluation of the given form (as an encoded AST),
   which must be already macroexpanded, in the given non-global environment. */
jitterlisp_object
jitterlisp_eval_interpreter_ast (jitterlisp_object ast, jitterlisp_object env);




/* Non-Jittery interpreter.
 * ************************************************************************** */

/* Macroexpand the given JitterLisp form, and return the result of its
   evaluation, using the global environment only. */
jitterlisp_object
jitterlisp_eval_globally_interpreter (jitterlisp_object form);

/* Macroexpand the given JitterLisp form, and return the result of its
   evaluation, using the given non-global environment. */
jitterlisp_object
jitterlisp_eval_interpreter (jitterlisp_object form, jitterlisp_object env);

#endif // #ifndef JITTERLISP_EVAL_INTERPRETER_H_
