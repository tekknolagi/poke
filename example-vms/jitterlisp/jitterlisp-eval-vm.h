/* Jittery Lisp: interpreter: naïve C version header.

   Copyright (C) 2017, 2018 Luca Saiu
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


#ifndef JITTERLISP_EVAL_VM_H_
#define JITTERLISP_EVAL_VM_H_

#include "jitterlisp-sexpression.h"



/* Jittery engine.
 * ************************************************************************** */

/* This is the evaluation engine using the Jittery VM. */




/* Jittery engine.
 * ************************************************************************** */

/* Macroexpand the given JitterLisp form, and return the result of its
   evaluation, using the global environment only.  Evaluate using the Jittery
   VM. */
jitterlisp_object
jitterlisp_eval_globally_vm (jitterlisp_object form);

/* Macroexpand the given JitterLisp form, and return the result of its
   evaluation, using the given non-global environment shaped as described in
   jitterlisp-utility.h .  Evaluate using the Jittery VM. */
jitterlisp_object
jitterlisp_eval_vm (jitterlisp_object form, jitterlisp_object env);

/* Return the result of applying closure_value, which is assumed to be an
   already evaluated closure (not checked by the function) to the operands,
   assumed to be a list (not checked for) of already evaluated operands.
   Evaluate using the Jittery VM. */
jitterlisp_object
jitterlisp_apply_vm (jitterlisp_object closure_value,
                              jitterlisp_object operands_as_list);


#endif // #ifndef JITTERLISP_EVAL_VM_H_
