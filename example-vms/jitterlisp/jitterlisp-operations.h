/* Jittery Lisp: operations on JitterLisp objects: header.

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


#ifndef JITTERLISP_OPERATIONS_H_
#define JITTERLISP_OPERATIONS_H_

/* Include the Gnulib header. */
#include <config.h>

#include <stdbool.h>

#include "jitterlisp-sexpression.h"
#include "jitterlisp-allocator.h"




/* Introduction and conventions.
 * ************************************************************************** */

/* JitterLisp operations are implemented as macros, with the number of function
   calls reduced to a mininum..

   Rationale: we want to avoid function calls from VM instructions, particularly
   for common operations.  Wrapped functions introduce a memory indirection per
   call at run time, so they are kept to a minimum. */

/* JitterLisp operation macros expand to C *statements*, not C expressions.
   Operations having one or more results are macros with l-values as their
   output arguments, which read objects and write other objects.

   Rationale: it would be possible to use expression by relying on the GNU C
   expression-as-statement extension, but as a Jitter example I want JitterLisp
   to be as portabile as reasonably possible.  I also want to be able to
   conditioanlize definitions with respect to the architecture and in particular
   to the machine word size, which makes boxedness configuration-dependent in
   many cases: what would be easy to define as an expression with a certain
   representation would be difficult or impossible with another.
   The style of having output arguments as l-values to be modified, while not
   the most friendly to the programmer, is well suited to VMs and scales
   well from stacks to registers: notice that a Jitter stack allows easy access
   to its top or undertop as an l-value, just like a register. */




/* Fixnum-to-fixnum operations.
 * ************************************************************************** */

/* These operations take one or more fixnums and compute another fixnum as a
   result.  Right now there is no other numeric type in JitterLisp, but these
   will have to become more complicated in the future. */



#endif // #ifndef JITTERLISP_OPERATIONS_H_
