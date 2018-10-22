/* Jitter: general-purpose bitwise macro header.

   Copyright (C) 2018 Luca Saiu
   Written by Luca Saiu

   This file is part of Jitter.

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


#ifndef JITTER_BITWISE_H_
#define JITTER_BITWISE_H_


#include <jitter/jitter.h>


/* Introduction.
 * ************************************************************************** */

/* This header contains some general-purpose macros for bitwise operations. */




/* Powers of two.
 * ************************************************************************** */

/* Expand to an expression evaluating to nonzero if the given argument, assumed
   to be nonzero, does *not* evaluate to a power of two.
   This may evaluate the argument multiple times, but is otherwise efficient and
   always branchless. */
#define JITTER_ISNT_NONZERO_A_POWER_OF_TWO(x)  \
  ((x) & ((x) - 1))

/* Expand to an expression evaluating to nonzero if the given argument, assumed
   to be nonzero, evaluates to a power of two.
   This may evaluate the argument multiple times. */
#define JITTER_IS_NONZERO_A_POWER_OF_TWO(x)  \
  (! JITTER_ISNT_NONZERO_A_POWER_OF_TWO(x))

/* Expand to an expression evaluating to nonzero if the given argument evaluates
   to a power of two.
   This may evaluate the argument multiple times. */
#define JITTER_IS_A_POWER_OF_TWO(x)  \
  (((x) != 0) && JITTER_IS_NONZERO_A_POWER_OF_TWO(x))

/* Expand to an expression evaluating to nonzero if the given argument does
   *not* evaluate to a power of two.
   This may evaluate the argument multiple times. */
#define JITTER_ISNT_A_POWER_OF_TWO(x)  \
  (((x) == 0) || JITTER_ISNT_NONZERO_A_POWER_OF_TWO(x))

/* Given a possibly integer x and a power of two p, expand to an expression
   evaluating to the maximum integer less than or equal to x which is a multiple
   of p.
   The signed version relies on two's complement representation.
   This assumes that p is a power of two and may evaluate both arguments
   multiple times.  However, if both arguments are constant expression, the
   expansion is also a constant expression. */
#define JITTER_PREVIOUS_MULTIPLE_OF_POWER_OF_TWO(x, p)  \
  ((x) & ~ ((p) - 1))

/* Given a possibly signed integer x and a power of two p, expand to an
   expression evaluating to the minimum integer greater than or equal to x which
   is a multiple of p.

   This assumes that p is a power of two and may evaluate both arguments
   multiple times.  However, if both arguments are constant expression, the
   expansion is also a constant expression.

   I learned this technique from from Hacker's Delight, §3-1.
   Let y be the smallest multiple of p greater than or equal to x, where p
   is a power of two.  Then:
     y = (x + p - 1) & - p     [1]
   and also
     y = x + (- x & (p - 1))   [2]
   I am currently using [1].  [2] can be useful in other contexts, because
   the second addend expresses a displacement from x to y. */
#define JITTER_NEXT_MULTIPLE_OF_POWER_OF_TWO(x, p)  \
  (((x) + ((p) - 1)) & ~ ((p) - 1))

#endif // #ifndef JITTER_BITWISE_H_
