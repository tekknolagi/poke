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


/* The operations defined here perform no type checking: it is assumed, and not
   checked, that the operands have the required tags. */




/* Expression operations.
 * ************************************************************************** */

// FIXME: describe the expression-operation idea.

// FIXME: JITTERLISP_EXP_




/* Fixnums-to-fixnum and fixnum-to-fixnum expression operations.
 * ************************************************************************** */

// FIXME: JITTERLISP_EXP_FF_F_

/* These operations take one or more fixnums and compute another fixnum as a
   result.  Right now there is no other numeric type in JitterLisp, but these
   will have to become more complicated in the future. */

/* Expand to an r-value expression evaluating to the tagged fixnum operation
   result having the given tagged fixnum operands, and the given infix C
   operation as the operator (the C operator working on untagged operands). */
#define JITTERLISP_EXP_FF_F_BINARY(_jitterlisp_infix,            \
                                   _jitterlisp_tagged_fixnum_a,  \
                                   _jitterlisp_tagged_fixnum_b)  \
  JITTERLISP_FIXNUM_ENCODE(                                      \
     (JITTERLISP_FIXNUM_DECODE(_jitterlisp_tagged_fixnum_a))     \
     _jitterlisp_infix                                           \
     (JITTERLISP_FIXNUM_DECODE(_jitterlisp_tagged_fixnum_b)))

/* Right now fixnum-to-fixnum operations behave "correctly" on overflow and
   underflow, where correctly means that the result is always tagged correctly
   -- but the result decoded value will be whatever the C operators yielded.
   I'll have to do something more complex, and almost certainly less efficient,
   after introducing bignums. */

/* The plus and minus operations can be defined in a more efficient way than the
   others with respect to tagging, even more if the fixnum tag is zero.
   Signedness does not matter for sum and subtraction on a two's complement
   machine, so we can avoid casting to and from a signed integer. */
#if JITTERLISP_FIXNUM_PTAG == 0
# define JITTERLISP_EXP_FF_F_PLUS_OR_MINUS(_jitterlisp_infix,            \
                                           _jitterlisp_tagged_fixnum_a,  \
                                           _jitterlisp_tagged_fixnum_b)  \
    JITTERLISP_WITH_TAG_MASKED_ON(                                       \
       ((_jitterlisp_tagged_fixnum_a)                                    \
        _jitterlisp_infix                                                \
        (_jitterlisp_tagged_fixnum_b)),                                  \
       JITTERLISP_FIXNUM_PTAG,                                           \
       JITTERLISP_FIXNUM_STAG,                                           \
       JITTERLISP_FIXNUM_STAG_BIT_NO)
#else // JITTERLISP_FIXNUM_PTAG != 0
# define JITTERLISP_EXP_FF_F_PLUS_OR_MINUS(_jitterlisp_infix,            \
                                           _jitterlisp_tagged_fixnum_a,  \
                                           _jitterlisp_tagged_fixnum_b)  \
    /* Notice that the infix operation is on unsigned operands. */       \
    JITTERLISP_WITH_TAG_MASKED_ON(                                       \
       ((_jitterlisp_tagged_fixnum_a)                                    \
        _jitterlisp_infix                                                \
        (JITTERLISP_WITH_TAG_SUBTRACTED(                                 \
           (_jitterlisp_tagged_fixnum_b),                                \
           JITTERLISP_FIXNUM_PTAG,                                       \
           JITTERLISP_FIXNUM_STAG,                                       \
           JITTERLISP_FIXNUM_STAG_BIT_NO))),                             \
       JITTERLISP_FIXNUM_PTAG,                                           \
       JITTERLISP_FIXNUM_STAG,                                           \
       JITTERLISP_FIXNUM_STAG_BIT_NO)
#endif // #if JITTERLISP_FIXNUM_PTAG == 0

/* Expression oprations on fixnums. */
#define JITTERLISP_EXP_FF_F_PLUS(_jitterlisp_tagged_fixnum_a,     \
                                 _jitterlisp_tagged_fixnum_b)     \
  JITTERLISP_EXP_FF_F_PLUS_OR_MINUS(+,                            \
                                    _jitterlisp_tagged_fixnum_a,  \
                                    _jitterlisp_tagged_fixnum_b)
#define JITTERLISP_EXP_FF_F_MINUS(_jitterlisp_tagged_fixnum_a,    \
                                  _jitterlisp_tagged_fixnum_b)    \
  JITTERLISP_EXP_FF_F_PLUS_OR_MINUS(-,                            \
                                    _jitterlisp_tagged_fixnum_a,  \
                                    _jitterlisp_tagged_fixnum_b)
#define JITTERLISP_EXP_FF_F_TIMES(_jitterlisp_tagged_fixnum_a,  \
                                  _jitterlisp_tagged_fixnum_b)  \
  JITTERLISP_EXP_FF_F_BINARY(*,                                 \
                             _jitterlisp_tagged_fixnum_a,       \
                             _jitterlisp_tagged_fixnum_b)
#define JITTERLISP_EXP_FF_F_DIVIDED(_jitterlisp_tagged_fixnum_a,  \
                                _jitterlisp_tagged_fixnum_b)      \
  JITTERLISP_EXP_FF_F_BINARY(/,                                   \
                             _jitterlisp_tagged_fixnum_a,         \
                             _jitterlisp_tagged_fixnum_b)
#define JITTERLISP_EXP_FF_F_REMAINDER(_jitterlisp_tagged_fixnum_a,  \
                                  _jitterlisp_tagged_fixnum_b)      \
  JITTERLISP_EXP_FF_F_BINARY(%,                                     \
                             _jitterlisp_tagged_fixnum_a,           \
                             _jitterlisp_tagged_fixnum_b)
#define JITTERLISP_EXP_F_F_MINUS(_jitterlisp_tagged_fixnum_a)  \
  JITTERLISP_EXP_FF_F_MINUS(JITTERLISP_FIXNUM_ENCODE(0),       \
                            _jitterlisp_tagged_fixnum_a)




/* Fixnums-to-boolean expression operations.
 * ************************************************************************** */

/* In the case of comparison expression operations on fixnums a solution in the
   spirit of the more efficient solution for plus and minus above works with
   *any* tag.  Notice that the operands must be compared as signed. */
#define JITTERLISP_EXP_FF_B_COMPARISON(_jitterlisp_infix,            \
                                       _jitterlisp_tagged_fixnum_a,  \
                                       _jitterlisp_tagged_fixnum_b)  \
  JITTERLISP_BOOLEAN_ENCODE(((jitter_int)                            \
                             (_jitterlisp_tagged_fixnum_a))          \
                            _jitterlisp_infix                        \
                            ((jitter_int)                            \
                             (_jitterlisp_tagged_fixnum_b)))

/* Boolean operations on fixnum operands. */
#define JITTERLISP_EXP_FF_B_EQUAL(_jitterlisp_tagged_fixnum_a,  \
                                  _jitterlisp_tagged_fixnum_b)  \
  JITTERLISP_EXP_FF_B_COMPARISON(==,                            \
                                 _jitterlisp_tagged_fixnum_a,   \
                                 _jitterlisp_tagged_fixnum_b)
#define JITTERLISP_EXP_FF_B_NOTEQUAL(_jitterlisp_tagged_fixnum_a,  \
                                     _jitterlisp_tagged_fixnum_b)  \
  JITTERLISP_EXP_FF_B_COMPARISON(!=,                               \
                                 _jitterlisp_tagged_fixnum_a,      \
                                 _jitterlisp_tagged_fixnum_b)
#define JITTERLISP_EXP_FF_B_LESS(_jitterlisp_tagged_fixnum_a,  \
                                 _jitterlisp_tagged_fixnum_b)  \
  JITTERLISP_EXP_FF_B_COMPARISON(<,                            \
                                 _jitterlisp_tagged_fixnum_a,  \
                                 _jitterlisp_tagged_fixnum_b)
#define JITTERLISP_EXP_FF_B_NOTLESS(_jitterlisp_tagged_fixnum_a,  \
                                    _jitterlisp_tagged_fixnum_b)  \
  JITTERLISP_EXP_FF_B_COMPARISON(>=,                              \
                                 _jitterlisp_tagged_fixnum_a,     \
                                 _jitterlisp_tagged_fixnum_b)
#define JITTERLISP_EXP_FF_B_GREATER(_jitterlisp_tagged_fixnum_a,  \
                                    _jitterlisp_tagged_fixnum_b)  \
  JITTERLISP_EXP_FF_B_LESS(_jitterlisp_tagged_fixnum_b,           \
                           _jitterlisp_tagged_fixnum_a)
#define JITTERLISP_EXP_FF_B_NOTGREATER(_jitterlisp_tagged_fixnum_a,  \
                                       _jitterlisp_tagged_fixnum_b)  \
  JITTERLISP_EXP_FF_B_NOTLESS(_jitterlisp_tagged_fixnum_b,           \
                              _jitterlisp_tagged_fixnum_a)




#endif // #ifndef JITTERLISP_OPERATIONS_H_
