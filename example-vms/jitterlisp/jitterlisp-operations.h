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
#include "jitterlisp-eval-interpreter.h" // FIXME: review after writing VM eval.
#include "jitterlisp-macros.h"
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
   checked, that every operand has the required tag. */




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
#if (JITTERLISP_FIXNUM_TAG) == 0
# define JITTERLISP_EXP_FF_F_PLUS_OR_MINUS(_jitterlisp_infix,            \
                                           _jitterlisp_tagged_fixnum_a,  \
                                           _jitterlisp_tagged_fixnum_b)  \
    JITTERLISP_WITH_TAG_MASKED_ON(                                       \
       ((_jitterlisp_tagged_fixnum_a)                                    \
        _jitterlisp_infix                                                \
        (_jitterlisp_tagged_fixnum_b)),                                  \
       JITTERLISP_FIXNUM_TAG,                                           \
       JITTERLISP_FIXNUM_TAG_BIT_NO)
#else // fixnum tag non-zero
# define JITTERLISP_EXP_FF_F_PLUS_OR_MINUS(_jitterlisp_infix,            \
                                           _jitterlisp_tagged_fixnum_a,  \
                                           _jitterlisp_tagged_fixnum_b)  \
    /* Notice that the infix operation is on unsigned operands. */       \
    JITTERLISP_WITH_TAG_MASKED_ON(                                       \
       ((_jitterlisp_tagged_fixnum_a)                                    \
        _jitterlisp_infix                                                \
        (JITTERLISP_WITH_TAG_SUBTRACTED(                                 \
           (_jitterlisp_tagged_fixnum_b),                                \
           JITTERLISP_FIXNUM_TAG,                                       \
           JITTERLISP_FIXNUM_TAG_BIT_NO))),                             \
       JITTERLISP_FIXNUM_TAG,                                           \
       JITTERLISP_FIXNUM_TAG_BIT_NO)
#endif // #if fixnum tag is zero

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




/* Cons expression operations.
 * ************************************************************************** */

/* Cons selectors operations, including the ones for composed selectors, are
   always definable as expressions as they don't require heap allocation.  (That
   is not the case for cons construction.) */

/* Expand to an r-value expression evaluating to the tagged car or cdr of the
   given tagged cons operand. */
#define JITTERLISP_EXP_C_A_CAR(_jitterlisp_tagged_cons)   \
  (JITTERLISP_CONS_DECODE(_jitterlisp_tagged_cons)->car)
#define JITTERLISP_EXP_C_A_CDR(_jitterlisp_tagged_cons)   \
  (JITTERLISP_CONS_DECODE(_jitterlisp_tagged_cons)->cdr)

/* Composed cons selectors.  Given a tagged value expand to an expression
   evaluating to its tagged caar, cadr, and so on.  These are compositions of
   JITTERLISP_EXP_C_A_CAR and JITTERLISP_EXP_C_A_CDR . */
/* Length 2. */
#define JITTERLISP_EXP_C_A_CAAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CAR(JITTERLISP_EXP_C_A_CAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CADR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CAR(JITTERLISP_EXP_C_A_CDR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CDAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CDR(JITTERLISP_EXP_C_A_CAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CDDR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CDR(JITTERLISP_EXP_C_A_CDR(_jitterlisp_tagged_cons))
/* Length 3. */
#define JITTERLISP_EXP_C_A_CAAAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CAR(JITTERLISP_EXP_C_A_CAAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CAADR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CAR(JITTERLISP_EXP_C_A_CADR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CADAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CAR(JITTERLISP_EXP_C_A_CDAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CADDR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CAR(JITTERLISP_EXP_C_A_CDDR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CDAAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CDR(JITTERLISP_EXP_C_A_CAAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CDADR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CDR(JITTERLISP_EXP_C_A_CADR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CDDAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CDR(JITTERLISP_EXP_C_A_CDAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CDDDR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CDR(JITTERLISP_EXP_C_A_CDDR(_jitterlisp_tagged_cons))
/* Length 4. */
#define JITTERLISP_EXP_C_A_CAAAAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CAR(JITTERLISP_EXP_C_A_CAAAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CAAADR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CAR(JITTERLISP_EXP_C_A_CAADR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CAADAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CAR(JITTERLISP_EXP_C_A_CADAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CAADDR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CAR(JITTERLISP_EXP_C_A_CADDR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CADAAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CAR(JITTERLISP_EXP_C_A_CDAAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CADADR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CAR(JITTERLISP_EXP_C_A_CDADR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CADDAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CAR(JITTERLISP_EXP_C_A_CDDAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CADDDR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CAR(JITTERLISP_EXP_C_A_CDDDR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CDAAAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CDR(JITTERLISP_EXP_C_A_CAAAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CDAADR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CDR(JITTERLISP_EXP_C_A_CAADR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CDADAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CDR(JITTERLISP_EXP_C_A_CADAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CDADDR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CDR(JITTERLISP_EXP_C_A_CADDR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CDDAAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CDR(JITTERLISP_EXP_C_A_CDAAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CDDADR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CDR(JITTERLISP_EXP_C_A_CDADR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CDDDAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CDR(JITTERLISP_EXP_C_A_CDDAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C_A_CDDDDR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C_A_CDR(JITTERLISP_EXP_C_A_CDDDR(_jitterlisp_tagged_cons))




/* FIXME: move
 * ************************************************************************** */

#define JITTER_BEGIN_  \
  do                   \
    {

#define JITTER_END_    \
    }                  \
  while (false)




/* Fixnums-to-fixnum operations.
 * ************************************************************************** */

// FIXME: implement.

#define JITTERLISP_PLUS_(_jitterlisp_out, _jitterlisp_in0, _jitterlisp_in1)  \
  JITTER_BEGIN_                                                              \
    _jitterlisp_out                                                          \
      = JITTERLISP_EXP_FF_F_PLUS(_jitterlisp_in0, _jitterlisp_in1);          \
  JITTER_END_

#define JITTERLISP_MINUS_(_jitterlisp_out, _jitterlisp_in0, _jitterlisp_in1)  \
  JITTER_BEGIN_                                                               \
    _jitterlisp_out                                                           \
      = JITTERLISP_EXP_FF_F_MINUS(_jitterlisp_in0, _jitterlisp_in1);          \
  JITTER_END_

#define JITTERLISP_TIMES_(_jitterlisp_out, _jitterlisp_in0, _jitterlisp_in1)  \
  JITTER_BEGIN_                                                               \
    _jitterlisp_out                                                           \
      = JITTERLISP_EXP_FF_F_TIMES(_jitterlisp_in0, _jitterlisp_in1);          \
  JITTER_END_

#define JITTERLISP_DIVIDED_(_jitterlisp_out, _jitterlisp_in0, _jitterlisp_in1) \
  JITTER_BEGIN_                                                                \
    if (_jitterlisp_in1 == JITTERLISP_FIXNUM_ENCODE(0))                        \
      jitterlisp_error_cloned ("division by zero");                            \
    _jitterlisp_out                                                            \
      = JITTERLISP_EXP_FF_F_DIVIDED(_jitterlisp_in0, _jitterlisp_in1);         \
  JITTER_END_

#define JITTERLISP_QUOTIENT_(_jitterlisp_out, _jitterlisp_in0, _jitterlisp_in1) \
  JITTERLISP_DIVIDED_(_jitterlisp_out, _jitterlisp_in0, _jitterlisp_in1)

#define JITTERLISP_REMAINDER_(_jitterlisp_out, _jitterlisp_in0,           \
                              _jitterlisp_in1)                            \
  JITTER_BEGIN_                                                           \
    if (_jitterlisp_in1 == JITTERLISP_FIXNUM_ENCODE(0))                   \
      jitterlisp_error_cloned ("remainder of division by zero");          \
    _jitterlisp_out                                                       \
      = JITTERLISP_EXP_FF_F_REMAINDER(_jitterlisp_in0, _jitterlisp_in1);  \
  JITTER_END_

#define JITTERLISP_1PLUS_(_jitterlisp_out, _jitterlisp_in0)     \
  JITTER_BEGIN_                                                 \
    _jitterlisp_out                                             \
      = JITTERLISP_EXP_FF_F_PLUS(_jitterlisp_in0,               \
                                 JITTERLISP_FIXNUM_ENCODE(1));  \
  JITTER_END_
#define JITTERLISP_1MINUS_(_jitterlisp_out, _jitterlisp_in0)     \
  JITTER_BEGIN_                                                  \
    _jitterlisp_out                                              \
      = JITTERLISP_EXP_FF_F_MINUS(_jitterlisp_in0,               \
                                  JITTERLISP_FIXNUM_ENCODE(1));  \
  JITTER_END_
#define JITTERLISP_NEGATE_(_jitterlisp_out, _jitterlisp_in0)  \
  JITTER_BEGIN_                                               \
    _jitterlisp_out                                           \
      = JITTERLISP_EXP_F_F_MINUS(_jitterlisp_in0);            \
  JITTER_END_




/* Fixnums-to-boolean operations.
 * ************************************************************************** */

// FIXME: implement.

#define JITTERLISP_LESSP_(_jitterlisp_out, _jitterlisp_in0,           \
                          _jitterlisp_in1)                            \
  JITTER_BEGIN_                                                       \
    _jitterlisp_out                                                   \
      = JITTERLISP_EXP_FF_B_LESS(_jitterlisp_in0, _jitterlisp_in1);   \
  JITTER_END_
#define JITTERLISP_GREATERP_(_jitterlisp_out, _jitterlisp_in0,          \
                             _jitterlisp_in1)                           \
  JITTERLISP_LESSP_(_jitterlisp_out, _jitterlisp_in1, _jitterlisp_in0)
#define JITTERLISP_NOTLESSP_(_jitterlisp_out, _jitterlisp_in0,          \
                             _jitterlisp_in1)                           \
  JITTER_BEGIN_                                                         \
    _jitterlisp_out                                                     \
      = JITTERLISP_EXP_FF_B_NOTLESS(_jitterlisp_in0, _jitterlisp_in1);  \
  JITTER_END_
#define JITTERLISP_NOTGREATERP_(_jitterlisp_out, _jitterlisp_in0,          \
                                _jitterlisp_in1)                           \
  JITTERLISP_NOTLESSP_(_jitterlisp_out, _jitterlisp_in1, _jitterlisp_in0)




/* Boolean operations.
 * ************************************************************************** */

/* Compute a tagged boolean, #t iff the argument is #f. */
#define JITTERLISP_NOT_(_jitterlisp_out, _jitterlisp_in0)              \
  JITTER_BEGIN_                                                        \
    _jitterlisp_out =                                                  \
      JITTERLISP_BOOLEAN_ENCODE((_jitterlisp_in0)                      \
                                == JITTERLISP_BOOLEAN_ENCODE(false));  \
  JITTER_END_




/* Cons operations.
 * ************************************************************************** */

// FIXME: comment.

#define JITTERLISP_CONS_(_jitterlisp_out, _jitterlisp_in0, _jitterlisp_in1)  \
  JITTER_BEGIN_                                                              \
    struct jitterlisp_cons *_jitterlisp_tmp                                  \
      = JITTERLISP_CONS_MAKE_UNINITIALIZED_UNENCODED();                      \
    _jitterlisp_tmp->car = (_jitterlisp_in0);                                \
    _jitterlisp_tmp->cdr = (_jitterlisp_in1);                                \
    (_jitterlisp_out) = JITTERLISP_CONS_ENCODE(_jitterlisp_tmp);             \
  JITTER_END_

#define JITTERLISP_CAR_(_jitterlisp_out, _jitterlisp_in0)  \
  JITTER_BEGIN_                                            \
    struct jitterlisp_cons *_jitterlisp_tmp                \
      = JITTERLISP_CONS_DECODE(_jitterlisp_in0);           \
    (_jitterlisp_out) = _jitterlisp_tmp->car;              \
  JITTER_END_
#define JITTERLISP_CDR_(_jitterlisp_out, _jitterlisp_in0)  \
  JITTER_BEGIN_                                            \
    struct jitterlisp_cons *_jitterlisp_tmp                \
      = JITTERLISP_CONS_DECODE(_jitterlisp_in0);           \
    (_jitterlisp_out) = _jitterlisp_tmp->cdr;              \
  JITTER_END_

#define JITTERLISP_SET_CARB_(_jitterlisp_out,      \
                             _jitterlisp_cons,     \
                             _jitterlisp_new_car)  \
  JITTER_BEGIN_                                    \
    struct jitterlisp_cons *_jitterlisp_tmp        \
      = JITTERLISP_CONS_DECODE(_jitterlisp_cons);  \
    _jitterlisp_tmp->car = (_jitterlisp_new_car);  \
    (_jitterlisp_out) = JITTERLISP_NOTHING;        \
  JITTER_END_
#define JITTERLISP_SET_CDRB_(_jitterlisp_out,      \
                             _jitterlisp_cons,     \
                             _jitterlisp_new_cdr)  \
  JITTER_BEGIN_                                    \
    struct jitterlisp_cons *_jitterlisp_tmp        \
      = JITTERLISP_CONS_DECODE(_jitterlisp_cons);  \
    _jitterlisp_tmp->cdr = (_jitterlisp_new_cdr);  \
    (_jitterlisp_out) = JITTERLISP_NOTHING;        \
  JITTER_END_




/* Closure operations.
 * ************************************************************************** */

// FIXME: comment.

#define JITTERLISP_CLOSURE_(_jitterlisp_out,                       \
                            _jitterlisp_in0,                       \
                            _jitterlisp_in1,                       \
                            _jitterlisp_in2)                       \
  JITTER_BEGIN_                                                    \
    struct jitterlisp_closure *_jitterlisp_tmp                     \
      = JITTERLISP_CLOSURE_MAKE_UNINITIALIZED_UNENCODED();         \
    _jitterlisp_tmp->environment = _jitterlisp_in0;                \
    _jitterlisp_tmp->formals = _jitterlisp_in1;                    \
    _jitterlisp_tmp->body = _jitterlisp_in2;                       \
    (_jitterlisp_out) = JITTERLISP_CLOSURE_ENCODE(_jitterlisp_tmp);  \
  JITTER_END_




/* Symbol operations.
 * ************************************************************************** */

// FIXME: comment.

#define JITTERLISP_GENSYM_(_jitterlisp_out)                           \
  JITTER_BEGIN_                                                       \
    struct jitterlisp_symbol *_jitterlisp_tmp                         \
      = JITTERLISP_SYMBOL_UNINTERNED_MAKE_UNINITIALIZED_UNENCODED();  \
    _jitterlisp_tmp->name_or_NULL = NULL;                             \
    _jitterlisp_tmp->global_value = JITTERLISP_UNDEFINED;             \
    (_jitterlisp_out) = JITTERLISP_SYMBOL_ENCODE(_jitterlisp_tmp);    \
  JITTER_END_




/* Vector operations.
 * ************************************************************************** */

// FIXME: comment.

/* Set the l-value _jitterlisp_out to be a new vector with the given
   (fixnum-encoded) number of elements, each initialized to the given encoded
   value. */
#define JITTERLISP_VECTOR_MAKE_(_jitterlisp_out,                       \
                                _jitterlisp_in_elt_no,                 \
                                _jitterlisp_in_initial_elt)            \
  JITTER_BEGIN_                                                        \
    const jitter_uint _jitterlisp_elt_no_untagged                      \
      = JITTERLISP_FIXNUM_DECODE(_jitterlisp_in_elt_no);               \
    const jitterlisp_object _jitterlisp_in_initial_elt_evaluated       \
      = (_jitterlisp_in_initial_elt);                                  \
    jitterlisp_object *_jitterlisp_elts                                \
      = ((jitterlisp_object *)                                         \
         jitterlisp_allocate (                                         \
            JITTERLISP_ALIGNED_SIZE(sizeof (jitterlisp_object)         \
                                    * _jitterlisp_elt_no_untagged)));  \
    int _jitterlisp_i;                                                 \
    for (_jitterlisp_i = 0;                                            \
         _jitterlisp_i < _jitterlisp_elt_no_untagged;                  \
         _jitterlisp_i ++)                                             \
      _jitterlisp_elts [_jitterlisp_i]                                 \
        = _jitterlisp_in_initial_elt_evaluated;                        \
    struct jitterlisp_vector *_jitterlisp_tmp                          \
      = JITTERLISP_VECTOR_MAKE_UNINITIALIZED_UNENCODED();              \
    _jitterlisp_tmp->element_no                                        \
      = JITTERLISP_FIXNUM_ENCODE(_jitterlisp_elt_no_untagged);         \
    _jitterlisp_tmp->elements = _jitterlisp_elts;                      \
    (_jitterlisp_out) = JITTERLISP_VECTOR_ENCODE(_jitterlisp_tmp);     \
  JITTER_END_




/* Non-primitive macro operations.
 * ************************************************************************** */

// FIXME: comment.

#define JITTERLISP_NON_PRIMITIVE_MACRO_(_jitterlisp_out,                        \
                                        _jitterlisp_in0,                        \
                                        _jitterlisp_in1,                        \
                                        _jitterlisp_in2)                        \
  JITTER_BEGIN_                                                                 \
    struct jitterlisp_closure *_jitterlisp_tmp                                  \
      = JITTERLISP_NON_PRIMITIVE_MACRO_MAKE_UNINITIALIZED_UNENCODED();          \
    _jitterlisp_tmp->environment = _jitterlisp_in0;                             \
    _jitterlisp_tmp->formals = _jitterlisp_in1;                                 \
    _jitterlisp_tmp->body = _jitterlisp_in2;                                    \
    (_jitterlisp_out) = JITTERLISP_NON_PRIMITIVE_MACRO_ENCODE(_jitterlisp_tmp); \
  JITTER_END_




/* Comparison operations.
 * ************************************************************************** */

/* Compute a tagged boolean, #t iff the two given arguments are
   equal-by-identity. */
#define JITTERLISP_EQP_(_jitterlisp_out, _jitterlisp_in0, _jitterlisp_in1)  \
  JITTER_BEGIN_                                                             \
    _jitterlisp_out =                                                       \
      JITTERLISP_BOOLEAN_ENCODE((_jitterlisp_in0) == (_jitterlisp_in1));    \
  JITTER_END_

/* Compute a tagged boolean, #t iff the two given arguments are
   not equal-by-identity. */
#define JITTERLISP_NEQP_(_jitterlisp_out, _jitterlisp_in0, _jitterlisp_in1)  \
  JITTER_BEGIN_                                                              \
    _jitterlisp_out =                                                        \
      JITTERLISP_BOOLEAN_ENCODE((_jitterlisp_in0) != (_jitterlisp_in1));     \
  JITTER_END_

/* Compute a tagged boolean, #t iff the given argument is the fixnum zero. */
#define JITTERLISP_ZEROP_(_jitterlisp_out, _jitterlisp_in0)       \
  JITTER_BEGIN_                                                   \
    _jitterlisp_out =                                             \
      JITTERLISP_BOOLEAN_ENCODE((_jitterlisp_in0)                 \
                                == JITTERLISP_FIXNUM_ENCODE(0));  \
  JITTER_END_

/* Compute a tagged boolean, #t iff the given argument is different from the
   fixnum zero. */
#define JITTERLISP_NON_ZEROP_(_jitterlisp_out, _jitterlisp_in0)   \
  JITTER_BEGIN_                                                   \
    _jitterlisp_out =                                             \
      JITTERLISP_BOOLEAN_ENCODE((_jitterlisp_in0)                 \
                                != JITTERLISP_FIXNUM_ENCODE(0));  \
  JITTER_END_




/* Type checking operations.
 * ************************************************************************** */

/* Compute a tagged boolean, #t iff the given in-argument is () . */
#define JITTERLISP_NULLP_(_jitterlisp_out, _jitterlisp_in0)                  \
  JITTER_BEGIN_                                                              \
    _jitterlisp_out =                                                        \
      JITTERLISP_BOOLEAN_ENCODE(JITTERLISP_IS_EMPTY_LIST(_jitterlisp_in0));  \
  JITTER_END_

/* Compute a tagged boolean, #t iff the given in-argument is not () . */
#define JITTERLISP_NON_NULLP_(_jitterlisp_out, _jitterlisp_in0)                \
  JITTER_BEGIN_                                                                \
    _jitterlisp_out =                                                          \
      JITTERLISP_BOOLEAN_ENCODE(! JITTERLISP_IS_EMPTY_LIST(_jitterlisp_in0));  \
  JITTER_END_

/* Compute a tagged boolean, #t iff the given in-argument is a fixnum . */
#define JITTERLISP_FIXNUMP_(_jitterlisp_out, _jitterlisp_in0)            \
  JITTER_BEGIN_                                                          \
    _jitterlisp_out =                                                    \
      JITTERLISP_BOOLEAN_ENCODE(JITTERLISP_IS_FIXNUM(_jitterlisp_in0));  \
  JITTER_END_

/* Compute a tagged boolean, #t iff the given in-argument is a number . */
#define JITTERLISP_NUMBERP_(_jitterlisp_out, _jitterlisp_in0)            \
  JITTER_BEGIN_                                                          \
    _jitterlisp_out =                                                    \
      (JITTERLISP_BOOLEAN_ENCODE(JITTERLISP_IS_FIXNUM(_jitterlisp_in0)   \
       /* || ... There are no other numbers yet. */));                   \
  JITTER_END_

/* Compute a tagged boolean, #t iff the given in-argument is a character . */
#define JITTERLISP_CHARACTERP_(_jitterlisp_out, _jitterlisp_in0)            \
  JITTER_BEGIN_                                                             \
    _jitterlisp_out =                                                       \
      JITTERLISP_BOOLEAN_ENCODE(JITTERLISP_IS_CHARACTER(_jitterlisp_in0));  \
  JITTER_END_

/* Compute a tagged boolean, #t iff the given in-argument is a boolean . */
#define JITTERLISP_BOOLEANP_(_jitterlisp_out, _jitterlisp_in0)            \
  JITTER_BEGIN_                                                           \
    _jitterlisp_out =                                                     \
      JITTERLISP_BOOLEAN_ENCODE(JITTERLISP_IS_BOOLEAN(_jitterlisp_in0));  \
  JITTER_END_

/* Compute a tagged boolean, #t iff the given in-argument is the eof object . */
#define JITTERLISP_EOFP_(_jitterlisp_out, _jitterlisp_in0)            \
  JITTER_BEGIN_                                                           \
    _jitterlisp_out =                                                     \
      JITTERLISP_BOOLEAN_ENCODE(JITTERLISP_IS_EOF(_jitterlisp_in0));  \
  JITTER_END_

/* Compute a tagged boolean, #t iff the given in-argument is the nothing
   object. */
#define JITTERLISP_NOTHINGP_(_jitterlisp_out, _jitterlisp_in0)            \
  JITTER_BEGIN_                                                           \
    _jitterlisp_out =                                                     \
      JITTERLISP_BOOLEAN_ENCODE(JITTERLISP_IS_NOTHING(_jitterlisp_in0));  \
  JITTER_END_

/* Compute a tagged boolean, #t iff the given in-argument is the undefined
   object. */
#define JITTERLISP_UNDEFINEDP_(_jitterlisp_out, _jitterlisp_in0)            \
  JITTER_BEGIN_                                                           \
    _jitterlisp_out =                                                     \
      JITTERLISP_BOOLEAN_ENCODE(JITTERLISP_IS_UNDEFINED(_jitterlisp_in0));  \
  JITTER_END_

/* Compute a tagged boolean, #t iff the given in-argument is a cons . */
#define JITTERLISP_CONSP_(_jitterlisp_out, _jitterlisp_in0)            \
  JITTER_BEGIN_                                                        \
    _jitterlisp_out =                                                  \
      JITTERLISP_BOOLEAN_ENCODE(JITTERLISP_IS_CONS(_jitterlisp_in0));  \
  JITTER_END_
/* Compute a tagged boolean, #t iff the given in-argument is not a cons . */
#define JITTERLISP_NON_CONSP_(_jitterlisp_out, _jitterlisp_in0)          \
  JITTER_BEGIN_                                                          \
    _jitterlisp_out =                                                    \
      JITTERLISP_BOOLEAN_ENCODE(! JITTERLISP_IS_CONS(_jitterlisp_in0));  \
  JITTER_END_

/* Compute a tagged boolean, #t iff the given in-argument is a symbol . */
#define JITTERLISP_SYMBOLP_(_jitterlisp_out, _jitterlisp_in0)            \
  JITTER_BEGIN_                                                          \
    _jitterlisp_out =                                                    \
      JITTERLISP_BOOLEAN_ENCODE(JITTERLISP_IS_SYMBOL(_jitterlisp_in0));  \
  JITTER_END_
/* Compute a tagged boolean, #t iff the given in-argument is not a symbol . */
#define JITTERLISP_NON_SYMBOLP_(_jitterlisp_out, _jitterlisp_in0)          \
  JITTER_BEGIN_                                                            \
    _jitterlisp_out =                                                      \
      JITTERLISP_BOOLEAN_ENCODE(! JITTERLISP_IS_SYMBOL(_jitterlisp_in0));  \
  JITTER_END_

/* Compute a tagged boolean, #t iff the given in-argument is a procedure; this
   doesn't distinguish between primitive procedures and procedure closures. */
#define JITTERLISP_PROCEDUREP_(_jitterlisp_out, _jitterlisp_in0)            \
  JITTER_BEGIN_                                                             \
    _jitterlisp_out =                                                       \
      JITTERLISP_BOOLEAN_ENCODE(JITTERLISP_IS_PROCEDURE(_jitterlisp_in0));  \
  JITTER_END_

/* Compute a tagged boolean, #t iff the given in-argument is a procedure; this
   doesn't distinguish between primitive procedures and procedure closures. */
#define JITTERLISP_PROCEDUREP_(_jitterlisp_out, _jitterlisp_in0)            \
  JITTER_BEGIN_                                                             \
    _jitterlisp_out =                                                       \
      JITTERLISP_BOOLEAN_ENCODE(JITTERLISP_IS_PROCEDURE(_jitterlisp_in0));  \
  JITTER_END_

/* Compute a tagged boolean, #t iff the given in-argument is a macro; this
   doesn't distinguish between primitive macros and macro closures. */
#define JITTERLISP_MACROP_(_jitterlisp_out, _jitterlisp_in0)            \
  JITTER_BEGIN_                                                         \
    _jitterlisp_out =                                                   \
      JITTERLISP_BOOLEAN_ENCODE(JITTERLISP_IS_MACRO(_jitterlisp_in0));  \
  JITTER_END_

/* Compute a tagged boolean, #t iff the given in-argument is an AST; this
   doesn't distinguish between primitive asts and ast closures. */
#define JITTERLISP_ASTP_(_jitterlisp_out, _jitterlisp_in0)            \
  JITTER_BEGIN_                                                       \
    _jitterlisp_out =                                                 \
      JITTERLISP_BOOLEAN_ENCODE(JITTERLISP_IS_AST(_jitterlisp_in0));  \
  JITTER_END_

/* Compute a tagged boolean, #t iff the given in-argument is a vector . */
#define JITTERLISP_VECTORP_(_jitterlisp_out, _jitterlisp_in0)            \
  JITTER_BEGIN_                                                          \
    _jitterlisp_out =                                                    \
      JITTERLISP_BOOLEAN_ENCODE(JITTERLISP_IS_VECTOR(_jitterlisp_in0));  \
  JITTER_END_




/* I/O operations.
 * ************************************************************************** */

#define JITTERLISP_DISPLAY_(_jitterlisp_out, _jitterlisp_in0)  \
  JITTER_BEGIN_                                                \
    jitterlisp_print_to_stream (stdout, _jitterlisp_in0);      \
    _jitterlisp_out = JITTERLISP_NOTHING;                      \
  JITTER_END_

#define JITTERLISP_NEWLINE_(_jitterlisp_out)  \
  JITTER_BEGIN_                               \
    putchar ('\n');                           \
    _jitterlisp_out = JITTERLISP_NOTHING;     \
  JITTER_END_

#define JITTERLISP_READ_(_jitterlisp_out)                   \
  JITTER_BEGIN_                                             \
    _jitterlisp_out = jitterlisp_read_readline_one ("> ");  \
  JITTER_END_




/* Interpretation operations.
 * ************************************************************************** */

/* Macroexpand the given expression in the given non-global expansion-time
   environment. */
#define JITTERLISP_MACROEXPAND_(_jitterlisp_out, _jitterlisp_in0,  \
                                _jitterlisp_in1)                   \
  JITTER_BEGIN_                                                    \
    _jitterlisp_out = jitterlisp_macroexpand (_jitterlisp_in0,     \
                                              _jitterlisp_in1);    \
  JITTER_END_

/* Eval the given expression in the given non-global environment. */
#define JITTERLISP_EVAL_(_jitterlisp_out, _jitterlisp_in0, _jitterlisp_in1)  \
  JITTER_BEGIN_                                                              \
    _jitterlisp_out = jitterlisp_eval_interpreter (_jitterlisp_in0,          \
                                                   _jitterlisp_in1);         \
  JITTER_END_




#endif // #ifndef JITTERLISP_OPERATIONS_H_
