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




/* Cons expression operations.
 * ************************************************************************** */

/* Cons selectors operations, including the ones for composed selectors, are
   always definable as expressions as they don't require heap allocation.  (That
   is not the case for cons construction.) */

/* Expand to an r-value expression evaluating to the tagged car or cdr of the
   given tagged cons operand. */
#define JITTERLISP_EXP_C___CAR(_jitterlisp_tagged_cons)   \
  (JITTERLISP_CONS_DECODE(_jitterlisp_tagged_cons)->car)
#define JITTERLISP_EXP_C___CDR(_jitterlisp_tagged_cons)   \
  (JITTERLISP_CONS_DECODE(_jitterlisp_tagged_cons)->cdr)

/* Composed cons selectors.  Given a tagged value expand to an expression
   evaluating to its tagged caar, cadr, and so on.  These are compositions of
   JITTERLISP_EXP_C___CAR and JITTERLISP_EXP_C___CDR . */
/* Length 2. */
#define JITTERLISP_EXP_C___CAAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CAR(JITTERLISP_EXP_C___CAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CADR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CAR(JITTERLISP_EXP_C___CDR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CDAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CDR(JITTERLISP_EXP_C___CAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CDDR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CDR(JITTERLISP_EXP_C___CDR(_jitterlisp_tagged_cons))
/* Length 3. */
#define JITTERLISP_EXP_C___CAAAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CAR(JITTERLISP_EXP_C___CAAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CAADR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CAR(JITTERLISP_EXP_C___CADR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CADAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CAR(JITTERLISP_EXP_C___CDAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CADDR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CAR(JITTERLISP_EXP_C___CDDR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CDAAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CDR(JITTERLISP_EXP_C___CAAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CDADR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CDR(JITTERLISP_EXP_C___CADR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CDDAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CDR(JITTERLISP_EXP_C___CDAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CDDDR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CDR(JITTERLISP_EXP_C___CDDR(_jitterlisp_tagged_cons))
/* Length 4. */
#define JITTERLISP_EXP_C___CAAAAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CAR(JITTERLISP_EXP_C___CAAAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CAAADR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CAR(JITTERLISP_EXP_C___CAADR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CAADAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CAR(JITTERLISP_EXP_C___CADAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CAADDR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CAR(JITTERLISP_EXP_C___CADDR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CADAAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CAR(JITTERLISP_EXP_C___CDAAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CADADR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CAR(JITTERLISP_EXP_C___CDADR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CADDAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CAR(JITTERLISP_EXP_C___CDDAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CADDDR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CAR(JITTERLISP_EXP_C___CDDDR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CDAAAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CDR(JITTERLISP_EXP_C___CAAAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CDAADR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CDR(JITTERLISP_EXP_C___CAADR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CDADAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CDR(JITTERLISP_EXP_C___CADAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CDADDR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CDR(JITTERLISP_EXP_C___CADDR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CDDAAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CDR(JITTERLISP_EXP_C___CDAAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CDDADR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CDR(JITTERLISP_EXP_C___CDADR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CDDDAR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CDR(JITTERLISP_EXP_C___CDDAR(_jitterlisp_tagged_cons))
#define JITTERLISP_EXP_C___CDDDDR(_jitterlisp_tagged_cons)                   \
  JITTERLISP_EXP_C___CDR(JITTERLISP_EXP_C___CDDDR(_jitterlisp_tagged_cons))




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




/* Fixnums-to-boolean operations.
 * ************************************************************************** */

// FIXME: implement.




/* Cons operations.
 * ************************************************************************** */

// FIXME: comment.

#define JITTERLISP_CONS_(_jitterlisp_out, _jitterlisp_in0, _jitterlisp_in1)  \
  JITTER_BEGIN_                                                              \
    struct jitterlisp_cons *_jitterlisp_tmp                                  \
      = JITTERLISP_CONS_MAKE_UNINITIALIZED_UNENCODED();                      \
    _jitterlisp_tmp->car = _jitterlisp_in0;                                  \
    _jitterlisp_tmp->cdr = _jitterlisp_in1;                                  \
    _jitterlisp_out = JITTERLISP_CONS_ENCODE(_jitterlisp_tmp);               \
  JITTER_END_

#define JITTERLISP_CAR_(_jitterlisp_out, _jitterlisp_in0)  \
  JITTER_BEGIN_                                            \
    struct jitterlisp_cons *_jitterlisp_tmp                \
      = JITTERLISP_CONS_DECODE(_jitterlisp_in0);           \
    _jitterlisp_out = _jitterlisp_tmp->car;                \
  JITTER_END_

#define JITTERLISP_CDR_(_jitterlisp_out, _jitterlisp_in0)  \
  JITTER_BEGIN_                                            \
    struct jitterlisp_cons *_jitterlisp_tmp                \
      = JITTERLISP_CONS_DECODE(_jitterlisp_in0);           \
    _jitterlisp_out = _jitterlisp_tmp->cdr;                \
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
    _jitterlisp_out = JITTERLISP_CLOSURE_ENCODE(_jitterlisp_tmp);  \
  JITTER_END_




/* Unique-object operations.
 * ************************************************************************** */

/* Compute a tagged boolean, #t iff the given in-argument is () . */
#define JITTERLISP_NULLP_(_jitterlisp_out, _jitterlisp_in0)                  \
  JITTER_BEGIN_                                                              \
    _jitterlisp_out =                                                        \
      JITTERLISP_BOOLEAN_ENCODE(JITTERLISP_IS_EMPTY_LIST(_jitterlisp_in0));  \
  JITTER_END_




#endif // #ifndef JITTERLISP_OPERATIONS_H_
