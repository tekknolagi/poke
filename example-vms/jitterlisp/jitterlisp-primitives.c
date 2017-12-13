/* Jittery Lisp: primitives.

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


#ifndef JITTERLISP_PRIMITIVES_H_
#define JITTERLISP_PRIMITIVES_H_

#include "jitterlisp-primitives.h"

#include <jitter/jitter-cpp.h>

#include "jitterlisp.h"




/* Primitive function definition infrastructure.
 * ************************************************************************** */

/* The prefix of C primitive function names. */
#define JITTERLISP_PRIMITIVE_C_FUNCTION_NAME_PREFIX  \
  jitterlisp_primitive_c_function_
#define JITTERLISP_PRIMITIVE_C_IN_ARITY_NAME_PREFIX  \
  jitterlisp_primitive_in_arity_

/* Expand to the full name of a primitive C function, given its suffix. */
#define JITTERLISP_PRIMITIVE_C_FUNCTION_NAME(_jitterlisp_c_name_suffix)  \
  JITTER_CONCATENATE_TWO(JITTERLISP_PRIMITIVE_C_FUNCTION_NAME_PREFIX,    \
                         _jitterlisp_c_name_suffix)

/* Expand to the full name of a global variable holding a primitive in-arity,
   given its suffix. */
#define JITTERLISP_PRIMITIVE_C_IN_ARITY_NAME(_jitterlisp_c_name_suffix)  \
  JITTER_CONCATENATE_TWO(JITTERLISP_PRIMITIVE_C_IN_ARITY_NAME_PREFIX,    \
                         _jitterlisp_c_name_suffix)

/* Expand to a C function definition, given its suffix name and its body.  The
   body can access actual pre-evaluated arguments thru the variable const
   jitterlisp_object *args , and can assign its result to jitterlisp_object res
   .  The variable res is automatically initialized to #<nothing> for
   convenience.
   This is a building block for the JITTERLISP_PRIMITIVE_FUNCTION_?_ macros
   below. */
#define JITTERLISP_PRIMITIVE_FUNCTION_(_jitterlisp_name_suffix,           \
                                       _jitterlisp_type_check_statement,  \
                                       _jitterlisp_body_statement)        \
  static jitterlisp_object                                                \
  JITTERLISP_PRIMITIVE_C_FUNCTION_NAME(_jitterlisp_name_suffix)           \
     (const jitterlisp_object *args)                                      \
  {                                                                       \
    const char *_jitterlisp_the_name_suffix                               \
      __attribute__ ((unused))                                            \
      = JITTER_STRINGIFY(_jitterlisp_name_suffix);                        \
    jitterlisp_object res = JITTERLISP_NOTHING;                           \
    const jitterlisp_object *_jitterlisp_next_arg                         \
      __attribute__ ((unused))                                            \
      = args;                                                             \
    JITTER_BEGIN_                                                         \
      _jitterlisp_type_check_statement;                                   \
    JITTER_END_;                                                          \
    JITTER_BEGIN_                                                         \
      _jitterlisp_body_statement;                                         \
    JITTER_END_;                                                          \
    return res;                                                           \
  }

/* Expand to a statement suitable for the _jitterlisp_type_check_statement
   argument of JITTERLISP_PRIMITIVE_FUNCTION_ above.  Check that the next
   argument has the given type (which is allowed to be ANYTHING , if any
   object is accepted); error out on type error, and simply advance
   the next-argument pointer otherwise. */
#define JITTERLISP_CHECK_TYPE(_jitterlisp_type_suffix)                      \
  JITTER_BEGIN_                                                             \
    /* Error out if the next argument doesn't have the required type. */    \
    if (! JITTER_CONCATENATE_TWO(JITTERLISP_IS_, _jitterlisp_type_suffix)(  \
             * _jitterlisp_next_arg))                                       \
      {                                                                     \
        /* FIXME: integrate into jitter_error_cloned. */                    \
        printf ("About the %i-th (0-based) actual for %s:\n",               \
                (int) (_jitterlisp_next_arg - args),                        \
                _jitterlisp_the_name_suffix);                               \
        jitterlisp_error_cloned ("invalid type argument for primitive");    \
      }                                                                     \
    /* Increment the next-argumnent pointer so that the next type check */  \
    /* affects the next argument. */                                        \
    _jitterlisp_next_arg ++;                                                \
  JITTER_END_

/* Expand to a C function definition for a 0-ary primitive, using
   JITTERLISP_PRIMITIVE_FUNCTION_ . */
#define JITTERLISP_PRIMITIVE_FUNCTION_0_(_jitterlisp_name_suffix,     \
                                         _jitterlisp_body_statement)  \
  JITTERLISP_PRIMITIVE_FUNCTION_(_jitterlisp_name_suffix,             \
                                 {},                                  \
                                 _jitterlisp_body_statement)

/* Expand to a C function definition for a 1-ary, 2-ary and so on primitive
   using JITTERLISP_PRIMITIVE_FUNCTION_ with a type-checking statement verifying
   that the argument have the given type.  The given types are allowed to be
   ANYTHING when no type checking needs to be performed. */
#define JITTERLISP_PRIMITIVE_FUNCTION_1_(_jitterlisp_name_suffix,              \
                                         _jitterlisp_type_0,                   \
                                         _jitterlisp_body_statement)           \
  JITTERLISP_PRIMITIVE_FUNCTION_(_jitterlisp_name_suffix,                      \
                                 {                                             \
                                   JITTERLISP_CHECK_TYPE(_jitterlisp_type_0);  \
                                 },                                            \
                                 _jitterlisp_body_statement)
#define JITTERLISP_PRIMITIVE_FUNCTION_2_(_jitterlisp_name_suffix,              \
                                         _jitterlisp_type_0,                   \
                                         _jitterlisp_type_1,                   \
                                         _jitterlisp_body_statement)           \
  JITTERLISP_PRIMITIVE_FUNCTION_(_jitterlisp_name_suffix,                      \
                                 {                                             \
                                   JITTERLISP_CHECK_TYPE(_jitterlisp_type_0);  \
                                   JITTERLISP_CHECK_TYPE(_jitterlisp_type_1);  \
                                 },                                            \
                                 _jitterlisp_body_statement)
#define JITTERLISP_PRIMITIVE_FUNCTION_3_(_jitterlisp_name_suffix,              \
                                         _jitterlisp_type_0,                   \
                                         _jitterlisp_type_1,                   \
                                         _jitterlisp_type_2,                   \
                                         _jitterlisp_body_statement)           \
  JITTERLISP_PRIMITIVE_FUNCTION_(_jitterlisp_name_suffix,                      \
                                 {                                             \
                                   JITTERLISP_CHECK_TYPE(_jitterlisp_type_0);  \
                                   JITTERLISP_CHECK_TYPE(_jitterlisp_type_1);  \
                                   JITTERLISP_CHECK_TYPE(_jitterlisp_type_2);  \
                                 },                                            \
                                 _jitterlisp_body_statement)
#define JITTERLISP_PRIMITIVE_FUNCTION_4_(_jitterlisp_name_suffix,              \
                                         _jitterlisp_type_0,                   \
                                         _jitterlisp_type_1,                   \
                                         _jitterlisp_type_2,                   \
                                         _jitterlisp_type_3,                   \
                                         _jitterlisp_body_statement)           \
  JITTERLISP_PRIMITIVE_FUNCTION_(_jitterlisp_name_suffix,                      \
                                 {                                             \
                                   JITTERLISP_CHECK_TYPE(_jitterlisp_type_0);  \
                                   JITTERLISP_CHECK_TYPE(_jitterlisp_type_1);  \
                                   JITTERLISP_CHECK_TYPE(_jitterlisp_type_2);  \
                                   JITTERLISP_CHECK_TYPE(_jitterlisp_type_3);  \
                                 },                                            \
                                 _jitterlisp_body_statement)




/* Primitive descriptor definition instrastructure.
 * ************************************************************************** */

/* Expand to a constant initializer for a struct jitterlisp_primitive including
   the given Lisp name, the given in-arity and the given suffix for a C
   function. */
#define JITTERLISP_PRIMITIVE_STRUCT_(_jitterlisp_lisp_name,        \
                                     _jitterlisp_in_arity,         \
                                     _jitterlisp_name_suffix)      \
  {                                                                \
    (_jitterlisp_lisp_name),                                       \
    ((jitter_uint) (_jitterlisp_in_arity)),                        \
    JITTERLISP_PRIMITIVE_C_FUNCTION_NAME(_jitterlisp_name_suffix)  \
  }




/* Primitive function definitions.
 * ************************************************************************** */

/* Define every primitive function. */

/* Type checking. */
JITTERLISP_PRIMITIVE_FUNCTION_1_(fixnump, ANYTHING,
  { JITTERLISP_FIXNUMP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(characterp, ANYTHING,
  { JITTERLISP_CHARACTERP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(nullp, ANYTHING,
  { JITTERLISP_NULLP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(non_nullp, ANYTHING,
  { JITTERLISP_NON_NULLP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(eofp, ANYTHING,
  { JITTERLISP_EOFP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(booleanp, ANYTHING,
  { JITTERLISP_BOOLEANP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(nothingp, ANYTHING,
  { JITTERLISP_NOTHINGP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(symbolp, ANYTHING,
  { JITTERLISP_SYMBOLP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(non_symbolp, ANYTHING,
  { JITTERLISP_NON_SYMBOLP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(consp, ANYTHING,
  { JITTERLISP_CONSP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(non_consp, ANYTHING,
  { JITTERLISP_NON_CONSP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(procedurep, ANYTHING,
  { JITTERLISP_PROCEDUREP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(vectorp, ANYTHING,
  { JITTERLISP_VECTORP_(res, args [0]); })
/* Arithmetic */
JITTERLISP_PRIMITIVE_FUNCTION_2_(plus, FIXNUM, FIXNUM,
  { JITTERLISP_PLUS_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(minus, FIXNUM, FIXNUM,
  { JITTERLISP_MINUS_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(times, FIXNUM, FIXNUM,
  { JITTERLISP_TIMES_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(divided, FIXNUM, FIXNUM,
  { JITTERLISP_DIVIDED_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(quotient, FIXNUM, FIXNUM,
  { JITTERLISP_QUOTIENT_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(remainder, FIXNUM, FIXNUM,
  { JITTERLISP_REMAINDER_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(one_plus, FIXNUM,
  { JITTERLISP_1PLUS_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(one_minus, FIXNUM,
  { JITTERLISP_1MINUS_(res, args [0]); })
/* Boolean operations. */
JITTERLISP_PRIMITIVE_FUNCTION_1_(not, ANYTHING,
  { JITTERLISP_NOT_(res, args [0]); })
/* Number comparison. */
JITTERLISP_PRIMITIVE_FUNCTION_2_(equals, FIXNUM, FIXNUM,
  { JITTERLISP_EQP_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(different, FIXNUM, FIXNUM,
  { JITTERLISP_NEQP_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(less, FIXNUM, FIXNUM,
  { JITTERLISP_LESSP_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(notgreater, FIXNUM, FIXNUM,
  { JITTERLISP_NOTGREATERP_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(greater, FIXNUM, FIXNUM,
  { JITTERLISP_GREATERP_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(notless, FIXNUM, FIXNUM,
  { JITTERLISP_NOTLESSP_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(zerop, FIXNUM,
  { JITTERLISP_ZEROP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(nzerop, FIXNUM,
  { JITTERLISP_NZEROP_(res, args [0]); })
/* Comparison. */
JITTERLISP_PRIMITIVE_FUNCTION_2_(eqp, ANYTHING, ANYTHING,
  { JITTERLISP_EQP_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(neqp, ANYTHING, ANYTHING,
  { JITTERLISP_NEQP_(res, args [0], args [1]); })
/* Cons operations. */
JITTERLISP_PRIMITIVE_FUNCTION_2_(cons, ANYTHING, ANYTHING,
  { JITTERLISP_CONS_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(car, CONS,
  { JITTERLISP_CAR_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(cdr, CONS,
  { JITTERLISP_CDR_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(set_car_b, CONS, ANYTHING,
  { JITTERLISP_SET_CARB_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(set_cdr_b, CONS, ANYTHING,
  { JITTERLISP_SET_CDRB_(res, args [0], args [1]); })
/* Symbol operations. */
JITTERLISP_PRIMITIVE_FUNCTION_0_(gensym,
  { JITTERLISP_GENSYM_(res); })
/* Vector operations. */
JITTERLISP_PRIMITIVE_FUNCTION_2_(make_vector, FIXNUM, ANYTHING,
  { JITTERLISP_VECTOR_MAKE_(res, args [0], args [1]); })
/* I/O operations. */
JITTERLISP_PRIMITIVE_FUNCTION_1_(display, ANYTHING,
  { JITTERLISP_DISPLAY_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_0_(newline,
  { JITTERLISP_NEWLINE_(res); })
JITTERLISP_PRIMITIVE_FUNCTION_0_(read,
  { JITTERLISP_READ_(res); })
/* Interpretation operations. */
JITTERLISP_PRIMITIVE_FUNCTION_2_(eval, ANYTHING, ANYTHING,
  { JITTERLISP_EVAL_(res, args [0], args [1]); })




/* Primitive descriptor definitions.
 * ************************************************************************** */

/* Define every primitive descriptor in a global constant array. */
static const struct jitterlisp_primitive
jitterlisp_primitives []
  = {
      /* Type checking. */
      JITTERLISP_PRIMITIVE_STRUCT_("fixnum?", 1, fixnump),
      JITTERLISP_PRIMITIVE_STRUCT_("character?", 1, characterp),
      JITTERLISP_PRIMITIVE_STRUCT_("null?", 1, nullp),
      JITTERLISP_PRIMITIVE_STRUCT_("non-null?", 1, non_nullp),
      JITTERLISP_PRIMITIVE_STRUCT_("eof?", 1, eofp),
      JITTERLISP_PRIMITIVE_STRUCT_("boolean?", 1, booleanp),
      JITTERLISP_PRIMITIVE_STRUCT_("nothing?", 1, nothingp),
      JITTERLISP_PRIMITIVE_STRUCT_("symbol?", 1, symbolp),
      JITTERLISP_PRIMITIVE_STRUCT_("non-symbol?", 1, non_symbolp),
      JITTERLISP_PRIMITIVE_STRUCT_("cons?", 1, consp),
      JITTERLISP_PRIMITIVE_STRUCT_("non-cons?", 1, non_consp),
      JITTERLISP_PRIMITIVE_STRUCT_("procedure?", 1, procedurep),
      JITTERLISP_PRIMITIVE_STRUCT_("vector?", 1, vectorp),
      /* Arithmetic. */
      JITTERLISP_PRIMITIVE_STRUCT_("+", 2, plus),
      JITTERLISP_PRIMITIVE_STRUCT_("-", 2, minus),
      JITTERLISP_PRIMITIVE_STRUCT_("*", 2, times),
      JITTERLISP_PRIMITIVE_STRUCT_("/", 2, divided),
      JITTERLISP_PRIMITIVE_STRUCT_("quotient", 2, quotient),
      JITTERLISP_PRIMITIVE_STRUCT_("remainder", 2, remainder),
      JITTERLISP_PRIMITIVE_STRUCT_("1+", 1, one_plus),
      JITTERLISP_PRIMITIVE_STRUCT_("1-", 1, one_minus),
      /* Boolean operations. */
      JITTERLISP_PRIMITIVE_STRUCT_("not", 1, not),
      /* Number comparison. */
      JITTERLISP_PRIMITIVE_STRUCT_("=", 2, equals),
      JITTERLISP_PRIMITIVE_STRUCT_("<>", 2, different),
      JITTERLISP_PRIMITIVE_STRUCT_("<", 2, less),
      JITTERLISP_PRIMITIVE_STRUCT_("<=", 2, notgreater),
      JITTERLISP_PRIMITIVE_STRUCT_(">", 2, greater),
      JITTERLISP_PRIMITIVE_STRUCT_(">=", 2, notless),
      JITTERLISP_PRIMITIVE_STRUCT_("zero?", 1, zerop),
      JITTERLISP_PRIMITIVE_STRUCT_("nzero?", 1, nzerop),
      /* Comparison. */
      JITTERLISP_PRIMITIVE_STRUCT_("eq?", 2, eqp),
      JITTERLISP_PRIMITIVE_STRUCT_("neq?", 2, neqp),
      /* Cons operations. */
      JITTERLISP_PRIMITIVE_STRUCT_("cons", 2, cons),
      JITTERLISP_PRIMITIVE_STRUCT_("car", 1, car),
      JITTERLISP_PRIMITIVE_STRUCT_("cdr", 1, cdr),
      JITTERLISP_PRIMITIVE_STRUCT_("set-car!", 2, set_car_b),
      JITTERLISP_PRIMITIVE_STRUCT_("set-cdr!", 2, set_cdr_b),
      /* Symbol operations. */
      JITTERLISP_PRIMITIVE_STRUCT_("gensym", 0, gensym),
      /* Vector operations. */
      JITTERLISP_PRIMITIVE_STRUCT_("make-vector", 2, make_vector),
      /* I/O operations. */
      JITTERLISP_PRIMITIVE_STRUCT_("display", 1, display),
      JITTERLISP_PRIMITIVE_STRUCT_("newline", 0, newline),
      JITTERLISP_PRIMITIVE_STRUCT_("read", 0, read),
      /* Interpretation operations. */
      JITTERLISP_PRIMITIVE_STRUCT_("eval", 2, eval)
    };

/* How many primitive descriptors there are. */
static const size_t
jitterlisp_primitive_no
  = sizeof (jitterlisp_primitives) / sizeof (struct jitterlisp_primitive);




/* Initialization and finalization of the primitives subsystem.
 * ************************************************************************** */

void
jitterlisp_primitives_initialize (void)
{
  int i;
  for (i = 0; i < jitterlisp_primitive_no; i ++)
    {
      struct jitterlisp_symbol *symbol_object
        = jitterlisp_symbol_make_interned (jitterlisp_primitives [i].name);
      symbol_object->global_value
        = JITTERLISP_PRIMITIVE_ENCODE(jitterlisp_primitives + i);
    }
}

void
jitterlisp_primitives_finalize (void)
{
  /* Do nothing.  Interned symbols are destroyed by the memory subsystem
     finalization function, and primitive descriptors are global constants. */
}

#endif // #ifndef JITTERLISP_PRIMITIVES_H_
