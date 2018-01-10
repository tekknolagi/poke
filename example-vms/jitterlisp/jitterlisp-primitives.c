/* Jittery Lisp: primitives.

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


#ifndef JITTERLISP_PRIMITIVES_H_
#define JITTERLISP_PRIMITIVES_H_

#include "jitterlisp-primitives.h"

#include <string.h>

#include <jitter/jitter-cpp.h>
#include <jitter/jitter-malloc.h>

#include "jitterlisp.h"
#include "jitterlisp-ast.h"




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
        printf ("About the %i-th (0-based) actual for %s  ",                \
                (int) (_jitterlisp_next_arg - args),                        \
                _jitterlisp_the_name_suffix);                               \
        jitterlisp_print_to_stream (stdout, * _jitterlisp_next_arg);        \
        printf (":\n");                                                     \
        jitterlisp_error_cloned ("invalid argument type for primitive "     \
                                 "(not "                                    \
                                 JITTER_STRINGIFY(_jitterlisp_type_suffix)  \
                                 ")");                                      \
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

/* Expand to a C function definition for a primitive macro function.  This is
   simpler than JITTERLISP_PRIMITIVE_FUNCTION_?_ because primitive macros always
   have two argument without type restriction, and can be built as simple
   wrappers given only the C name. */
#define JITTERLISP_PRIMITIVE_MACRO_FUNCTION_(_jitterlisp_name_suffix)      \
  JITTERLISP_PRIMITIVE_FUNCTION_(                                          \
     _jitterlisp_name_suffix,                                              \
     {                                                                     \
       JITTERLISP_CHECK_TYPE(ANYTHING);                                    \
       JITTERLISP_CHECK_TYPE(ANYTHING);                                    \
     },                                                                    \
     {                                                                     \
       res = JITTER_CONCATENATE_TWO(jitterlisp_primitive_macro_function_,  \
                                    _jitterlisp_name_suffix)               \
                (args [0], args [1]);                                      \
     })




/* Primitive descriptor definition instrastructure.
 * ************************************************************************** */

/* Expand to a constant initializer for a struct jitterlisp_primitive including
   the given Lisp name, the given in-arity and the given suffix for a C
   function.  The generated descriptor is for a primitive procedure iff
   _jitterlisp_procedure is non-false. */
#define JITTERLISP_PRIMITIVE_PROCEDURE_OR_MACRO_STRUCT_(           \
           _jitterlisp_lisp_name,                                  \
           _jitterlisp_in_arity,                                   \
           _jitterlisp_name_suffix,                                \
           _jitterlisp_procedure)                                  \
  {                                                                \
    (_jitterlisp_lisp_name),                                       \
    ((jitter_uint) (_jitterlisp_in_arity)),                        \
    (_jitterlisp_procedure),                                       \
    JITTERLISP_PRIMITIVE_C_FUNCTION_NAME(_jitterlisp_name_suffix)  \
  }

/* Like JITTERLISP_PRIMITIVE_PROCEDURE_OR_MACRO_STRUCT_ without the
   _jitterlisp_procedure argument, always expanding to a primitive procedure
   descriptor. */
#define JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_(_jitterlisp_lisp_name,    \
                                               _jitterlisp_in_arity,     \
                                               _jitterlisp_name_suffix)  \
  JITTERLISP_PRIMITIVE_PROCEDURE_OR_MACRO_STRUCT_(                       \
     _jitterlisp_lisp_name,                                              \
     _jitterlisp_in_arity,                                               \
     _jitterlisp_name_suffix,                                            \
     true)

/* Like JITTERLISP_PRIMITIVE_PROCEDURE_OR_MACRO_STRUCT_ without the
   _jitterlisp_procedure or _jitterlisp_in_arity argument, always expanding to a
   primitive macro descriptor. */
#define JITTERLISP_PRIMITIVE_MACRO_STRUCT_(_jitterlisp_lisp_name,    \
                                           _jitterlisp_name_suffix)  \
  JITTERLISP_PRIMITIVE_PROCEDURE_OR_MACRO_STRUCT_(                   \
     _jitterlisp_lisp_name,                                          \
     2,                                                              \
     _jitterlisp_name_suffix,                                        \
     false)




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
JITTERLISP_PRIMITIVE_FUNCTION_1_(undefinedp, ANYTHING,
  { JITTERLISP_UNDEFINEDP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(symbolp, ANYTHING,
  { JITTERLISP_SYMBOLP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(non_symbolp, ANYTHING,
  { JITTERLISP_NON_SYMBOLP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(consp, ANYTHING,
  { JITTERLISP_CONSP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(non_consp, ANYTHING,
  { JITTERLISP_NON_CONSP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(closurep, ANYTHING,
  { JITTERLISP_CLOSUREP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(primitivep, ANYTHING,
  { JITTERLISP_PRIMITIVEP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(astp, ANYTHING,
  { JITTERLISP_ASTP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(macrop, ANYTHING,
  { JITTERLISP_MACROP_(res, args [0]); })
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
JITTERLISP_PRIMITIVE_FUNCTION_1_(negate, FIXNUM,
  { JITTERLISP_NEGATE_(res, args [0]); })
/* Boolean operations. */
JITTERLISP_PRIMITIVE_FUNCTION_1_(not, ANYTHING,
  { JITTERLISP_NOT_(res, args [0]); })
/* Number comparison. */
JITTERLISP_PRIMITIVE_FUNCTION_2_(equals, FIXNUM, FIXNUM,
  { JITTERLISP_EQP_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(different, FIXNUM, FIXNUM,
  { JITTERLISP_NOT_EQP_(res, args [0], args [1]); })
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
JITTERLISP_PRIMITIVE_FUNCTION_1_(non_zerop, FIXNUM,
  { JITTERLISP_NON_ZEROP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(positivep, FIXNUM,
  { JITTERLISP_POSITIVEP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(non_positivep, FIXNUM,
  { JITTERLISP_NON_POSITIVEP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(negativep, FIXNUM,
  { JITTERLISP_NEGATIVEP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(non_negativep, FIXNUM,
  { JITTERLISP_NON_NEGATIVEP_(res, args [0]); })
/* Comparison. */
JITTERLISP_PRIMITIVE_FUNCTION_2_(eqp, ANYTHING, ANYTHING,
  { JITTERLISP_EQP_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(not_eqp, ANYTHING, ANYTHING,
  { JITTERLISP_NOT_EQP_(res, args [0], args [1]); })
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
JITTERLISP_PRIMITIVE_FUNCTION_1_(constantp, SYMBOL,
  { JITTERLISP_CONSTANTP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(make_constant, SYMBOL,
  { JITTERLISP_MAKE_CONSTANT_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(definedp, SYMBOL,
  { JITTERLISP_DEFINEDP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(symbol_global, SYMBOL,
  { JITTERLISP_SYMBOL_GLOBAL_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(undefine, SYMBOL,
  { JITTERLISP_UNDEFINE_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_0_(interned_symbols,
  { JITTERLISP_INTERNED_SYMBOLS_(res); })
/* Closure operations. */
JITTERLISP_PRIMITIVE_FUNCTION_1_(closure_environment, CLOSURE,
  { JITTERLISP_CLOSURE_ENVIRONMENT_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(closure_formals, CLOSURE,
  { JITTERLISP_CLOSURE_FORMALS_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(closure_body, CLOSURE,
  { JITTERLISP_CLOSURE_BODY_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_4_(closure_setb, CLOSURE, ALIST, SYMBOLS, AST,
  { JITTERLISP_CLOSURE_SET_(res, args [0], args [1], args [2], args [3]); })
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
/* Error handling operations. */
JITTERLISP_PRIMITIVE_FUNCTION_1_(error, ANYTHING,
  { JITTERLISP_ERROR_(res, args [0]); })
/* AST case-checking operations. */
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_literalp, AST,
  { JITTERLISP_AST_LITERALP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_variablep, AST,
  { JITTERLISP_AST_VARIABLEP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_definep, AST,
  { JITTERLISP_AST_DEFINEP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_ifp, AST,
  { JITTERLISP_AST_IFP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_setbp, AST,
  { JITTERLISP_AST_SETBP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_whilep, AST,
  { JITTERLISP_AST_WHILEP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_primitivep, AST,
  { JITTERLISP_AST_PRIMITIVEP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_callp, AST,
  { JITTERLISP_AST_CALLP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_lambdap, AST,
  { JITTERLISP_AST_LAMBDAP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_letp, AST,
  { JITTERLISP_AST_LETP_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_sequencep, AST,
  { JITTERLISP_AST_SEQUENCEP_(res, args [0]); })
/* AST construction operations.  There's no need to check types here, since
   the underlying C functions do that already. */
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_literal, ANYTHING,
  { JITTERLISP_AST_LITERAL_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_variable, ANYTHING,
  { JITTERLISP_AST_VARIABLE_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(ast_define, ANYTHING, ANYTHING,
  { JITTERLISP_AST_DEFINE_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_3_(ast_if, ANYTHING, ANYTHING, ANYTHING,
  { JITTERLISP_AST_IF_(res, args [0], args [1], args [2]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(ast_setb, ANYTHING, ANYTHING,
  { JITTERLISP_AST_SETB_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(ast_while, ANYTHING, ANYTHING,
  { JITTERLISP_AST_WHILE_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(ast_primitive, ANYTHING, ANYTHING,
  { JITTERLISP_AST_PRIMITIVE_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(ast_call, ANYTHING, ANYTHING,
  { JITTERLISP_AST_CALL_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(ast_lambda, ANYTHING, ANYTHING,
  { JITTERLISP_AST_LAMBDA_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_3_(ast_let, ANYTHING, ANYTHING, ANYTHING,
  { JITTERLISP_AST_LET_(res, args [0], args [1], args [2]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(ast_sequence, ANYTHING, ANYTHING,
  { JITTERLISP_AST_SEQUENCE_(res, args [0], args [1]); })
/* AST access operations. */
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_literal_value, AST,
  { JITTERLISP_AST_LITERAL_VALUE_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_variable_name, AST,
  { JITTERLISP_AST_VARIABLE_NAME_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_define_name, AST,
  { JITTERLISP_AST_DEFINE_NAME_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_define_body, AST,
  { JITTERLISP_AST_DEFINE_BODY_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_if_condition, AST,
  { JITTERLISP_AST_IF_CONDITION_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_if_then, AST,
  { JITTERLISP_AST_IF_THEN_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_if_else, AST,
  { JITTERLISP_AST_IF_ELSE_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_setb_name, AST,
  { JITTERLISP_AST_SETB_NAME_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_setb_body, AST,
  { JITTERLISP_AST_SETB_BODY_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_while_guard, AST,
  { JITTERLISP_AST_WHILE_GUARD_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_while_body, AST,
  { JITTERLISP_AST_WHILE_BODY_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_primitive_operator, AST,
  { JITTERLISP_AST_PRIMITIVE_OPERATOR_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_primitive_operands, AST,
  { JITTERLISP_AST_PRIMITIVE_OPERANDS_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_call_operator, AST,
  { JITTERLISP_AST_CALL_OPERATOR_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_call_operands, AST,
  { JITTERLISP_AST_CALL_OPERANDS_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_lambda_formals, AST,
  { JITTERLISP_AST_LAMBDA_FORMALS_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_lambda_body, AST,
  { JITTERLISP_AST_LAMBDA_BODY_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_let_bound_name, AST,
  { JITTERLISP_AST_LET_BOUND_NAME_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_let_bound_form, AST,
  { JITTERLISP_AST_LET_BOUND_FORM_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_let_body, AST,
  { JITTERLISP_AST_LET_BODY_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_sequence_first, AST,
  { JITTERLISP_AST_SEQUENCE_FIRST_(res, args [0]); })
JITTERLISP_PRIMITIVE_FUNCTION_1_(ast_sequence_second, AST,
  { JITTERLISP_AST_SEQUENCE_SECOND_(res, args [0]); })
/* Interpretation operations. */
JITTERLISP_PRIMITIVE_FUNCTION_2_(macroexpand, ANYTHING, ALIST,
  { JITTERLISP_MACROEXPAND_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(eval_interpreter, ANYTHING, ALIST,
  { JITTERLISP_EVAL_INTERPRETER_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(eval_vm, ANYTHING, ALIST,
  { JITTERLISP_EVAL_VM_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(eval, ANYTHING, ALIST,
  { JITTERLISP_EVAL_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(apply_interpreter, CLOSURE, LIST,
  { JITTERLISP_APPLY_INTERPRETER_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(apply_vm, CLOSURE, LIST,
  { JITTERLISP_APPLY_VM_(res, args [0], args [1]); })
JITTERLISP_PRIMITIVE_FUNCTION_2_(apply, CLOSURE, LIST,
  { JITTERLISP_APPLY_(res, args [0], args [1]); })
/* Operations to display legal notices. */
JITTERLISP_PRIMITIVE_FUNCTION_0_(copying,
  { printf ("%s\n", jitterlisp_gpl); })
JITTERLISP_PRIMITIVE_FUNCTION_0_(no_warranty,
  { printf ("%s\n", jitterlisp_no_warranty); })

/* Primitive macro functions. */
JITTERLISP_PRIMITIVE_MACRO_FUNCTION_(define)
JITTERLISP_PRIMITIVE_MACRO_FUNCTION_(define_constant)
JITTERLISP_PRIMITIVE_MACRO_FUNCTION_(if)
JITTERLISP_PRIMITIVE_MACRO_FUNCTION_(cond)
JITTERLISP_PRIMITIVE_MACRO_FUNCTION_(setb)
JITTERLISP_PRIMITIVE_MACRO_FUNCTION_(while)
JITTERLISP_PRIMITIVE_MACRO_FUNCTION_(primitive)
JITTERLISP_PRIMITIVE_MACRO_FUNCTION_(call)
JITTERLISP_PRIMITIVE_MACRO_FUNCTION_(lambda)
JITTERLISP_PRIMITIVE_MACRO_FUNCTION_(let_star)
JITTERLISP_PRIMITIVE_MACRO_FUNCTION_(begin)
JITTERLISP_PRIMITIVE_MACRO_FUNCTION_(quote)
JITTERLISP_PRIMITIVE_MACRO_FUNCTION_(undefined)
JITTERLISP_PRIMITIVE_MACRO_FUNCTION_(low_level_macro)




/* Primitive descriptor definitions.
 * ************************************************************************** */

/* Define every primitive descriptor in a global constant array. */
static struct jitterlisp_primitive
jitterlisp_primitives []
  = {
      /* Type checking. */
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("fixnum?", 1, fixnump),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("character?", 1, characterp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("null?", 1, nullp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("non-null?", 1, non_nullp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("eof?", 1, eofp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("boolean?", 1, booleanp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("nothing?", 1, nothingp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("undefined?", 1, undefinedp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("symbol?", 1, symbolp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("non-symbol?", 1, non_symbolp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("cons?", 1, consp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("non-cons?", 1, non_consp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("closure?", 1, closurep),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("primitive?", 1, primitivep),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast?", 1, astp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("macro?", 1, macrop),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("vector?", 1, vectorp),
      /* Arithmetic. */
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("primordial-+", 2, plus),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("primordial--", 2, minus),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("primordial-*", 2, times),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("primordial-/", 2, divided),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("quotient", 2, quotient),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("remainder", 2, remainder),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("1+", 1, one_plus),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("1-", 1, one_minus),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("negate", 1, negate),
      /* Boolean operations. */
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("not", 1, not),
      /* Number comparison. */
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("=", 2, equals),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("<>", 2, different),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("<", 2, less),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("<=", 2, notgreater),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_(">", 2, greater),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_(">=", 2, notless),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("zero?", 1, zerop),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("non-zero?", 1, non_zerop),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("positive?", 1, positivep),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("non-positive?", 1, non_positivep),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("negative?", 1, negativep),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("non-negative?", 1, non_negativep),
      /* Comparison. */
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("eq?", 2, eqp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("not-eq?", 2, not_eqp),
      /* Cons operations. */
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("cons", 2, cons),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("car", 1, car),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("cdr", 1, cdr),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("set-car!", 2, set_car_b),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("set-cdr!", 2, set_cdr_b),
      /* Symbol operations. */
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("gensym", 0, gensym),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("constant?", 1, constantp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("make-constant", 1, make_constant),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("defined?", 1, definedp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("symbol-global", 1, symbol_global),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("undefine", 1, undefine),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("interned-symbols", 0,
                                             interned_symbols),
      /* Closure operations. */
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("closure-environment", 1,
                                             closure_environment),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("closure-formals", 1,
                                             closure_formals),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("closure-body", 1, closure_body),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("closure-set!", 4, closure_setb),
      /* Vector operations. */
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("make-vector", 2, make_vector),
      /* I/O operations. */
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("display", 1, display),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("newline", 0, newline),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("read", 0, read),
      /* Error handling operations. */
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("error", 1, error),
      /* AST case-checking operations. */
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-literal?", 1, ast_literalp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-variable?", 1, ast_variablep),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-define?", 1, ast_definep),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-if?", 1, ast_ifp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-set!?", 1, ast_setbp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-while?", 1, ast_whilep),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-primitive?", 1, ast_primitivep),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-call?", 1, ast_callp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-lambda?", 1, ast_lambdap),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-let?", 1, ast_letp),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-sequence?", 1, ast_sequencep),
      /* AST construction operations. */
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-literal", 1, ast_literal),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-variable", 1, ast_variable),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-define", 2, ast_define),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-if", 3, ast_if),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-set!", 2, ast_setb),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-while", 2, ast_while),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-primitive", 2, ast_primitive),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-call", 2, ast_call),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-lambda", 2, ast_lambda),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-let", 3, ast_let),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-sequence", 2, ast_sequence),
      /* AST access operations. */
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-literal-value", 1, ast_literal_value),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-variable-name", 1, ast_variable_name),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-define-name", 1, ast_define_name),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-define-body", 1, ast_define_body),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-if-condition", 1, ast_if_condition),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-if-then", 1, ast_if_then),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-if-else", 1, ast_if_else),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-set!-name", 1, ast_setb_name),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-set!-body", 1, ast_setb_body),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-while-guard", 1, ast_while_guard),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-while-body", 1, ast_while_body),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-primitive-operator", 1, ast_primitive_operator),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-primitive-operands", 1, ast_primitive_operands),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-call-operator", 1, ast_call_operator),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-call-operands", 1, ast_call_operands),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-lambda-formals", 1, ast_lambda_formals),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-lambda-body", 1, ast_lambda_body),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-let-bound-name", 1, ast_let_bound_name),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-let-bound-form", 1, ast_let_bound_form),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-let-body", 1, ast_let_body),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-sequence-first", 1, ast_sequence_first),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("ast-sequence-second", 1, ast_sequence_second),
      /* Interpretation operations. */
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("primordial-macroexpand", 2,
                                             macroexpand),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("primordial-eval-interpreter", 2,
                                             eval_interpreter),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("primordial-eval-vm", 2, eval_vm),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("primordial-eval", 2, eval),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("apply-interpreter", 2,
                                             apply_interpreter),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("apply-vm", 2, apply_vm),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("apply", 2, apply),
      /* Operations to display legal notices. */
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("copying", 0, copying),
      JITTERLISP_PRIMITIVE_PROCEDURE_STRUCT_("no-warranty", 0, no_warranty),

      /* Primitive macros */
      JITTERLISP_PRIMITIVE_MACRO_STRUCT_("define", define),
      JITTERLISP_PRIMITIVE_MACRO_STRUCT_("define-constant", define_constant),
      JITTERLISP_PRIMITIVE_MACRO_STRUCT_("if", if),
      JITTERLISP_PRIMITIVE_MACRO_STRUCT_("cond", cond),
      JITTERLISP_PRIMITIVE_MACRO_STRUCT_("set!", setb),
      JITTERLISP_PRIMITIVE_MACRO_STRUCT_("while", while),
      JITTERLISP_PRIMITIVE_MACRO_STRUCT_("primitive", primitive),
      JITTERLISP_PRIMITIVE_MACRO_STRUCT_("call", call),
      JITTERLISP_PRIMITIVE_MACRO_STRUCT_("lambda", lambda),
      JITTERLISP_PRIMITIVE_MACRO_STRUCT_("let*", let_star),
      JITTERLISP_PRIMITIVE_MACRO_STRUCT_("begin", begin),
      JITTERLISP_PRIMITIVE_MACRO_STRUCT_("quote", quote),
      JITTERLISP_PRIMITIVE_MACRO_STRUCT_("undefined", undefined),
      JITTERLISP_PRIMITIVE_MACRO_STRUCT_("low-level-macro", low_level_macro)
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
  /* Allocate a few symbols used for primitive wrapper (see below) formals.  In
     order to make the printed closures more readable and to relieve the stress
     on the garbage collector, we use interned symbols.  There is no harm in
     reusing the sams symbols for every primitive wrapper.  */
  jitterlisp_object variable_array [JITTERLISP_PRIMITIVE_MAX_IN_ARITY];
  int i;
  for (i = 0; i < JITTERLISP_PRIMITIVE_MAX_IN_ARITY; i ++)
    {
      char variable_name [100];
      sprintf (variable_name, "x-%i", i);
      struct jitterlisp_symbol *variable_struct_p
        = jitterlisp_symbol_make_interned (variable_name);
      variable_array [i] = JITTERLISP_SYMBOL_ENCODE(variable_struct_p);
    }

  /* For every possible primitive procedure arity make a list of formals (which
     is to say, a list of symbols) and a list of actuals (which is to say, a
     list of variable ASTs).  Share structure as far as possible. */
  jitterlisp_object formals [JITTERLISP_PRIMITIVE_MAX_IN_ARITY + 1];
  jitterlisp_object actuals [JITTERLISP_PRIMITIVE_MAX_IN_ARITY + 1];
  formals [0] = JITTERLISP_EMPTY_LIST;
  actuals [0] = JITTERLISP_EMPTY_LIST;
  for (i = 1; i <= JITTERLISP_PRIMITIVE_MAX_IN_ARITY; i ++)
    {
      formals [i] = jitterlisp_cons (variable_array [i - 1],
                                     formals [i - 1]);
      actuals [i] = jitterlisp_cons (jitterlisp_ast_make_variable
                                        (variable_array [i - 1]),
                                     actuals [i - 1]);
    }

  /* For every primitive... */
  for (i = 0; i < jitterlisp_primitive_no; i ++)
    {
      struct jitterlisp_symbol *name_symbol_p
        = jitterlisp_symbol_make_interned (jitterlisp_primitives [i].name);
      struct jitterlisp_primitive *descriptor = jitterlisp_primitives + i;
      /* ...Check if the primitive descriptor is for a primitive procedure or a
         primitive macro. */
      if (descriptor->procedure)
        {
          /* The descriptor is for a primitive procedure.  Globally bind two
             interned symbols, one (with the name prefixed by "primitive-") to
             the primitive procedure object, and another (with no name prefix)
             to a closure wrapper around it.
             Rationale: ordinary user code will call only closures, never
             primitives: this avoids a type check on the operand at call
             time.  Known calls to primitives can be made efficient via
             inlining. */

          /* Define the primitive object. */
          size_t name_length = strlen (jitterlisp_primitives [i].name);
          char *prefixed_name = jitter_xmalloc (name_length + 100);
          sprintf (prefixed_name,
                   "primitive-%s", jitterlisp_primitives [i].name);
          struct jitterlisp_symbol *prefixed_name_symbol_p
            = jitterlisp_symbol_make_interned (prefixed_name);
          free (prefixed_name);
          jitterlisp_object primitive_object
            = JITTERLISP_PRIMITIVE_ENCODE(descriptor);
          prefixed_name_symbol_p->global_value = primitive_object;
          prefixed_name_symbol_p->global_constant = true;

          /* Define the wrapper as a closure object containing a primitive
             use. */
          jitterlisp_object formals_for_this_arity
            = formals [jitterlisp_primitives [i].in_arity];
          jitterlisp_object actuals_for_this_arity
            = actuals [jitterlisp_primitives [i].in_arity];
          jitterlisp_object wrapper_body
            = jitterlisp_ast_make_primitive (primitive_object,
                                             actuals_for_this_arity);
          jitterlisp_object closure;
          JITTERLISP_CLOSURE_(closure,
                              jitterlisp_empty_environment,
                              formals_for_this_arity,
                              wrapper_body);
          name_symbol_p->global_value = closure;
          name_symbol_p->global_constant = true;
        }
      else
        {
          /* The descriptor is for a primitive macro.  Just globally bind the
             symbol to a primitive macro object. */
          name_symbol_p->global_value
            = JITTERLISP_PRIMITIVE_MACRO_ENCODE(descriptor);

          /* Notice that primitive macros are not bound as constants: it is
             possible to redefine them in Lisp, which has no serious performance
             implications.  The main purpose of constants is to allow inlining
             of known callees before run time; macroexpansion occurs before
             run time anyway. */
        }
    }
}

void
jitterlisp_primitives_finalize (void)
{
  /* Do nothing.  Interned symbols are destroyed by the memory subsystem
     finalization function, and primitive descriptors are global constants. */
}

#endif // #ifndef JITTERLISP_PRIMITIVES_H_
