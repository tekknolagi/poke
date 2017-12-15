/* Jittery Lisp: AST header.

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


#ifndef JITTERLISP_AST_H_
#define JITTERLISP_AST_H_



#include "jitterlisp.h"
#include "jitterlisp-sexpression.h"




/* AST data structure.
 * ************************************************************************** */

/* The AST case as an expression case identifier. */
enum jitterlisp_ast_case
  {
    /* Literal constant.  Exactly one sub, any s-expression. */
    jitterlisp_ast_case_literal,

    /* Variable.  Exactly one sub, a symbol. */
    jitterlisp_ast_case_variable,

    /* Global definition.  Exactly two subs, a symbol and an AST. */
    jitterlisp_ast_case_define,

    /* Two-way conditional.  Exactly three subs, ASTs. */
    jitterlisp_ast_case_if,

    /* Variable assignment.  Exactly two subs, a symbol and an AST. */
    jitterlisp_ast_case_setb,

    /* While loop.  Exactly two subs, a guard AST and a body AST. */
    jitterlisp_ast_case_while,

    /* Primitive call.  At least one sub, the primitive object, then one AST per
       actual. */
    jitterlisp_ast_case_primitive,

    /* Procedure call.  At least one sub, the operand as an AST, then one AST
       per actual. */
    jitterlisp_ast_case_call,

    /* Procedure abstraction.  Exactly two subs, one list of symbols for the
       formals and one AST for the body.  This is different from other cases,
       but allows the list of formals to be reused as a shared read-only datum,
       making closure initialization faster. */
    jitterlisp_ast_case_lambda,

    /* Parallel-binding let.  Two subs per binding (a symbol for the variable,
       an AST for the bound expression, alternated in this order) followed by
       one AST for the body. */
    jitterlisp_ast_case_let,

    /* Sequence.  Exactly two AST subs. */
    jitterlisp_ast_case_sequence,

    /* Current-environment form.  No subs. */
    jitterlisp_ast_case_current_environment
  };

/* An AST data structure.  The structure definition itself seems lax, but
   operations on ASTs are designed to only build syntactically valid ASTs,
   and fail otherwise.  This eliminates the need for safety checks at
   interpretation or VM code generation time.  Sub-structures are all
   tagged. */
struct jitterlisp_ast
{
  /* What case this AST represents. */
  enum jitterlisp_ast_case case_;

  /* How many sub-components this AST has.  This is kept correct even for cases
     with a fixed number of subs. */
  jitter_uint sub_no;

  /* A pointer to this AST's sub-components, which are element_no.  Some
     sub-components may be other ASTs and others may be different Lisp object,
     according to the case. */
  jitterlisp_object *subs;
};




/* AST high-level allocation.
 * ************************************************************************** */

/* Return a fresh encoded AST of the appropriate case, completely initialized
   with the given subs.  When multiple ASTs or multiple symbols are required
   (such as in the case of primitive or lambda) the arguments are s-expression
   lists with ASTs or symbols as elements; in such case the formal arguments
   have a plural name ending in _asts or _symbols . */
jitterlisp_object
jitterlisp_ast_make_literal (jitterlisp_object value);
jitterlisp_object
jitterlisp_ast_make_variable (jitterlisp_object symbol);
jitterlisp_object
jitterlisp_ast_make_define (jitterlisp_object symbol, jitterlisp_object ast);
jitterlisp_object
jitterlisp_ast_make_if (jitterlisp_object condition_ast,
                        jitterlisp_object then_ast,
                        jitterlisp_object else_ast);
jitterlisp_object
jitterlisp_ast_make_setb (jitterlisp_object symbol, jitterlisp_object ast);
jitterlisp_object
jitterlisp_ast_make_while (jitterlisp_object condition_ast,
                           jitterlisp_object body_ast);
jitterlisp_object
jitterlisp_ast_make_primitive (jitterlisp_object primitive_symbol,
                               jitterlisp_object actual_asts);
jitterlisp_object
jitterlisp_ast_make_call (jitterlisp_object operator_ast,
                          jitterlisp_object actual_asts);
jitterlisp_object
jitterlisp_ast_make_lambda (jitterlisp_object formal_symbols,
                            jitterlisp_object body_ast);
jitterlisp_object
jitterlisp_ast_make_let (jitterlisp_object bound_symbols,
                         jitterlisp_object bound_asts,
                         jitterlisp_object body_ast);
jitterlisp_object
jitterlisp_ast_make_sequence (jitterlisp_object ast_0,
                              jitterlisp_object ast_1);
jitterlisp_object
jitterlisp_ast_make_current_environment (void);

#endif // #ifndef JITTERLISP_AST_H_
