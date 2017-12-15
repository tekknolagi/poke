/* Jittery Lisp: Lisp macros.

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


#include "jitterlisp-macros.h"

#include "jitterlisp.h"


/* Macroexpansion utility.
 * ************************************************************************** */

/* Return the list of the macroexpansions of the s-expressions in the given
   list. */
static jitterlisp_object
jitterlisp_macroexpand_multiple (jitterlisp_object os,
                                 jitterlisp_object env)
{
  /* This logic is slightly more complex than the obvious recursive alternative,
     but uses constant stack space and doesn't use any temporary heap data
     structure. */
  jitterlisp_object res;
  jitterlisp_object *res_restp = & res;
  while (! JITTERLISP_IS_EMPTY_LIST(os))
    {
      if (! JITTERLISP_IS_CONS(os))
        {
          printf ("About "); // FIXME: integrate with the error function
          jitterlisp_print_to_stream (stdout, os);
          jitterlisp_error_cloned ("jitterlisp_macroexpand_multiple: non-list "
                                   "argument");
        }
      jitterlisp_object car = JITTERLISP_EXP_C_A_CAR(os);
      jitterlisp_object macroexpanded_car = jitterlisp_macroexpand (car, env);
      * res_restp = jitterlisp_cons (macroexpanded_car, JITTERLISP_UNDEFINED);
      res_restp = & JITTERLISP_EXP_C_A_CDR(* res_restp);

      os = JITTERLISP_EXP_C_A_CDR(os);
    }
  * res_restp = JITTERLISP_EMPTY_LIST;
  return res;
}

static jitterlisp_object
jitterlisp_macroexpand_macro_call (jitterlisp_object macro,
                                   jitterlisp_object cdr,
                                   jitterlisp_object env)
{
  /*
  printf ("%% macroexpanding macro call to ");
  jitterlisp_print_to_stream (stdout, macro); printf ("...\n");
  */
  if (JITTERLISP_IS_PRIMITIVE_MACRO(macro))
    {
      struct jitterlisp_primitive *p = JITTERLISP_PRIMITIVE_MACRO_DECODE(macro);
      jitterlisp_object primitive_args [2] = {cdr, env};
      return p->function (primitive_args);
    }
  else if (JITTERLISP_IS_NON_PRIMITIVE_MACRO(macro))
    {
      //struct jitterlisp_closure *c = JITTERLISP_NON_PRIMITIVE_MACRO_DECODE(macro);
    jitterlisp_error_cloned ("macroexpanding non-primitive macro call: unimplemented");
    }
  else
    jitterlisp_error_cloned ("macroexpanding macro call to non-macro");
}

static jitterlisp_object
jitterlisp_macroexpand_cons (jitterlisp_object o, jitterlisp_object env)
{
  jitterlisp_object car = JITTERLISP_EXP_C_A_CAR(o);
  jitterlisp_object cdr = JITTERLISP_EXP_C_A_CDR(o);
  /*
  printf ("* jitterlisp_macroexpand_cons: car is ");
  jitterlisp_print_to_stream (stdout, car); printf ("\n");
  printf ("  jitterlisp_macroexpand_cons: cdr is ");
  jitterlisp_print_to_stream (stdout, cdr); printf ("\n");
  printf ("  jitterlisp_macroexpand_cons: env is ");
  jitterlisp_print_to_stream (stdout, env); printf ("\n");
  */
  /* If the car is a symbol bound to a macro then the cons encodes a macro call.
     Macroexpand it as such. */
  if (JITTERLISP_IS_SYMBOL (car) &&
      jitterlisp_environment_has (env, car))
    {
      jitterlisp_object car_value = jitterlisp_environment_lookup (env, car);
      if (JITTERLISP_IS_MACRO (car_value))
        return jitterlisp_macroexpand_macro_call (car_value, cdr, env);
    }

  /* The cons doesn't encode a macro call.  Macroexpand it into a procedure
     call. */
  return jitterlisp_primitive_macro_function_call (o, env);
}




/* Macroexpansion main function.
 * ************************************************************************** */

jitterlisp_object
jitterlisp_macroexpand (jitterlisp_object o,
                        jitterlisp_object env)
{
  if (JITTERLISP_IS_AST(o))
    /* FIXME: is this case necessary?  Is it desirable? */
    return o;
  else if (JITTERLISP_IS_SYMBOL(o))
    return jitterlisp_ast_make_variable (o);
  else if (JITTERLISP_IS_CONS(o))
    return jitterlisp_macroexpand_cons (o, env);
  else
    return jitterlisp_ast_make_literal (o);
}




/* AST primitive macro functions.
 * ************************************************************************** */

jitterlisp_object
jitterlisp_primitive_macro_function_define (jitterlisp_object cdr,
                                            jitterlisp_object env)
{
  if (! JITTERLISP_IS_CONS (cdr))
    jitterlisp_error_cloned ("define: invalid cdr");

  jitterlisp_object defined_thing = JITTERLISP_EXP_C_A_CAR (cdr);
  if (JITTERLISP_IS_SYMBOL(defined_thing))
    {
      jitterlisp_object body
        = jitterlisp_primitive_macro_function_begin
             (JITTERLISP_EXP_C_A_CDR (cdr), env);
      return jitterlisp_ast_make_define (defined_thing, body);
    }
  else if (JITTERLISP_IS_CONS(defined_thing))
    {
      jitterlisp_object defined_name = JITTERLISP_EXP_C_A_CAR (defined_thing);
      if (! JITTERLISP_IS_SYMBOL (defined_name))
        jitterlisp_error_cloned ("define: non-symbol procedure name");
      jitterlisp_object formals = jitterlisp_cdr (defined_thing);
      jitterlisp_object lambda
        = jitterlisp_primitive_macro_function_lambda
             (jitterlisp_cons (formals, jitterlisp_cdr (cdr)),
              env);
      return jitterlisp_ast_make_define (defined_name, lambda);
    }
  else
    jitterlisp_error_cloned ("define: invalid cadr");
}

jitterlisp_object
jitterlisp_primitive_macro_function_if (jitterlisp_object cdr,
                                        jitterlisp_object env)
{
  if (! JITTERLISP_IS_CONS (cdr))
    jitterlisp_error_cloned ("if: invalid cdr");
  if (! JITTERLISP_IS_CONS (JITTERLISP_EXP_C_A_CDR (cdr)))
    jitterlisp_error_cloned ("if: invalid cddr");

  jitterlisp_object condition = JITTERLISP_EXP_C_A_CAR (cdr);
  jitterlisp_object then_branch = jitterlisp_cadr (cdr);
  jitterlisp_object else_branch_forms = jitterlisp_cddr (cdr);

  return jitterlisp_ast_make_if
            (jitterlisp_macroexpand (condition, env),
             jitterlisp_macroexpand (then_branch, env),
             jitterlisp_primitive_macro_function_begin (else_branch_forms,
                                                        env));
}

jitterlisp_object
jitterlisp_primitive_macro_function_cond (jitterlisp_object cdr,
                                          jitterlisp_object env)
{
  jitterlisp_object clauses = cdr;

  /* A no-clause cons expands to a no-form begin. */
  if (JITTERLISP_IS_EMPTY_LIST (clauses))
    return jitterlisp_primitive_macro_function_begin (clauses, env);

  if (! JITTERLISP_IS_CONS (clauses))
    jitterlisp_error_cloned ("cond: non-list clauses");

  /* There is at least one clause.  Expand to an if with another cond in the
     else branch. */
  jitterlisp_object clause = JITTERLISP_EXP_C_A_CAR (cdr);
  if (! JITTERLISP_IS_CONS (clause))
    jitterlisp_error_cloned ("cond: non-list clause");
  jitterlisp_object condition = JITTERLISP_EXP_C_A_CAR (clause);
  jitterlisp_object clause_body = JITTERLISP_EXP_C_A_CDR (clause);
  jitterlisp_object more_clauses = JITTERLISP_EXP_C_A_CDR (clauses);
  return jitterlisp_ast_make_if
            (jitterlisp_macroexpand (condition, env),
             jitterlisp_primitive_macro_function_begin (clause_body, env),
             jitterlisp_primitive_macro_function_cond (more_clauses,
                                                       env));
}

jitterlisp_object
jitterlisp_primitive_macro_function_setb (jitterlisp_object cdr,
                                          jitterlisp_object env)
{
  // FIXME: move validation to AST construction?
  if (! JITTERLISP_IS_CONS (cdr))
    jitterlisp_error_cloned ("set!: invalid cdr");
  jitterlisp_object variable = JITTERLISP_EXP_C_A_CAR (cdr);
  if (! JITTERLISP_IS_SYMBOL (variable))
    jitterlisp_error_cloned ("set!: non-symbol variable name");
  jitterlisp_object forms = JITTERLISP_EXP_C_A_CDR (cdr);

  return jitterlisp_ast_make_setb
            (variable,
             jitterlisp_primitive_macro_function_begin (forms, env));
}

jitterlisp_object
jitterlisp_primitive_macro_function_while (jitterlisp_object cdr,
                                           jitterlisp_object env)
{
  if (! JITTERLISP_IS_CONS (cdr))
    jitterlisp_error_cloned ("while: invalid cdr");

  jitterlisp_object guard = JITTERLISP_EXP_C_A_CAR (cdr);
  jitterlisp_object body_forms = JITTERLISP_EXP_C_A_CDR (cdr);

  return jitterlisp_ast_make_while
            (jitterlisp_macroexpand (guard, env),
             jitterlisp_primitive_macro_function_begin (body_forms, env));
}

jitterlisp_object
jitterlisp_primitive_macro_function_primitive (jitterlisp_object cdr,
                                               jitterlisp_object env)
{
  // FIXME: move validation to AST construction?
  if (! JITTERLISP_IS_CONS (cdr))
    jitterlisp_error_cloned ("primitive: invalid cdr");
  jitterlisp_object primitive_name = jitterlisp_car (cdr);
  if (! JITTERLISP_IS_SYMBOL (primitive_name))
    jitterlisp_error_cloned ("primitive: non-symbol primitive name");
  jitterlisp_object primitive
    = JITTERLISP_SYMBOL_DECODE(primitive_name)->global_value;
  if (! JITTERLISP_IS_PRIMITIVE (primitive))
    jitterlisp_error_cloned ("primitive: non primitive name");
  jitterlisp_object primitive_actuals = jitterlisp_cdr (cdr);
  size_t actual_arity = jitterlisp_length (primitive_actuals);
  if (actual_arity != JITTERLISP_PRIMITIVE_DECODE(primitive)->in_arity)
    jitterlisp_error_cloned ("primitive: invalid arity");

  return jitterlisp_ast_make_primitive
            (primitive,
             jitterlisp_macroexpand_multiple (primitive_actuals, env));
}

jitterlisp_object
jitterlisp_primitive_macro_function_call (jitterlisp_object cdr,
                                          jitterlisp_object env)
{
  if (! JITTERLISP_IS_CONS (cdr))
    jitterlisp_error_cloned ("call: invalid cdr");
  jitterlisp_object operator = JITTERLISP_EXP_C_A_CAR (cdr);
  jitterlisp_object operands = JITTERLISP_EXP_C_A_CDR (cdr);
  return jitterlisp_ast_make_call (jitterlisp_macroexpand (operator, env),
                                   jitterlisp_macroexpand_multiple (operands,
                                                                    env));
}

jitterlisp_object
jitterlisp_primitive_macro_function_lambda (jitterlisp_object cdr,
                                            jitterlisp_object env)
{
  if (! JITTERLISP_IS_CONS (cdr))
    jitterlisp_error_cloned ("lambda: invalid cdr");
  jitterlisp_object formals = JITTERLISP_EXP_C_A_CAR(cdr);
  jitterlisp_object body_forms = JITTERLISP_EXP_C_A_CDR(cdr);
  return jitterlisp_ast_make_lambda
            (formals,
             jitterlisp_primitive_macro_function_begin (body_forms, env));
}

/* Given a list encoding let bindings return the list of bound symbols in order,
   or error out if the binding list is not well-formed.
   Rationale: this is a slightly inefficient, non-tail recursive way of scanning
   let bindings, but is simpler to handle than building a list front-to-back
   with pointers in code that is supposed to support exact pointer finding in
   the future: the alternative would require pointers to list elements to be
   filled, or reversing lists. */
static jitterlisp_object
jitterlisp_bindings_get_bound_variables (jitterlisp_object bindings)
{
  if (JITTERLISP_IS_EMPTY_LIST(bindings))
    return JITTERLISP_EMPTY_LIST;
  else if (! JITTERLISP_IS_CONS(bindings))
    jitterlisp_error_cloned ("let: non-list bindings");

  jitterlisp_object binding = JITTERLISP_EXP_C_A_CAR(bindings);
  if (! JITTERLISP_IS_CONS(binding))
    jitterlisp_error_cloned ("let: non-list binding");
  jitterlisp_object bound_symbol = JITTERLISP_EXP_C_A_CAR(binding);
  if (! JITTERLISP_IS_SYMBOL(bound_symbol))
    jitterlisp_error_cloned ("let: non-symbol bound variable");

  jitterlisp_object cdr_bindings = JITTERLISP_EXP_C_A_CDR (bindings);
  jitterlisp_object cdr_bound_variables
    = jitterlisp_bindings_get_bound_variables (cdr_bindings);
  return jitterlisp_cons (bound_symbol, cdr_bound_variables);
}

/* Given a list encoding let bindings and a non-global environment return the
   list of bound ASTs in order, already expanded.  This assumes that
   jitterlisp_bindings_get_bound_variables has been called before on the same
   bindings, so doesn't need to check that the bindings are well-formed.
   Rationale: see the comment for jitterlisp_bindings_get_bound_variables . */
static jitterlisp_object
jitterlisp_bindings_get_bound_asts (jitterlisp_object bindings,
                                    jitterlisp_object env)
{
  if (JITTERLISP_IS_EMPTY_LIST(bindings))
    return JITTERLISP_EMPTY_LIST;

  jitterlisp_object binding = JITTERLISP_EXP_C_A_CAR(bindings);
  jitterlisp_object bound_forms = JITTERLISP_EXP_C_A_CDR(binding);
  jitterlisp_object bound_ast
    = jitterlisp_primitive_macro_function_begin (bound_forms, env);

  jitterlisp_object cdr_bindings = JITTERLISP_EXP_C_A_CDR (bindings);
  jitterlisp_object cdr_bound_asts
    = jitterlisp_bindings_get_bound_asts (cdr_bindings, env);
  return jitterlisp_cons (bound_ast, cdr_bound_asts);
}

jitterlisp_object
jitterlisp_primitive_macro_function_let (jitterlisp_object cdr,
                                         jitterlisp_object env)
{
  /* Start by holding the binging list and the body form list in automatic C
     variables, everything unexpanded. */
  if (! JITTERLISP_IS_CONS(cdr))
    jitterlisp_error_cloned ("let: invalid cdr");
  jitterlisp_object bindings = JITTERLISP_EXP_C_A_CAR(cdr);
  jitterlisp_object body_forms = JITTERLISP_EXP_C_A_CDR(cdr);

  /* Make two separate lists, one for bound variables and another for their
     bound ASTs, by walking the binding list in order. */
  jitterlisp_object bound_symbols
    = jitterlisp_bindings_get_bound_variables (bindings);
  jitterlisp_object bound_asts
    = jitterlisp_bindings_get_bound_asts (bindings, env);

  /* Expand the body as well, and I have every component for the let AST. */
  jitterlisp_object body_ast
    = jitterlisp_primitive_macro_function_begin (body_forms, env);

  return jitterlisp_ast_make_let (bound_symbols, bound_asts, body_ast);
}

jitterlisp_object
jitterlisp_primitive_macro_function_begin (jitterlisp_object cdr,
                                           jitterlisp_object env)
{
  /* Zero-form body. */
  if (JITTERLISP_IS_EMPTY_LIST(cdr))
    return jitterlisp_ast_make_literal (JITTERLISP_NOTHING);

  /* Ill-formed body. */
  if (! JITTERLISP_IS_CONS(cdr))
    jitterlisp_error_cloned ("begin: non-list body");

  /* At this point we have to look inside the cons. */
  jitterlisp_object first = JITTERLISP_EXP_C_A_CAR(cdr);
  jitterlisp_object rest = JITTERLISP_EXP_C_A_CDR(cdr);

  /* One-form body. */
  if (JITTERLISP_IS_EMPTY_LIST(rest))
    return jitterlisp_macroexpand (first, env);

  /* Multiple-form body. */
  return jitterlisp_ast_make_sequence
            (jitterlisp_macroexpand (first, env),
             jitterlisp_primitive_macro_function_begin (rest, env));
}

jitterlisp_object
jitterlisp_primitive_macro_function_quote (jitterlisp_object cdr,
                                           jitterlisp_object env)
{
  if (! JITTERLISP_IS_CONS (cdr))
    jitterlisp_error_cloned ("quote: invalid cdr");
  if (! JITTERLISP_IS_EMPTY_LIST (JITTERLISP_EXP_C_A_CDR(cdr)))
    jitterlisp_error_cloned ("quote: invalid cddr");

  jitterlisp_object literal = JITTERLISP_EXP_C_A_CAR(cdr);
  return jitterlisp_ast_make_literal (literal);
}

jitterlisp_object
jitterlisp_primitive_macro_function_current_environment (jitterlisp_object cdr,
                                                         jitterlisp_object env)
{
  return jitterlisp_ast_make_current_environment ();
}

