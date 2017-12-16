/* Jittery Lisp: interpreter: naïve C version.

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


#include "jitterlisp-eval-interpreter.h"

#include <jitter/jitter-fatal.h> // FIXME: remove unless needed in the end.

#include "jitterlisp.h"


/* Non-Jittery interpreter: temporary main interpreter with the wrong API.
 * ************************************************************************** */
jitterlisp_object
jitterlisp_eval_interpreter_____ (jitterlisp_object unexpanded_form,
                             jitterlisp_object env)
{
  printf ("Macroexpanding ");
  jitterlisp_print_to_stream (stdout, unexpanded_form);
  printf ("...\n");
  // FIXME: this is wrong: it should be done elsewhere.
  jitterlisp_object form = jitterlisp_macroexpand (unexpanded_form, env);
  printf ("...into ");
  jitterlisp_print_to_stream (stdout, form);
  printf ("\n");

  if (! JITTERLISP_IS_AST(form))
    {
      printf ("About "); // FIXME: add to the error message
      jitterlisp_print_to_stream (stdout, form);
      printf (":\n");
      jitterlisp_error_cloned ("evaluating non-AST object");
    }
  else
    return jitterlisp_eval_interpreter_ast (form, env);
}




/* Interpreter utility.
 * ************************************************************************** */

__attribute__ ((unused))
static inline jitterlisp_object
jitterlisp_list_0 (void)
{
  return JITTERLISP_EMPTY_LIST;
}

__attribute__ ((unused))
static inline jitterlisp_object
jitterlisp_list_1 (jitterlisp_object a)
{
  return jitterlisp_cons (a, jitterlisp_list_0 ());
}

__attribute__ ((unused))
static inline jitterlisp_object
jitterlisp_list_2 (jitterlisp_object a, jitterlisp_object b)
{
  return jitterlisp_cons (a, jitterlisp_list_1 (b));
}

__attribute__ ((unused))
static inline jitterlisp_object
jitterlisp_list_3 (jitterlisp_object a, jitterlisp_object b,
                   jitterlisp_object c)
{
  return jitterlisp_cons (a, jitterlisp_list_2 (b, c));
}

__attribute__ ((unused))
static inline jitterlisp_object
jitterlisp_list_4 (jitterlisp_object a, jitterlisp_object b,
                   jitterlisp_object c, jitterlisp_object d)
{
  return jitterlisp_cons (a, jitterlisp_list_3 (b, c, d));
}

__attribute__ ((unused))
static inline jitterlisp_object
jitterlisp_list_5 (jitterlisp_object a, jitterlisp_object b,
                   jitterlisp_object c, jitterlisp_object d,
                   jitterlisp_object e)
{
  return jitterlisp_cons (a, jitterlisp_list_4 (b, c, d, e));
}

/* Return a fresh closure with the given components. */
static jitterlisp_object
jitterlisp_closure (jitterlisp_object environment,
                    jitterlisp_object formals,
                    jitterlisp_object body)
{
  jitterlisp_object res;
  if (! jitterlisp_is_list_of_symbols (formals))
    jitterlisp_error_cloned ("procedure formals not a list of symbols");
  // FIXME: (ideally) check that the body is well-formed.
  JITTERLISP_CLOSURE_(res, environment, formals, body);
  return res;
}




/* Non-Jittery interpreter helpers.
 * ************************************************************************** */

/* Return non-false iff the given object is self-evaluating. */
static bool
jitterlisp_is_self_evaluating (jitterlisp_object o)
{
  return (JITTERLISP_IS_UNIQUE(o)
          || JITTERLISP_IS_CHARACTER(o)
          || JITTERLISP_IS_FIXNUM(o));
}

/* See the comments in the version below to see why this implementation is not
   used. */
__attribute__ ((unused))
static jitterlisp_object
jitterlisp_eval_interpreter_begin_alternative (jitterlisp_object forms,
                                               jitterlisp_object env)
{
  jitterlisp_object res = JITTERLISP_NOTHING;
  while (! JITTERLISP_IS_EMPTY_LIST(forms))
    {
      if (! JITTERLISP_IS_CONS(forms))
        jitterlisp_error_cloned ("form-sequence body not a list");

      res = jitterlisp_eval_interpreter (JITTERLISP_EXP_C_A_CAR(forms), env);
      forms = JITTERLISP_EXP_C_A_CDR(forms);
    }
  return res;
}

/* Eval a list of forms in sequence; return the last result, or #<nothing> for
   an empty sequence. */
static jitterlisp_object
jitterlisp_eval_interpreter_begin (jitterlisp_object forms,
                                   jitterlisp_object env)
{
  /* It is very important that a tail call as the last form is correctly
     recognized by GCC.  The code could be made simpler otherwise; see the
     unused alternative above. */
  if (JITTERLISP_IS_EMPTY_LIST(forms))
    return JITTERLISP_NOTHING;

  while (true)
    {
      if (! JITTERLISP_IS_CONS(forms))
        jitterlisp_error_cloned ("form-sequence body not a list");
      jitterlisp_object first_form = JITTERLISP_EXP_C_A_CAR(forms);
      jitterlisp_object more_forms = JITTERLISP_EXP_C_A_CDR(forms);
      /* If there is nothing more after first_form evaluate it in a tail
         call. */
      if (JITTERLISP_IS_EMPTY_LIST(more_forms))
        return jitterlisp_eval_interpreter (first_form, env);

      /* If we arrived here then more_forms is not empty.  Evaluate first_form,
         ignoring the result, and keep iterating on more_forms. */
      jitterlisp_eval_interpreter (first_form, env);
      forms = more_forms;
    }
}

static jitterlisp_object
jitterlisp_eval_interpreter_cond (jitterlisp_object cdr,
                                  jitterlisp_object env)
{
  /* Check every clause in order. */
  jitterlisp_object clauses = cdr;
  while (! JITTERLISP_IS_EMPTY_LIST(clauses))
    {
      /* Evaluate the condition of the next clause: if it's non-#f then evaluate
         the clause forms and return their result.  This is epsilon-style: each
         cond clause can contain zero or more forms after the condition.*/
      if (! JITTERLISP_IS_CONS(clauses))
        jitterlisp_error_cloned ("cond clauses not a list");
      jitterlisp_object clause = JITTERLISP_EXP_C_A_CAR(clauses);
      if (! JITTERLISP_IS_CONS(clause))
        jitterlisp_error_cloned ("cond clause not a non-empty list");
      /* We don't support Scheme-style else conditions. */
      jitterlisp_object condition = JITTERLISP_EXP_C_A_CAR(clause);
      jitterlisp_object condition_result
        = jitterlisp_eval_interpreter (condition, env);
      if (! JITTERLISP_IS_FALSE(condition_result))
        {
          jitterlisp_object clause_forms = JITTERLISP_EXP_C_A_CDR(clause);
          return jitterlisp_eval_interpreter_begin (clause_forms, env);
        }

      /* If arrived here then the condition evaluated to #f.  Advance to the
         rest of the clause list. */
      clauses = JITTERLISP_EXP_C_A_CDR(clauses);
    }

  /* No clause condition evaluated to non-#f. */
  return JITTERLISP_NOTHING;
}

static jitterlisp_object
jitterlisp_eval_interpreter_current_environment (jitterlisp_object cdr,
                                                 jitterlisp_object env)
{
  if (! JITTERLISP_IS_EMPTY_LIST(cdr))
    jitterlisp_error_cloned ("current-environment called with arguments");

  /* Just return the existing environment as the result. */
  return env;
}

static jitterlisp_object
jitterlisp_eval_interpreter_define (jitterlisp_object cdr,
                                    jitterlisp_object env)
{
  if (! JITTERLISP_IS_CONS(cdr))
    jitterlisp_error_cloned ("define not followed by a cons");
  jitterlisp_object bound_thing = JITTERLISP_EXP_C_A_CAR(cdr);
  jitterlisp_object after_bound_thing_forms = JITTERLISP_EXP_C_A_CDR(cdr);
  jitterlisp_object variable;

  /* We support two syntaxes, Scheme-style: (define SYMBOL FORMS...)
     or (define (FUNCTION-SYMBOL ARGUMENT-SYMBOLS...) FORMS...) . */
  if (JITTERLISP_IS_SYMBOL(bound_thing))
    variable = bound_thing;
  else
    {
      if (! jitterlisp_is_list_of_symbols (bound_thing))
        jitterlisp_error_cloned ("define not followed by cons or "
                                 "list of symbols");
      if (JITTERLISP_IS_EMPTY_LIST(bound_thing))
        jitterlisp_error_cloned ("define followed by empty list");

      /* Translate (define (FUNCTION-SYMBOL ARGUMENT-SYMBOLS...) FORMS...) into
         (define FUNCTION-SYMBOL (lambda (ARGUMENT-SYMBOLS...) FORMS...) . */
      variable = jitterlisp_car (bound_thing);
      jitterlisp_object lambda
        = jitterlisp_cons (jitterlisp_object_lambda,
                           jitterlisp_cons (jitterlisp_cdr (bound_thing),
                                            after_bound_thing_forms));
      /* The defined forms are evaluated in a sequence: make a singleton list
         to hold the synthetic lambda. */
      after_bound_thing_forms = jitterlisp_cons (lambda, JITTERLISP_EMPTY_LIST);
    }
  jitterlisp_object new_value
    = jitterlisp_eval_interpreter_begin (after_bound_thing_forms, env);

  /* Always bind in the global environment, ignoring any binding for variable in
     env.  This is different from Scheme (and Common-Lisp), even if it behaves
     the same way at the top level. */
  struct jitterlisp_symbol *unencoded_variable
    = JITTERLISP_SYMBOL_DECODE(variable);
  unencoded_variable->global_value = new_value;
  return JITTERLISP_NOTHING;
}

static jitterlisp_object
jitterlisp_eval_interpreter_if (jitterlisp_object cdr,
                                jitterlisp_object env)
{
  if (! JITTERLISP_IS_CONS(cdr))
    jitterlisp_error_cloned ("if not followed by a cons");
  jitterlisp_object condition = JITTERLISP_EXP_C_A_CAR(cdr);
  jitterlisp_object after_condition = JITTERLISP_EXP_C_A_CDR(cdr);
  if (! JITTERLISP_IS_CONS(after_condition))
    jitterlisp_error_cloned ("if condition not followed by a cons");
  jitterlisp_object then = JITTERLISP_EXP_C_A_CAR(after_condition);
  jitterlisp_object else_forms = JITTERLISP_EXP_C_A_CDR(after_condition);

  /* Notice the asymmetry: the then branch is only one form, but the else branch
     is a list of forms, like in Emacs Lisp and epsilon.  This is also a
     convenient way of returning #<nothing> for an empty else branch. */
  if (! JITTERLISP_IS_FALSE (jitterlisp_eval_interpreter (condition,
                                                          env)))
    return jitterlisp_eval_interpreter (then, env);
  else
    return jitterlisp_eval_interpreter_begin (else_forms, env);
}

static jitterlisp_object
jitterlisp_eval_interpreter_lambda (jitterlisp_object cdr,
                                    jitterlisp_object env)
{
  if (! JITTERLISP_IS_CONS(cdr))
    jitterlisp_error_cloned ("lambda not followed by a cons");

  jitterlisp_object formals = JITTERLISP_EXP_C_A_CAR(cdr);
  jitterlisp_object body_forms = JITTERLISP_EXP_C_A_CDR(cdr);
  return jitterlisp_closure (env, formals, body_forms);
}

/* What kind of semantics should be adopted for evaluating a block. */
enum jitterlisp_let_kind
  {
    /* Ordinary Scheme-style (non-named) let . */
    jitterlisp_let_kind_ordinary,

    /* Scheme-style let* . */
    jitterlisp_let_kind_star,

    /* Scheme-style letrec . */
    jitterlisp_let_kind_rec
  };

static inline jitterlisp_object
jitterlisp_eval_interpreter_let_variant (jitterlisp_object cdr,
                                         jitterlisp_object env,
                                         const enum jitterlisp_let_kind kind)
{
  /* Bind subforms to C variables. */
  if (! JITTERLISP_IS_CONS(cdr))
    jitterlisp_error_cloned ("let/let*/letrec not followed by a cons");
  jitterlisp_object bindings = JITTERLISP_EXP_C_A_CAR(cdr);
  jitterlisp_object body_forms = JITTERLISP_EXP_C_A_CDR(cdr);

  /* Use an extended environment, which will be added to incrementally in the
     case of let and let*, or filled right now with temporary bindings for every
     bound variable in the case of letrec . */
  jitterlisp_object body_env = env;
  if (kind == jitterlisp_let_kind_rec)
    {
      jitterlisp_object bindings_rest = bindings;
      while (! JITTERLISP_IS_EMPTY_LIST (bindings_rest))
        {
          if (! JITTERLISP_IS_CONS(bindings_rest))
            jitterlisp_error_cloned ("letrec bindings not a list");
          jitterlisp_object binding = JITTERLISP_EXP_C_A_CAR(bindings_rest);
          if (! JITTERLISP_IS_CONS(bindings_rest))
            jitterlisp_error_cloned ("letrec binding not a list");
          jitterlisp_object variable = JITTERLISP_EXP_C_A_CAR(binding);

          /* Add a temporary binding, non-destructively.  The bound value
             is temporary and will be overridden by the actual variable
             definition below. */
          body_env = jitterlisp_environment_bind (body_env, variable,
                                                  JITTERLISP_UNDEFINED);

          bindings_rest = JITTERLISP_EXP_C_A_CDR(bindings_rest);
        }
    }

  /* Evalate each variable binding and add to the environment. */
  while (! JITTERLISP_IS_EMPTY_LIST (bindings))
    {
      /* Bind binding subforms to C variables. */
      if (kind != jitterlisp_let_kind_rec /* Already traversed for letrec . */
          && ! JITTERLISP_IS_CONS(bindings))
        jitterlisp_error_cloned ("let/let* bindings not a list");
      jitterlisp_object first_binding = JITTERLISP_EXP_C_A_CAR(bindings);
      if (kind != jitterlisp_let_kind_rec /* Already traversed for letrec . */
          && ! JITTERLISP_IS_CONS(first_binding))
        jitterlisp_error_cloned ("let/let* binding not a list");
      jitterlisp_object binding_variable
        = JITTERLISP_EXP_C_A_CAR(first_binding);
      if (! JITTERLISP_IS_SYMBOL(binding_variable))
        jitterlisp_error_cloned ("let/let*/letrec binding variable not a symbol");
      jitterlisp_object binding_forms = JITTERLISP_EXP_C_A_CDR(first_binding);

      /* Evaluate the binding forms in the appropriate environment; which
         environemnt that is depends on the let kind. */
      jitterlisp_object binding_forms_env;
      switch (kind)
        {
        case jitterlisp_let_kind_ordinary:
          /* Use the environment which was active out of the block, with
             no bound variables visible. */
          binding_forms_env = env;
          break;
        case jitterlisp_let_kind_star:
        case jitterlisp_let_kind_rec:
          /* Use the same environment we are going to use for the body.
             In the case of let* this will only have the *previous*
             bindings already visible; in the case of letrec every binding
             is aready there, even if the values may change. */
          binding_forms_env = body_env;
          break;
        default:
          jitter_fatal ("invalid let kind");
        }
      jitterlisp_object binding_result
        = jitterlisp_eval_interpreter_begin (binding_forms, binding_forms_env);

      /* Bind the variable in the extended environment, according to the let
         kind. */
      switch (kind)
        {
        case jitterlisp_let_kind_ordinary:
        case jitterlisp_let_kind_star:
          /* Extend the environment for the body with a new binding. */
          body_env = jitterlisp_environment_bind (body_env, binding_variable,
                                                  binding_result);
          break;
        case jitterlisp_let_kind_rec:
          /* Destructively update the existing environment, which is the same
             for bound expressions and the body. */
          jitterlisp_environment_set (body_env, binding_variable,
                                      binding_result);
          break;
        default:
          jitter_fatal ("invalid let kind");
        }

      /* Go on with the next binding. */
      bindings = JITTERLISP_EXP_C_A_CDR(bindings);
    }

  /* Evaluate the body in the extended environment. */
  return jitterlisp_eval_interpreter_begin (body_forms, body_env);
}

static jitterlisp_object
jitterlisp_eval_interpreter_let (jitterlisp_object cdr,
                                 jitterlisp_object env)
{
  return jitterlisp_eval_interpreter_let_variant (cdr, env,
                                                  jitterlisp_let_kind_ordinary);
}

static jitterlisp_object
jitterlisp_eval_interpreter_letrec (jitterlisp_object cdr,
                                    jitterlisp_object env)
{
  return jitterlisp_eval_interpreter_let_variant (cdr, env,
                                                  jitterlisp_let_kind_rec);
}

static jitterlisp_object
jitterlisp_eval_interpreter_letstar (jitterlisp_object cdr,
                                     jitterlisp_object env)
{
  return jitterlisp_eval_interpreter_let_variant (cdr, env,
                                                  jitterlisp_let_kind_star);
}

static jitterlisp_object
jitterlisp_eval_interpreter_quote (jitterlisp_object cdr,
                                   jitterlisp_object env)
{
  if (! JITTERLISP_IS_CONS(cdr))
    jitterlisp_error_cloned ("quote not followed by a cons");
  jitterlisp_object cddr = JITTERLISP_EXP_C_A_CDR(cdr);
  if (! JITTERLISP_IS_EMPTY_LIST(cddr))
    jitterlisp_error_cloned ("invalid quote argument");
  jitterlisp_object cadr = JITTERLISP_EXP_C_A_CAR(cdr);

  return cadr;
}

static jitterlisp_object
jitterlisp_eval_interpreter_setb (jitterlisp_object cdr,
                                  jitterlisp_object env)
{
  if (! JITTERLISP_IS_CONS(cdr))
    jitterlisp_error_cloned ("set! not followed by a cons");
  jitterlisp_object variable = JITTERLISP_EXP_C_A_CAR(cdr);
  if (! JITTERLISP_IS_SYMBOL(variable))
    jitterlisp_error_cloned ("set! not followed by a symbol");
  jitterlisp_object after_variable_forms = JITTERLISP_EXP_C_A_CDR(cdr);
  jitterlisp_object new_value
    = jitterlisp_eval_interpreter_begin (after_variable_forms, env);
  jitterlisp_environment_set (env, variable, new_value);

  return JITTERLISP_NOTHING;
}

static jitterlisp_object
jitterlisp_eval_interpreter_while (jitterlisp_object cdr,
                                   jitterlisp_object env)
{
  if (! JITTERLISP_IS_CONS(cdr))
    jitterlisp_error_cloned ("while not followed by a cons");
  jitterlisp_object guard = JITTERLISP_EXP_C_A_CAR(cdr);
  jitterlisp_object body = JITTERLISP_EXP_C_A_CDR(cdr);

  while (! JITTERLISP_IS_FALSE (jitterlisp_eval_interpreter (guard, env)))
    jitterlisp_eval_interpreter_begin (body, env);
  return JITTERLISP_NOTHING;
}

/* Evaluate the given operands in the given environment, and return the result
   of the application of the given (already evaluated) primitive to them.  This
   is different from the conventional apply function used in Scheme interpreters
   in that the operands are not already evaluated; the advantage is avoiding a
   temporary list.
   The noinline attribute is important here: this function invokes a primitive C
   function in what would syntactically look like a tail context, but passing it
   a pointer to local storage as argument; that prevents GCC from compiling the
   call as a sibling call optimization, which in itself is a very minor loss.
   However having the body of this function inlined in
   jitterlisp_eval_interpreter_call would prevent sibling call compilation in
   the case of *closure* calls as well, leaking stack space in an unacceptable
   way for deeply nested tail calls.
   Tested with a GCC 8 snapshot from early October 2017. */
__attribute__ ((noinline))
static jitterlisp_object
jitterlisp_eval_interpreter_call_primitive (jitterlisp_object primitive,
                                            jitterlisp_object unevaluated_actuals,
                                            jitterlisp_object env)
{
  /* Keep the primitive fields into local C variables. */
  struct jitterlisp_primitive *untagged_primitive
    = JITTERLISP_PRIMITIVE_DECODE(primitive);
  jitter_uint in_arity = untagged_primitive->in_arity;
  jitterlisp_primitive_function function = untagged_primitive->function;

  /* Evaluate unevaluated_actuals into a temporary array.  The array will remain
     allocated until the primitive function returns: primitive calls do not
     happen in a C tail context, which should not be a problem. */
  jitterlisp_object actual_results [JITTERLISP_PRIMITIVE_MAX_IN_ARITY];
  int next_actual_index = 0;
  while (! JITTERLISP_IS_EMPTY_LIST(unevaluated_actuals))
    {
      if (! JITTERLISP_IS_CONS(unevaluated_actuals))
        jitterlisp_error_cloned ("primitive call actuals not a list");
      jitterlisp_object actual = JITTERLISP_EXP_C_A_CAR(unevaluated_actuals);
      jitterlisp_object actual_result
        = jitterlisp_eval_interpreter (actual, env);
      actual_results [next_actual_index ++] = actual_result;

      unevaluated_actuals = JITTERLISP_EXP_C_A_CDR(unevaluated_actuals);
    }

  /* Check whether the in-arity has been respected. */
  if (next_actual_index != in_arity)
    {
      printf ("About "); jitterlisp_print_to_stream (stdout, primitive); printf ("\n"); // FIXME: add to the error message
      jitterlisp_error_cloned ("too many or too few primitive actuals");
    }

  /* Call the function.  Since ther function receives a pointer to local
     automatic storage GCC shouldn't consider this a sibling call.  If it does
     I'm screwed. */
  return function (actual_results);
}

/* Evaluate the given operands in the given environment, and return the result
   of the application of the given (already evaluated) closure to them.  This is
   different from the conventional apply function used in Scheme interpreters in
   that the operands are not already evaluated; the advantage is avoiding a
   temporary list. */
static jitterlisp_object
jitterlisp_eval_interpreter_call_closure (jitterlisp_object closure,
                                          jitterlisp_object unevaluated_actuals,
                                          jitterlisp_object env)
{
  /* Keep the closure fields into local C variables. */
  struct jitterlisp_closure *untagged_closure
    = JITTERLISP_CLOSURE_DECODE(closure);
  jitterlisp_object formals = untagged_closure->formals;
  jitterlisp_object closure_environment = untagged_closure->environment;
  jitterlisp_object body_forms = untagged_closure->body;

  /* Evaluate each actual, and bind its formal to it in a new (temporary)
     environment, starting from the closure environment.  We can assume that the
     environment is an a-list and omit tag checks. */
  jitterlisp_object body_environment = closure_environment;
  while (! JITTERLISP_IS_EMPTY_LIST(unevaluated_actuals))
    {
      if (! JITTERLISP_IS_CONS(unevaluated_actuals))
        jitterlisp_error_cloned ("closure call actuals not a list");
      if (JITTERLISP_IS_EMPTY_LIST(formals))
        jitterlisp_error_cloned ("too many closure call actuals");
      jitterlisp_object formal = JITTERLISP_EXP_C_A_CAR(formals);
      jitterlisp_object actual = JITTERLISP_EXP_C_A_CAR(unevaluated_actuals);
      jitterlisp_object actual_result
        = jitterlisp_eval_interpreter (actual, env);
      body_environment
        = jitterlisp_environment_bind (body_environment, formal, actual_result);

      formals = JITTERLISP_EXP_C_A_CDR(formals);
      unevaluated_actuals = JITTERLISP_EXP_C_A_CDR(unevaluated_actuals);
    }

  if (! JITTERLISP_IS_EMPTY_LIST(formals))
    jitterlisp_error_cloned ("not enough actuals for closure");

  /* Evaluate the global body in the environment we have extended. */
  return jitterlisp_eval_interpreter_begin (body_forms, body_environment);
}

/* Call the given operator, already evaluated into either a primitive or a
   closure, with the given operands still to evaluate in the given environment.
   Return the result. */
static jitterlisp_object
jitterlisp_eval_interpreter_call_evaluated_operator
   (jitterlisp_object evaluated_operator,
    jitterlisp_object unevaluated_actuals,
    jitterlisp_object env)
{
  /* In either case the actuals have not been evaluated yet. */
  if (JITTERLISP_IS_CLOSURE(evaluated_operator))
    return jitterlisp_eval_interpreter_call_closure (evaluated_operator,
                                                     unevaluated_actuals,
                                                     env);
  else if (JITTERLISP_IS_PRIMITIVE(evaluated_operator))
    return jitterlisp_eval_interpreter_call_primitive (evaluated_operator,
                                                       unevaluated_actuals,
                                                       env);
  else
    {
      printf ("About "); jitterlisp_print_to_stream (stdout, evaluated_operator); printf ("\n"); // FIXME: add to the error message
      jitterlisp_error_cloned ("call: non-closure non-primitive operator");
    }
}

/* Evaluate the given operator and operands in the given environment, and return
   the result of their application.  This is different from the conventional
   apply function used in Scheme meta-circular interpreters in that the operands
   (and operator) are not pre-evaluated here; the advantage is avoiding a
   temporary list. */
static jitterlisp_object
jitterlisp_eval_interpreter_call (jitterlisp_object operator,
                                  jitterlisp_object actuals,
                                  jitterlisp_object env)
{
  /* Evaluate the operator.  Unless there is a type error the operator will be
     either a closure or a primitive. */
  jitterlisp_object operator_result
    = jitterlisp_eval_interpreter (operator, env);
  return jitterlisp_eval_interpreter_call_evaluated_operator (operator_result,
                                                              actuals,
                                                              env);
}

static jitterlisp_object
jitterlisp_eval_interpreter_quasiquote (jitterlisp_object cdr,
                                        jitterlisp_object env)
{
  if (! JITTERLISP_IS_CONS(cdr))
    jitterlisp_error_cloned ("quasiquote arguments not a cons");
  if (! JITTERLISP_IS_EMPTY_LIST(JITTERLISP_EXP_C_A_CDR(cdr)))
    jitterlisp_error_cloned ("more than one quasiquote arguments");
  jitterlisp_object arg = JITTERLISP_EXP_C_A_CAR(cdr);

  /* qq_call will be (quasiquote-procedure (quote arg)) . */
  // This is certainly optimizable.
  jitterlisp_object qq_name = jitterlisp_object_quasiquote_procedure;
  jitterlisp_object quote_name = jitterlisp_object_quote;
  jitterlisp_object qq_call
    = jitterlisp_list_2 (qq_name,
                         jitterlisp_list_2 (quote_name,
                                            arg));
  jitterlisp_object expansion
    = jitterlisp_eval_interpreter (qq_call, env);
  return jitterlisp_eval_interpreter (expansion, env);
}

static jitterlisp_object
jitterlisp_eval_interpreter_cons_of_symbol (jitterlisp_object symbol,
                                            jitterlisp_object cdr,
                                            jitterlisp_object env)
{
  /* First check whether the symbol is bound in the environment, and in that
     case use it as a procedure.  There are no reserved words in JitterLisp, and
     everything is re-definable. */ // FIXME: do I want key words instead?
  if (jitterlisp_environment_has (env, symbol))
    return jitterlisp_eval_interpreter_call (symbol, cdr, env);

  /* Check if the symbol is the name of a special form.  If so evaluate the
     special form thru its helper. */
  if (symbol == jitterlisp_object_begin)
    return jitterlisp_eval_interpreter_begin (cdr, env);
  else if (symbol == jitterlisp_object_call)
    return jitterlisp_eval_interpreter_call (jitterlisp_car (cdr),
                                             jitterlisp_cdr (cdr),
                                             env);
  else if (symbol == jitterlisp_object_cond)
    return jitterlisp_eval_interpreter_cond (cdr, env);
  else if (symbol == jitterlisp_object_current_environment)
    return jitterlisp_eval_interpreter_current_environment (cdr, env);
  else if (symbol == jitterlisp_object_define)
    return jitterlisp_eval_interpreter_define (cdr, env);
  else if (symbol == jitterlisp_object_if)
    return jitterlisp_eval_interpreter_if (cdr, env);
  else if (symbol == jitterlisp_object_lambda)
    return jitterlisp_eval_interpreter_lambda (cdr, env);
  else if (symbol == jitterlisp_object_let)
    return jitterlisp_eval_interpreter_let (cdr, env);
  else if (symbol == jitterlisp_object_letrec)
    return jitterlisp_eval_interpreter_letrec (cdr, env);
  else if (symbol == jitterlisp_object_letstar)
    return jitterlisp_eval_interpreter_letstar (cdr, env);
  else if (symbol == jitterlisp_object_quasiquote)
    return jitterlisp_eval_interpreter_quasiquote (cdr, env);
  else if (symbol == jitterlisp_object_quote)
    return jitterlisp_eval_interpreter_quote (cdr, env);
  else if (symbol == jitterlisp_object_setb)
    return jitterlisp_eval_interpreter_setb (cdr, env);
  else if (symbol == jitterlisp_object_while)
    return jitterlisp_eval_interpreter_while (cdr, env);

  /* If we arrived here the operator is unknown. */
  printf ("About "); // FIXME: add to the error message
  jitterlisp_print_to_stream (stdout, symbol);
  printf (":\n");
  jitterlisp_error_cloned ("unbound operator");
}




/* Non-Jittery interpreter: main function.
 * ************************************************************************** */

jitterlisp_object
jitterlisp_eval_interpreter___ (jitterlisp_object original_form,
                             jitterlisp_object env)
{
  jitterlisp_object form = jitterlisp_macroexpand (original_form, env);
  printf ("Macroexpanded\n  ");
  jitterlisp_print_to_stream (stdout, original_form);
  printf ("\ninto\n  ");
  jitterlisp_print_to_stream (stdout, form);
  printf ("\n");

  if (jitterlisp_is_self_evaluating (form))
    return form;

  if (JITTERLISP_IS_SYMBOL(form))
    return jitterlisp_environment_lookup (env, form);

  if (JITTERLISP_IS_CONS(form))
    {
      jitterlisp_object car = JITTERLISP_EXP_C_A_CAR(form);
      jitterlisp_object cdr = JITTERLISP_EXP_C_A_CDR(form);
      if (JITTERLISP_IS_SYMBOL(car))
        return jitterlisp_eval_interpreter_cons_of_symbol (car, cdr, env);
      else
        return jitterlisp_eval_interpreter_call(car, cdr, env);
    }

  jitterlisp_error_cloned ("eval: this should never happen");
}




/* Non-Jittery interpreter: AST evaluation helpers for primitives and closures.
 * ************************************************************************** */

/* Return the evaluation of the given primitive on the given (of course still
   unevaluated) operand ASTs.  Assume that the rator argument is an encoded
   primitive, and that rand_asts is a C array of operand_no elements.
   The noinline attribute is important here: this function invokes a primitive C
   function in what would syntactically look like a tail context, but passing it
   a pointer to local storage as argument; that prevents GCC from compiling the
   call as a sibling call optimization, which in itself is a very minor loss.
   However having the body of this function inlined in
   jitterlisp_eval_interpreter_call would prevent sibling call compilation in
   the case of *closure* calls as well, leaking stack space in an unacceptable
   way for deeply nested tail calls.
   Tested with a GCC 8 snapshot from early October 2017. */
__attribute__ ((noinline))
static jitterlisp_object
jitterlisp_eval_interpreter_ast_primitive (jitterlisp_object rator,
                                           const jitterlisp_object *rand_asts,
                                           size_t rand_no,
                                           jitterlisp_object env)
{
  /* FIXME: this, and likely this C function signature as well, will need to
     change with an exact-pointer-finding GC.
     Evaluate primitive actuals into a temporary array which is large enough for
     the actuals of any primitive.  Don't bother initializing the elements we
     don't actually use.  If the AST has been built correctly the primitive
     in-arity is correct, so we don't need to check it now at run time. */
  jitterlisp_object values [JITTERLISP_PRIMITIVE_MAX_IN_ARITY];
  int i;
  for (i = 0; i < rand_no; i ++)
    values [i] = jitterlisp_eval_interpreter_ast (rand_asts [i], env);

  return JITTERLISP_PRIMITIVE_DECODE(rator)->function (values);
}

/* Return the result of the given call in the given environment.  The operator
   is an AST, still to evaluate, and the operands are tagged ASTs in the given
   number; the operator comes first in the array.  The operator may evaluate to
   a primitive, a closure, or something else, in which case this function errors
   out cleanly. */
static inline jitterlisp_object
jitterlisp_eval_interpreter_ast_call
   (const jitterlisp_object *rator_and_rand_asts,
    size_t rator_and_rand_no,
    jitterlisp_object env)
{
  /* First evaluate the operator. */
  jitterlisp_object rator_value
    = jitterlisp_eval_interpreter_ast (rator_and_rand_asts [0], env);

  /* Check the operator tag.  If it's a primitive use the primitive helper
     function (but checking in-arity first, as this hasn't been checked at AST
     construction time), otherwise evaluate the operands into an extended
     environment and use eval. */
  if (JITTERLISP_IS_PRIMITIVE(rator_value))
    {
      const int expected_in_arity_plus_1
        = JITTERLISP_PRIMITIVE_DECODE(rator_value)->in_arity + 1;
      if (expected_in_arity_plus_1 < rator_and_rand_no)
        {
          printf ("About "); // FIXME: add to the error message
          jitterlisp_print_to_stream (stdout, rator_value);
          printf (":\n");
          jitterlisp_error_cloned ("primitive call: too many actuals");
        }
      else if (expected_in_arity_plus_1 > rator_and_rand_no)
        {
          printf ("About "); // FIXME: add to the error message
          jitterlisp_print_to_stream (stdout, rator_value);
          printf (":\n");
          jitterlisp_error_cloned ("primitive call: not enough actuals");
        }

      return jitterlisp_eval_interpreter_ast_primitive (rator_value,
                                                        rator_and_rand_asts + 1,
                                                        rator_and_rand_no - 1,
                                                        env);
    }
  if (! JITTERLISP_IS_CLOSURE(rator_value))
    {
      printf ("About "); // FIXME: add to the error message
      jitterlisp_print_to_stream (stdout, rator_value);
      printf (":\n");
      jitterlisp_error_cloned ("call: non-primitive non-closure operator");
    }

  /* If we arrived here the operator is a closure.  Evaluate actuals binding
     them to the closure formals, in order, starting from the closure
     environment.  Unfortunately we have to check the arity at run time,
     differently from the primitive case. */
  struct jitterlisp_closure *closure = JITTERLISP_CLOSURE_DECODE(rator_value);
  jitterlisp_object formals = closure->formals;
  jitterlisp_object body_env = closure->environment;
  int i;
  for (i = 1; i < rator_and_rand_no; i ++)
    {
      if (JITTERLISP_IS_EMPTY_LIST(formals))
        jitterlisp_error_cloned ("call: too many actuals");

      jitterlisp_object rand_value =
        jitterlisp_eval_interpreter_ast (rator_and_rand_asts [i], env);
      jitterlisp_object formal = JITTERLISP_EXP_C_A_CAR(formals);
      body_env = jitterlisp_environment_bind (body_env, formal, rand_value);

      formals = JITTERLISP_EXP_C_A_CDR(formals);
    }
  if (! JITTERLISP_IS_EMPTY_LIST(formals))
    jitterlisp_error_cloned ("call: not enough actuals");

  /* Return the evaluation of the closure body in the extended closure
     environment. */
  jitterlisp_object body_ast = closure->body;
  return jitterlisp_eval_interpreter_ast (body_ast, body_env);
}




/* Non-Jittery interpreter: AST evaluation.
 * ************************************************************************** */

jitterlisp_object
jitterlisp_eval_interpreter_ast (jitterlisp_object o,
                                 jitterlisp_object env)
{
  /* No need to validate o: if it comes from macroexpansion it's definitely an
     encoded AST, and its subs are well-formed as well. */
  const struct jitterlisp_ast *ast = JITTERLISP_AST_DECODE(o);
  const jitter_uint sub_no = ast->sub_no;
  const jitterlisp_object * const subs = ast->subs;
  /*
  printf ("jitterlisp_eval_interpreter_ast (case %i, %i subs): ",
          (int) ast->case_, (int) sub_no);
  jitterlisp_print_to_stream (stdout, JITTERLISP_AST_ENCODE(ast));
  printf ("\n");
  */
  switch (ast->case_)
    {
    case jitterlisp_ast_case_literal:
      return subs [0];

    case jitterlisp_ast_case_variable:
      return jitterlisp_environment_lookup (env, subs [0]);

    case jitterlisp_ast_case_define:
      {
        struct jitterlisp_symbol *unencoded_variable
          = JITTERLISP_SYMBOL_DECODE(subs [0]);
        jitterlisp_object defined_value
          = jitterlisp_eval_interpreter_ast (subs [1], env);
        unencoded_variable->global_value = defined_value;
        return JITTERLISP_NOTHING;
      }

    case jitterlisp_ast_case_if:
      {
        jitterlisp_object condition_result
          = jitterlisp_eval_interpreter_ast (subs [0], env);
        jitterlisp_object branch
          = (JITTERLISP_IS_FALSE(condition_result)
             ? subs [2]
             : subs [1]);
        return jitterlisp_eval_interpreter_ast (branch, env);
      }

    case jitterlisp_ast_case_setb:
      {
        jitterlisp_object bound_value
          = jitterlisp_eval_interpreter_ast (subs [1], env);
        jitterlisp_environment_set (env, subs [0], bound_value);
        return JITTERLISP_NOTHING;
      }

    case jitterlisp_ast_case_while:
      {
        const jitterlisp_object guard = subs [0];
        const jitterlisp_object body = subs [1];
        while (! JITTERLISP_IS_FALSE (jitterlisp_eval_interpreter_ast (guard,
                                                                       env)))
          jitterlisp_eval_interpreter_ast (body, env);
        return JITTERLISP_NOTHING;
      }

    case jitterlisp_ast_case_primitive:
      return jitterlisp_eval_interpreter_ast_primitive (subs [0],
                                                        subs + 1,
                                                        sub_no - 1,
                                                        env);

    case jitterlisp_ast_case_call:
      return jitterlisp_eval_interpreter_ast_call (subs, sub_no, env);

    case jitterlisp_ast_case_lambda:
      {
        /* Notice that the lambda formals are already stored as a list of
           symbols in the AST, differently from other AST cases; that is an
           optimization to make this closure initialization faster. */
        jitterlisp_object res;
        JITTERLISP_CLOSURE_(res, env, subs [0], subs [1]);
        return res;
      }

    case jitterlisp_ast_case_let:
      {
        /* Evaluate each binding in env, but at the same time build another
           environment equal to env extended it with the new bindings. */
        jitterlisp_object body_env = env;
        int i;
        const int binding_no = sub_no - 1;
        for (i = 0; i < binding_no; i += 2)
          {
            jitterlisp_object bound_value =
              jitterlisp_eval_interpreter_ast (subs [i + 1], env);
            body_env
              = jitterlisp_environment_bind (body_env, subs [i], bound_value);
          }

        /* Evaluate the body in the extended environment. */
        return jitterlisp_eval_interpreter_ast (subs [sub_no - 1], body_env);
      }

    case jitterlisp_ast_case_sequence:
      jitterlisp_eval_interpreter_ast (subs [0], env);
      return jitterlisp_eval_interpreter_ast (subs [1],env);

    case jitterlisp_ast_case_current_environment:
      return env;

    default:
      printf ("About "); // FIXME: add to the error message
      jitterlisp_print_to_stream (stdout, o);
      printf (":\n");
      jitterlisp_error_cloned ("eval: invalid or unimplemented AST case");
    }
}




/* Non-Jittery interpreter: user API.
 * ************************************************************************** */

jitterlisp_object
jitterlisp_eval_globally_interpreter (jitterlisp_object unexpanded_form)
{
  return jitterlisp_eval_interpreter (unexpanded_form,
                                      jitterlisp_empty_environment);
}

jitterlisp_object
jitterlisp_eval_interpreter (jitterlisp_object unexpanded_form,
                             jitterlisp_object env)
{
  printf ("Macroexpanding ");
  jitterlisp_print_to_stream (stdout, unexpanded_form);
  printf ("...\n");
  jitterlisp_object ast = jitterlisp_macroexpand (unexpanded_form, env);
  printf ("...into ");
  jitterlisp_print_to_stream (stdout, ast);
  printf ("\n");
  return jitterlisp_eval_interpreter_ast (ast, env);
}
