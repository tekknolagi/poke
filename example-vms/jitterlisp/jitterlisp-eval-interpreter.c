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

#include <string.h>  // For strcmp .  FIXME: Probably not needed in the end.

#include <jitter/jitter-cpp.h>
#include <jitter/jitter-dynamic-buffer.h>
#include <jitter/jitter-malloc.h>
#include <jitter/jitter-string.h> // for jitter_clone_string: possibly to remove.

#include "jitterlisp.h"


/* Interpreter utility.
 * ************************************************************************** */

/* Return non-false iff the given argument is a list of symbols, possibly
   empty. */
static bool
jitterlisp_is_list_of_symbols (jitterlisp_object o)
{
  while (! JITTERLISP_IS_EMPTY_LIST (o))
    {
      if (! JITTERLISP_IS_CONS (o))
        return false;
      jitterlisp_object car = JITTERLISP_EXP_C_A_CAR(o);
      jitterlisp_object cdr = JITTERLISP_EXP_C_A_CDR(o);
      if (! JITTERLISP_IS_SYMBOL (car))
        return false;
      o = cdr;
    }
  return true;
}

/* Return a fresh cons of the given car and cdr. */
static inline jitterlisp_object
jitterlisp_cons (jitterlisp_object car, jitterlisp_object cdr)
{
  jitterlisp_object res;
  JITTERLISP_CONS_(res, car, cdr);
  return res;
}

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

/* Return the encoded car of the given encoded cons, tag-checking. */
static inline jitterlisp_object
jitterlisp_car (jitterlisp_object cons)
{
  if (! JITTERLISP_IS_CONS(cons))
    jitterlisp_error_cloned ("car of non-cons");
  else
    return JITTERLISP_EXP_C_A_CAR(cons);
}

/* Return the encoded cdr of the given encoded cons, tag-checking. */
static inline jitterlisp_object
jitterlisp_cdr (jitterlisp_object cons)
{
  if (! JITTERLISP_IS_CONS(cons))
    jitterlisp_error_cloned ("cdr of non-cons");
  else
    return JITTERLISP_EXP_C_A_CDR(cons);
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




/* Environments.
 * ************************************************************************** */

/* This data structure holds a binding from variable to value representing a
   non-global environment.  Non-global means local (procedure arguments, let)
   plus non-local (locals from outer static contexts) variables.  Global
   variables are handled differently, with a value directly stored in the symbol
   data structure.  Non-global bindings have precedence over global bindings.

   Variables are encoding as symbols and compared by identity.  This
   functionality is for this compilation unit's internal use, not exported in a
   header: the VM implementation will need something similar but not identical,
   and I still have to figure out the details.

   This is an ordinary a-list implemented as an s-expression; set! modifies it
   destructively.  An inefficient but very simple solution. */

/* The empty non-global environment. */
static const jitterlisp_object
jitterlisp_empty_environment = JITTERLISP_EMPTY_LIST;

/* Return an expanded non-global environment, sharing structure with the given
   one, binding the given name to the given value.  The given environment is
   not modified. */
static jitterlisp_object
jitterlisp_environment_bind (jitterlisp_object env, jitterlisp_object name,
                             jitterlisp_object value)
{
  return jitterlisp_cons (jitterlisp_cons (name, value), env);
}

/* Return the value bound to the given name in the local environment and,
   failing that, in the global environment.  Error out if the name is not bound
   in the global environment either. */
static jitterlisp_object
jitterlisp_environment_lookup (jitterlisp_object env, jitterlisp_object name)
{
  /* First look for a binding in the local environment, which is to say look
     for the first cons in env whose car is equal-by-identity to name... */
  jitterlisp_object env_rest;
  for (env_rest = env;
       env_rest != JITTERLISP_EMPTY_LIST;
       env_rest = JITTERLISP_EXP_C_A_CDR(env_rest))
    {
      jitterlisp_object next_cons = JITTERLISP_EXP_C_A_CAR(env_rest);
      jitterlisp_object next_name = JITTERLISP_EXP_C_A_CAR(next_cons);
      if (next_name == name)
        return JITTERLISP_EXP_C_A_CDR(next_cons);
    }

  /* ...The symbol is not bound in the given local environment.  Look it up as a
     global. */
  struct jitterlisp_symbol *unencoded_name = JITTERLISP_SYMBOL_DECODE(name);
  jitterlisp_object res = unencoded_name->global_value;
  if (JITTERLISP_IS_UNDEFINED(res))
    {
      printf ("About "); jitterlisp_print_to_stream (stdout, name); printf ("\n"); // FIXME: add to the error message
      jitterlisp_error_cloned ("unbound variable");
    }
  else
    return res;
}

/* Return non-false iff the given environment is bound to the given name. */
static bool
jitterlisp_environment_has (jitterlisp_object env, jitterlisp_object name)
{
  /* First look for a binding in the local environment, which is to say look
     for the first cons in env whose car is equal-by-identity to name... */
  jitterlisp_object env_rest;
  for (env_rest = env;
       env_rest != JITTERLISP_EMPTY_LIST;
       env_rest = JITTERLISP_EXP_C_A_CDR(env_rest))
    {
      jitterlisp_object next_cons = JITTERLISP_EXP_C_A_CAR(env_rest);
      jitterlisp_object next_name = JITTERLISP_EXP_C_A_CAR(next_cons);
      if (next_name == name)
        return true;
    }

  /* ...The symbol is not bound in the given local environment.  Look it up as a
     global. */
  struct jitterlisp_symbol *unencoded_name = JITTERLISP_SYMBOL_DECODE(name);
  return ! JITTERLISP_IS_UNDEFINED(unencoded_name->global_value);
}

/* Destructively update the first binding for the given name in the given
   non-global environment, setting it to the given new value.  If the name is
   not bound in the non-global environment then modify the global binding. */
static void
jitterlisp_environment_set (jitterlisp_object env, jitterlisp_object name,
                            jitterlisp_object new_value)
{
  /* First look for a binding in the local environment, which is to say look
     for the first cons in env whose car is equal-by-identity to name... */
  jitterlisp_object env_rest;
  for (env_rest = env;
       env_rest != JITTERLISP_EMPTY_LIST;
       env_rest = JITTERLISP_EXP_C_A_CDR(env_rest))
    {
      jitterlisp_object useless __attribute__ ((unused));
      jitterlisp_object next_cons = JITTERLISP_EXP_C_A_CAR(env_rest);
      jitterlisp_object next_name = JITTERLISP_EXP_C_A_CAR(next_cons);
      if (next_name == name)
        {
          JITTERLISP_SET_CDRB_(useless, next_cons, new_value);
          return;
        }
    }

  /* ...The symbol is not bound in the given local environment.  Change its
     global binding. */
  struct jitterlisp_symbol *unencoded_name = JITTERLISP_SYMBOL_DECODE(name);
  unencoded_name->global_value = new_value;
}




/* Non-Jittery interpreter helpers.
 * ************************************************************************** */

/* Forward-declaration: eval the given form in the given local environment. */
static jitterlisp_object
jitterlisp_eval_interpreter (jitterlisp_object form, jitterlisp_object env);

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

static inline jitterlisp_object
jitterlisp_eval_interpreter_let_or_let_star (jitterlisp_object cdr,
                                             jitterlisp_object env,
                                             bool star)
{
  /* Bind subforms to C variables. */
  if (! JITTERLISP_IS_CONS(cdr))
    jitterlisp_error_cloned ("let or let* not followed by a cons");
  jitterlisp_object bindings = JITTERLISP_EXP_C_A_CAR(cdr);
  jitterlisp_object body_forms = JITTERLISP_EXP_C_A_CDR(cdr);

  /* Build an extended environment by evaluating binding forms. */
  jitterlisp_object body_env = env;
  while (! JITTERLISP_IS_EMPTY_LIST (bindings))
    {
      /* Bind binding subforms to C variables. */
      if (! JITTERLISP_IS_CONS(bindings))
        jitterlisp_error_cloned ("let or let* bindings not a list");
      jitterlisp_object first_binding = JITTERLISP_EXP_C_A_CAR(bindings);
      if (! JITTERLISP_IS_CONS(first_binding))
        jitterlisp_error_cloned ("let or let* binding not a list");
      jitterlisp_object binding_variable
        = JITTERLISP_EXP_C_A_CAR(first_binding);
      if (! JITTERLISP_IS_SYMBOL(binding_variable))
        jitterlisp_error_cloned ("let or let* binding variable not a symbol");
      jitterlisp_object binding_forms = JITTERLISP_EXP_C_A_CDR(first_binding);

      /* Evaluate the binding forms in the appropriate environment; which one
         depends on whether this is a let or let* block. */
      jitterlisp_object binding_forms_env
        = star ? body_env : env;
      jitterlisp_object binding_result
        = jitterlisp_eval_interpreter_begin (binding_forms, binding_forms_env);

      /* Add a binding for the variable in the extended environment. */
      body_env = jitterlisp_environment_bind (body_env, binding_variable,
                                              binding_result);

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
  return jitterlisp_eval_interpreter_let_or_let_star (cdr, env, false);
}

static jitterlisp_object
jitterlisp_eval_interpreter_let_star (jitterlisp_object cdr,
                                      jitterlisp_object env)
{
  return jitterlisp_eval_interpreter_let_or_let_star (cdr, env, true);
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

static jitterlisp_object
jitterlisp_eval_interpreter_call_primitive (jitterlisp_object primitive,
                                            jitterlisp_object actuals,
                                            jitterlisp_object env)
{
  /* Keep the primitive fields into local C variables. */
  struct jitterlisp_primitive *untagged_primitive
    = JITTERLISP_PRIMITIVE_DECODE(primitive);
  jitter_uint in_arity = untagged_primitive->in_arity;
  jitterlisp_primitive_function function = untagged_primitive->function;

  /* Evaluate actuals into a temporary array.  The array will remain allocated
     until the primitive function returns: primitive calls do not happen in a C
     tail context, which should not be a problem. */
  jitterlisp_object actual_results [JITTERLISP_PRIMITIVE_MAX_IN_ARITY];
  int next_actual_index = 0;
  while (! JITTERLISP_IS_EMPTY_LIST(actuals))
    {
      if (! JITTERLISP_IS_CONS(actuals))
        jitterlisp_error_cloned ("primitive call actuals not a list");
      jitterlisp_object actual = JITTERLISP_EXP_C_A_CAR(actuals);
      jitterlisp_object actual_result
        = jitterlisp_eval_interpreter (actual, env);
      actual_results [next_actual_index ++] = actual_result;

      actuals = JITTERLISP_EXP_C_A_CDR(actuals);
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

/* Evaluate the given operator and operands in the given environment, and return
   the result of their application.  This is different from the conventional
   apply function used in Scheme interpreters in that the operator and operands
   are not already evaluated; the advantage is avoiding a temporary list. */
static jitterlisp_object
jitterlisp_eval_interpreter_call_closure (jitterlisp_object closure,
                                          jitterlisp_object actuals,
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
  while (! JITTERLISP_IS_EMPTY_LIST(actuals))
    {
      if (! JITTERLISP_IS_CONS(actuals))
        jitterlisp_error_cloned ("closure call actuals not a list");
      if (JITTERLISP_IS_EMPTY_LIST(formals))
        jitterlisp_error_cloned ("too many closure call actuals");
      jitterlisp_object formal = JITTERLISP_EXP_C_A_CAR(formals);
      jitterlisp_object actual = JITTERLISP_EXP_C_A_CAR(actuals);
      jitterlisp_object actual_result
        = jitterlisp_eval_interpreter (actual, env);
      body_environment
        = jitterlisp_environment_bind (body_environment, formal, actual_result);

      formals = JITTERLISP_EXP_C_A_CDR(formals);
      actuals = JITTERLISP_EXP_C_A_CDR(actuals);
    }

  if (! JITTERLISP_IS_EMPTY_LIST(formals))
    jitterlisp_error_cloned ("not enough actuals for closure");

  /* Evaluate the global body in the environment we have extended. */
  return jitterlisp_eval_interpreter_begin (body_forms, body_environment);
}

/* Evaluate the given operator and operands in the given environment, and return
   the result of their application.  This is different from the conventional
   apply function used in Scheme meta-circular interpreters in that the operands
   are pre-evaluated into a list; the advantage is avoiding a temporary list. */
static jitterlisp_object
jitterlisp_eval_interpreter_call (jitterlisp_object operator,
                                  jitterlisp_object actuals,
                                  jitterlisp_object env)
{
  /* Evaluate the operator.  Unless there is a type error the operator will be
     either a closure or a primitive. */
  jitterlisp_object operator_result
    = jitterlisp_eval_interpreter (operator, env);

  /* In either case the actuals have not been evaluated yet. */
  if (JITTERLISP_IS_CLOSURE(operator_result))
    return jitterlisp_eval_interpreter_call_closure (operator_result,
                                                     actuals,
                                                     env);
  else if (JITTERLISP_IS_PRIMITIVE(operator_result))
    return jitterlisp_eval_interpreter_call_primitive (operator_result,
                                                       actuals,
                                                       env);
  else
    {
      printf ("About "); jitterlisp_print_to_stream (stdout, operator_result); printf ("\n"); // FIXME: add to the error message
      jitterlisp_error_cloned ("call: non-closure non-primitive operator");
    }
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
  else if (symbol == jitterlisp_object_let_star)
    return jitterlisp_eval_interpreter_let_star (cdr, env);
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

static jitterlisp_object
jitterlisp_eval_interpreter (jitterlisp_object form, jitterlisp_object env)
{
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




/* Non-Jittery interpreter: user API.
 * ************************************************************************** */

jitterlisp_object
jitterlisp_eval_globally_interpreter (jitterlisp_object form)
{
  return jitterlisp_eval_interpreter (form, jitterlisp_empty_environment);
}
