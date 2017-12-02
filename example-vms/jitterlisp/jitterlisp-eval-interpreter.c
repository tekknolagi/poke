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
    jitterlisp_error_cloned ("unbound variable");
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
      jitterlisp_object next_cons = JITTERLISP_EXP_C_A_CAR(env_rest);
      jitterlisp_object next_name = JITTERLISP_EXP_C_A_CAR(next_cons);
      if (next_name == name)
        {
          JITTERLISP_SET_CDR_(next_cons, new_value);
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
jitterlisp_eval_interpreter_set_bang (jitterlisp_object cdr,
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

/* Evaluate the given operator and operands in the given environment, and return
   the result of their application.  This is different from the conventional
   apply function used in Scheme interpreters in that the operator and operands
   are not already evaluated; the advantage is avoiding a temporary list. */
static jitterlisp_object
jitterlisp_eval_interpreter_call (jitterlisp_object operator,
                                  jitterlisp_object actuals,
                                  jitterlisp_object env)
{
  /* Evaluate the operator into a closure and keep closure fields into local C
     variables. */
  jitterlisp_object operator_result
    = jitterlisp_eval_interpreter (operator, env);
  if (! JITTERLISP_IS_CLOSURE(operator_result))
    jitterlisp_error_cloned ("call: non-closure operator");
  struct jitterlisp_closure *closure
    = JITTERLISP_CLOSURE_DECODE(operator_result);
  jitterlisp_object formals = closure->formals;
  jitterlisp_object closure_environment = closure->environment;
  jitterlisp_object body = closure->body;

  /* Evaluate each actual, and bind its formal to it in a new (temporary)
     environment, starting from the closure environment.  We can assume that the
     environment is an a-list and omit tag checks. */
  jitterlisp_object body_environment = closure_environment;
  while (! JITTERLISP_IS_EMPTY_LIST(actuals))
    {
      if (! JITTERLISP_IS_CONS(actuals))
        jitterlisp_error_cloned ("call actuals not a list");
      if (JITTERLISP_IS_EMPTY_LIST(formals))
        jitterlisp_error_cloned ("too many actuals");
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
    jitterlisp_error_cloned ("not enough actuals");

  /* Evaluate the global body in the environment we have extended. */
  return jitterlisp_eval_interpreter_begin (body, body_environment);
}

static jitterlisp_object
jitterlisp_eval_interpreter_primitive (jitterlisp_object name,
                                       jitterlisp_object actuals,
                                       jitterlisp_object env)
{
  jitterlisp_object res;
  jitterlisp_object args [10];
  int next_arg_index = 0;
#define JITTERLISP_NO_MORE_ARGS                                       \
  JITTER_BEGIN_                                                       \
    if (! JITTERLISP_IS_EMPTY_LIST(actuals))                          \
      jitterlisp_error_cloned ("too many primitive actuals");         \
  JITTER_END_
#define JITTERLISP_EVAL_ARG                                           \
  JITTER_BEGIN_                                                       \
    if (JITTERLISP_IS_EMPTY_LIST(actuals))                            \
      jitterlisp_error_cloned ("not enough primitive actuals");       \
    if (! JITTERLISP_IS_CONS(actuals))                                \
      jitterlisp_error_cloned ("primitive actuals not a list");       \
    args [next_arg_index ++]                                          \
      = jitterlisp_eval_interpreter (jitterlisp_car (actuals), env);  \
    actuals = jitterlisp_cdr (actuals);                               \
  JITTER_END_
#define JITTERLISP_EVAL_ARG_TYPED(_JITTERLISP_TYPE)                 \
  JITTER_BEGIN_                                                     \
    JITTERLISP_EVAL_ARG;                                            \
  if (! JITTER_CONCATENATE_TWO(JITTERLISP_IS_, _JITTERLISP_TYPE)(   \
           args [next_arg_index - 1]))                              \
    jitterlisp_error_cloned ("invalid type for primitive actual");  \
  JITTER_END_
#define JITTERLISP_EVAL_ARGS_0  \
  JITTER_BEGIN_                 \
    JITTERLISP_NO_MORE_ARGS;    \
  JITTER_END_
#define JITTERLISP_EVAL_ARGS_1  \
  JITTER_BEGIN_                 \
    JITTERLISP_EVAL_ARG;        \
    JITTERLISP_NO_MORE_ARGS;    \
  JITTER_END_
#define JITTERLISP_EVAL_ARGS_2  \
  JITTER_BEGIN_                 \
    JITTERLISP_EVAL_ARG;        \
    JITTERLISP_EVAL_ARG;        \
    JITTERLISP_NO_MORE_ARGS;    \
  JITTER_END_
#define JITTERLISP_EVAL_ARGS_3  \
  JITTER_BEGIN_                 \
    JITTERLISP_EVAL_ARG;        \
    JITTERLISP_EVAL_ARG;        \
    JITTERLISP_EVAL_ARG;        \
    JITTERLISP_NO_MORE_ARGS;    \
  JITTER_END_

#define JITTERLISP_EVAL_ARGS_TYPED_1(_JITTERLISP_TYPE1)  \
  JITTER_BEGIN_                                          \
    JITTERLISP_EVAL_ARG_TYPED(_JITTERLISP_TYPE1);        \
    JITTERLISP_NO_MORE_ARGS;                             \
  JITTER_END_
#define JITTERLISP_EVAL_ARGS_TYPED_2(_JITTERLISP_TYPE1,  \
                                     _JITTERLISP_TYPE2)  \
  JITTER_BEGIN_                                          \
    JITTERLISP_EVAL_ARG_TYPED(_JITTERLISP_TYPE1);        \
    JITTERLISP_EVAL_ARG_TYPED(_JITTERLISP_TYPE2);        \
    JITTERLISP_NO_MORE_ARGS;                             \
  JITTER_END_
#define JITTERLISP_EVAL_ARGS_TYPED_3(_JITTERLISP_TYPE1,  \
                                     _JITTERLISP_TYPE2,  \
                                     _JITTERLISP_TYPE3)  \
  JITTER_BEGIN_                                          \
    JITTERLISP_EVAL_ARG_TYPED(_JITTERLISP_TYPE1);        \
    JITTERLISP_EVAL_ARG_TYPED(_JITTERLISP_TYPE2);        \
    JITTERLISP_EVAL_ARG_TYPED(_JITTERLISP_TYPE3);        \
    JITTERLISP_NO_MORE_ARGS;                             \
  JITTER_END_

  struct jitterlisp_symbol *unencoded_name = JITTERLISP_SYMBOL_DECODE(name);
  char *interned_name = unencoded_name->name_or_NULL;
  if (interned_name == NULL)
    jitterlisp_error_cloned ("uninterned symbol as primitive operator");

  if (false)
    {
      /* Useless case, just to make all of the following cases start with
         "else". */
    }
  /* Type checking. */
  else if (! strcmp (interned_name, "fixnum?"))
    {
      JITTERLISP_EVAL_ARGS_1;
      JITTERLISP_FIXNUMP_(res, args [0]);
    }
  else if (! strcmp (interned_name, "character?"))
    {
      JITTERLISP_EVAL_ARGS_1;
      JITTERLISP_CHARACTERP_(res, args [0]);
    }
  else if (! strcmp (interned_name, "null?"))
    {
      JITTERLISP_EVAL_ARGS_1;
      JITTERLISP_NULLP_(res, args [0]);
    }
  else if (! strcmp (interned_name, "boolean?"))
    {
      JITTERLISP_EVAL_ARGS_1;
      JITTERLISP_BOOLEANP_(res, args [0]);
    }
  else if (! strcmp (interned_name, "symbol?"))
    {
      JITTERLISP_EVAL_ARGS_1;
      JITTERLISP_SYMBOLP_(res, args [0]);
    }
  else if (! strcmp (interned_name, "cons?"))
    {
      JITTERLISP_EVAL_ARGS_1;
      JITTERLISP_CONSP_(res, args [0]);
    }
  else if (! strcmp (interned_name, "procedure?"))
    {
      JITTERLISP_EVAL_ARGS_1;
      JITTERLISP_PROCEDUREP_(res, args [0]);
    }
  /* Arithmetic. */
  else if (! strcmp (interned_name, "+"))
    {
      JITTERLISP_EVAL_ARGS_TYPED_2(FIXNUM, FIXNUM);
      JITTERLISP_PLUS_(res, args [0], args [1]);
    }
  else if (! strcmp (interned_name, "-"))
    {
      JITTERLISP_EVAL_ARGS_TYPED_2(FIXNUM, FIXNUM);
      JITTERLISP_MINUS_(res, args [0], args [1]);
    }
  else if (! strcmp (interned_name, "*"))
    {
      JITTERLISP_EVAL_ARGS_TYPED_2(FIXNUM, FIXNUM);
      JITTERLISP_TIMES_(res, args [0], args [1]);
    }
  else if (! strcmp (interned_name, "/"))
    {
      JITTERLISP_EVAL_ARGS_TYPED_2(FIXNUM, FIXNUM);
      if (args [1] == JITTERLISP_FIXNUM_ENCODE(0))
        jitterlisp_error_cloned ("division by zero");
      JITTERLISP_DIVIDED_(res, args [0], args [1]);
    }
  else if (! strcmp (interned_name, "remainder"))
    {
      JITTERLISP_EVAL_ARGS_TYPED_2(FIXNUM, FIXNUM);
      if (args [1] == JITTERLISP_FIXNUM_ENCODE(0))
        jitterlisp_error_cloned ("remainder of division by zero");
      JITTERLISP_REMAINDER_(res, args [0], args [1]);
    }
  else if (! strcmp (interned_name, "1+"))
    {
      JITTERLISP_EVAL_ARGS_TYPED_1(FIXNUM);
      JITTERLISP_1PLUS_(res, args [0]);
    }
  else if (! strcmp (interned_name, "1-"))
    {
      JITTERLISP_EVAL_ARGS_TYPED_1(FIXNUM);
      JITTERLISP_1MINUS_(res, args [0]);
    }
  /* Boolean operations. */
  else if (! strcmp (interned_name, "not"))
    {
      JITTERLISP_EVAL_ARGS_1;
      JITTERLISP_NOT_(res, args [0]);
    }
  /* Comparison. */
  else if (! strcmp (interned_name, "eq?"))
    {
      JITTERLISP_EVAL_ARGS_2;
      JITTERLISP_EQP_(res, args [0], args [1]);
    }
  else if (! strcmp (interned_name, "neq?"))
    {
      JITTERLISP_EVAL_ARGS_2;
      JITTERLISP_NEQP_(res, args [0], args [1]);
    }
  else if (! strcmp (interned_name, "zero?"))
    {
      JITTERLISP_EVAL_ARGS_1;
      JITTERLISP_ZEROP_(res, args [0]);
    }
  else if (! strcmp (interned_name, "nzero?"))
    {
      JITTERLISP_EVAL_ARGS_1;
      JITTERLISP_NZEROP_(res, args [0]);
    }
  /* Conses. */
  else if (! strcmp (interned_name, "cons"))
    {
      JITTERLISP_EVAL_ARGS_2;
      JITTERLISP_CONS_(res, args [0], args [1]);
    }
  else if (! strcmp (interned_name, "car"))
    {
      JITTERLISP_EVAL_ARGS_TYPED_1(CONS);
      JITTERLISP_CAR_(res, args [0]);
    }
  else if (! strcmp (interned_name, "cdr"))
    {
      JITTERLISP_EVAL_ARGS_TYPED_1(CONS);
      JITTERLISP_CDR_(res, args [0]);
    }
  /* FIXME: add composed selectors. */
  /* Default. */
  else
    jitterlisp_error_cloned ("unbound primitive");


#undef JITTERLISP_EVAL_ARG
#undef JITTERLISP_EVAL_ARG_TYPED
#undef JITTERLISP_EVAL_ARGS_0
#undef JITTERLISP_EVAL_ARGS_1
#undef JITTERLISP_EVAL_ARGS_2
#undef JITTERLISP_EVAL_ARGS_3
#undef JITTERLISP_EVAL_ARGS_TYPED_1
#undef JITTERLISP_EVAL_ARGS_TYPED_2
#undef JITTERLISP_EVAL_ARGS_TYPED_3
#undef JITTERLISP_NO_MORE_ARGS
  return res;
}


static jitterlisp_object
jitterlisp_eval_interpreter_cons_of_symbol (jitterlisp_object symbol,
                                            jitterlisp_object cdr,
                                            jitterlisp_object env)
{
  /* First check whether the symbol is bound in the environment, and in that
     case use it as a procedure.  There are no reserved words in JitterLisp, and
     everything is re-definable. */
  if (jitterlisp_environment_has (env, symbol))
    return jitterlisp_eval_interpreter_call (symbol, cdr, env);

  /* Check if the symbol is the name of a special form.  If so evaluate the
     special form thru its helper. */
  if (symbol == jitterlisp_object_begin)
    return jitterlisp_eval_interpreter_begin (cdr, env);
  if (symbol == jitterlisp_object_if)
    return jitterlisp_eval_interpreter_if (cdr, env);
  else if (symbol == jitterlisp_object_lambda)
    return jitterlisp_eval_interpreter_lambda (cdr, env);
  else if (symbol == jitterlisp_object_quote)
    return jitterlisp_eval_interpreter_quote (cdr, env);
  else if (symbol == jitterlisp_object_set_bang)
    return jitterlisp_eval_interpreter_set_bang (cdr, env);
  else if (symbol == jitterlisp_object_while)
    return jitterlisp_eval_interpreter_while (cdr, env);

  else
    return jitterlisp_eval_interpreter_primitive (symbol, cdr, env);

  //  /* The symbol is unbound so it can't evaluate to a procedure, and is not the
  //     name of a special form either. */
  //  jitterlisp_error_cloned ("unbound operator");
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
