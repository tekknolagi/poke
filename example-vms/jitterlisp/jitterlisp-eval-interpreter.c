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

#include <jitter/jitter-dynamic-buffer.h>
#include <jitter/jitter-malloc.h>
#include <jitter/jitter-string.h> // for jitter_clone_string: possibly to remove.

#include "jitterlisp.h"


/* Environments.
 * ************************************************************************** */

/* This data structure holds a binding from variable to value representing a
   non-global environment.  Non-global means local (procedure arguments, let)
   plus non-local (locals from outer static contexts) variables.  Global
   variables are handled differently, with a value directly stored in the symbol
   data structure.  Non-global bindings have precedence over global bindings.
   The most recent non-global binding has precedence over the previous ones, and
   will be the first to be removed.  As non-global bindings follow a LIFO
   discipline the underlying data structure is, unsurprisingly, a stack.

   Variables are encoding as symbols and compared by identity.  This
   functionality is for this compilation unit's internal use, not exported in a
   header: the VM implementation will need something similar but not identical,
   and I still have to figure out the details. */

/* FIXME: if I introduce a moving garbage collector this will need some
   careful checking, to ensure that symbols from an evaluation environemnts
   are correctly treated as roots. */

/* FIXME: the entire idea of doing this in C is questionable.  I will need
   garbage-collected environment anyway for closures. */

/* A non-global environment. */
struct jitterlisp_environment
{
};

/* Return a pointer to a fresh nonglobal environment structure containing no
   bindings. */
struct jitterlisp_environment*
jitterlisp_environment_make (void)
{
  struct jitterlisp_environment *res
    = jitter_xmalloc (sizeof (struct jitterlisp_environment));
  return res;
}

/* Destroy the pointed nonglobal environment. */
void
jitterlisp_environment_destroy (struct jitterlisp_environment *e)
{
  free (e);
}




/* Non-Jittery interpreter.
 * ************************************************************************** */

static jitterlisp_object
jitterlisp_eval_globally_interpreter_in
  (jitterlisp_object form,
   struct jitterlisp_environment * const env)
{
  /* FIXME: before having a real eval I can still check for memory leaks using
     Valgrind; the critical case is freeing resources on error.  Here by
     convention
     // the empty list causes a failure,
     the empty list evaluates to the character #\a ,
     symbols evaluate to the fixnum 42 ,
     booleans to a symbol
     and everything else to #<nothing>. */
  if (form == JITTERLISP_EMPTY_LIST)
    return JITTERLISP_CHARACTER_ENCODE('a');
      /*if (form == JITTERLISP_EMPTY_LIST)
    jitterlisp_error (jitter_clone_string ("the empty list is evil"));
  else*/ if (JITTERLISP_IS_SYMBOL(form))
    //return JITTERLISP_FIXNUM_ENCODE(42);
    return JITTERLISP_FIXNUM_PLUS(JITTERLISP_FIXNUM_ENCODE(40),
                                  JITTERLISP_FIXNUM_ENCODE(2));
  else if (JITTERLISP_IS_BOOLEAN(form))
    return JITTERLISP_SYMBOL_ENCODE(
              jitterlisp_symbol_make_interned("something"));
  else
    return JITTERLISP_NOTHING;
}




/* Non-Jittery interpreter: user API.
 * ************************************************************************** */

jitterlisp_object
jitterlisp_eval_globally_interpreter (jitterlisp_object form)
{
  struct jitterlisp_environment *env
    = jitterlisp_environment_make ();
  jitterlisp_object res = jitterlisp_eval_globally_interpreter_in (form, env);
  jitterlisp_environment_destroy (env);
  return res;
}
