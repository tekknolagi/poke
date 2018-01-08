/* Jittery Lisp: utility function header.

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


#ifndef JITTERLISP_UTILITY_H_
#define JITTERLISP_UTILITY_H_

#include "jitterlisp-sexpression.h"




/* S-expression shape checking.
 * ************************************************************************** */

/* Return non-false iff the given s-expression is a list. */
bool
jitterlisp_is_list (jitterlisp_object o);

/* Return non-false iff the given s-expression is a list of symbols. */
bool
jitterlisp_is_list_of_symbols (jitterlisp_object o);

/* Return non-false iff the given s-expression is a list of symbols all
   different from one another.  This is O(n log(n)). */
bool
jitterlisp_is_list_of_distinct_symbols (jitterlisp_object o);

/* Return non-false iff the given s-expression is an a-list of the kind used for
   macroexpand and eval from Lisp.  This means that the a-list keys must be
   symbols. */
bool
jitterlisp_is_alist (jitterlisp_object o);

/* A macro wrapper around jitterlisp_is_list , convenient for type-checking
   primitive arguments.  This calls jitterlisp_is_list . */
#define JITTERLISP_IS_LIST(x)  \
  (jitterlisp_is_list (x))

/* A macro wrapper around jitterlisp_is_list_of_symbols , convenient for
   type-checking primitive arguments.  This calls jitterlisp_is_list_of_symbols
   . */
#define JITTERLISP_IS_SYMBOLS(x)  \
  (jitterlisp_is_list_of_symbols (x))

/* A macro wrapper around jitterlisp_is_alist , convenient for type-checking
   primitive arguments.  This calls jitterlisp_is_alist . */
#define JITTERLISP_IS_ALIST(x)  \
  (jitterlisp_is_alist (x))




/* S-expression shape validation.
 * ************************************************************************** */

/* The functions in this section do nothing on success, and error out in case of
   failure.  They all take an encoded s-expression. */

/* Error out if the given encoded s-expression is not a symbol. */
void
jitterlisp_validate_empty_list (jitterlisp_object o);

/* Error out if the given encoded s-expression is not a symbol. */
void
jitterlisp_validate_symbol (jitterlisp_object o);

/* Error out if the given encoded s-expression is not a primitive. */
void
jitterlisp_validate_primitive (jitterlisp_object o);

/* Error out if the given encoded s-expression is not an AST. */
void
jitterlisp_validate_ast (jitterlisp_object o);

/* Error out if the given encoded s-expression is not a list of symbols. */
void
jitterlisp_validate_symbols (jitterlisp_object list);

/* Error out if the given encoded s-expression is not a list of symbols all
   different from one another.  This is O(n log(n)). */
void
jitterlisp_validate_distinct_symbols (jitterlisp_object list);

/* Error out if the given encoded s-expression is not a list of ASTs. */
void
jitterlisp_validate_asts (jitterlisp_object list);

/* Error out if the given encoded s-expression is not an a-list.  This is meant
   for validating the arguments of eval and macroexpand , as used from Lisp. */
void
jitterlisp_validate_alist (jitterlisp_object o);




/* List functions.
 * ************************************************************************** */

/* Return the length of the given encoded list, unencoded.  Error out if the
   argument is not a list. */
size_t
jitterlisp_length (jitterlisp_object list);




/* S-expression constructors and selectors.
 * ************************************************************************** */

/* Return a fresh cons of the given car and cdr. */
jitterlisp_object
jitterlisp_cons (jitterlisp_object car, jitterlisp_object cdr);

/* Return the encoded car of the given encoded s-expression, if the argument is
   a cons; error out otherwise. */
jitterlisp_object
jitterlisp_car (jitterlisp_object cons);

/* Return the encoded cdr of the given encoded s-expression, if the argument is
   a cons; error out otherwise. */
jitterlisp_object
jitterlisp_cdr (jitterlisp_object cons);

/* Composed selectors. */
jitterlisp_object
jitterlisp_caar (jitterlisp_object cons);
jitterlisp_object
jitterlisp_cadr (jitterlisp_object cons);
jitterlisp_object
jitterlisp_cdar (jitterlisp_object cons);
jitterlisp_object
jitterlisp_cddr (jitterlisp_object cons);
jitterlisp_object
jitterlisp_caaar (jitterlisp_object cons);
jitterlisp_object
jitterlisp_caadr (jitterlisp_object cons);
jitterlisp_object
jitterlisp_cadar (jitterlisp_object cons);
jitterlisp_object
jitterlisp_caddr (jitterlisp_object cons);
jitterlisp_object
jitterlisp_cdaar (jitterlisp_object cons);
jitterlisp_object
jitterlisp_cdadr (jitterlisp_object cons);
jitterlisp_object
jitterlisp_cddar (jitterlisp_object cons);
jitterlisp_object
jitterlisp_cdddr (jitterlisp_object cons);

/* Return a fresh list with the given objects as elements. */
jitterlisp_object
jitterlisp_list_1 (jitterlisp_object o0);
jitterlisp_object
jitterlisp_list_2 (jitterlisp_object o0, jitterlisp_object o1);
jitterlisp_object
jitterlisp_list_3 (jitterlisp_object o0, jitterlisp_object o1,
                   jitterlisp_object o2);
jitterlisp_object
jitterlisp_list_4 (jitterlisp_object o0, jitterlisp_object o1,
                   jitterlisp_object o2, jitterlisp_object o3);





/* Non-global environments.
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
extern const jitterlisp_object
jitterlisp_empty_environment;

/* Return an expanded non-global environment, sharing structure with the given
   one, binding the given name to the given value.  The given environment is
   not modified. */
jitterlisp_object
jitterlisp_environment_bind (jitterlisp_object env, jitterlisp_object name,
                             jitterlisp_object value);

/* Return the value bound to the given name in the local environment and,
   failing that, in the global environment.  Error out if the name is not bound
   in the global environment either. */
jitterlisp_object
jitterlisp_environment_lookup (jitterlisp_object env, jitterlisp_object name);

/* Return non-false iff the given symbol is bound in either the given non-global
   environment or the global environment. */
bool
jitterlisp_environment_has (jitterlisp_object env, jitterlisp_object name);

/* Destructively update the first binding for the given name in the given
   non-global environment, or the global binding if no non-global environment
   exists (subject to the error conditions of jitterlisp_global_set, below).
   Error out if the name is not bound. */
void
jitterlisp_environment_setb (jitterlisp_object env, jitterlisp_object name,
                             jitterlisp_object new_value);

/* Destructively update the global binding for the given symbol, which must
   be already globally bound and non-constant, to the new value.  Error out
   if the name is not globally bound or is a global constant. */
void
jitterlisp_global_setb (jitterlisp_object name, jitterlisp_object new_value);

/* Like jitterlisp_global_set , but do not error out if the symbol has no
   previous binding.  Do error out if the symbol is bound to a global
   constant. */
void
jitterlisp_define (jitterlisp_object name, jitterlisp_object new_value);




#endif // #ifndef JITTERLISP_UTILITY_H_
