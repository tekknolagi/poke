/* Jittery structured language example: code generator common machinery.

   Copyright (C) 2017 Luca Saiu
   Written by Luca Saiu

   This file is part of the Jitter structured-language example, distributed
   along with Jitter under the same license.

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


#ifndef STRUCTURED_CODE_GENERATOR_H_
#define STRUCTURED_CODE_GENERATOR_H_

#include "structuredvm-vm.h"
#include "structured-syntax.h"

/* Core type definitions.
 * ************************************************************************** */

/* The type definition for a register index.  It is a very good idea for this to
   be signed, to make it possible to represent special invalid values as well. */
typedef int structured_register_index; // FIXME: do I want and need this?




/* Compile-time environment: initialization, finalization.
 * ************************************************************************** */

/* The compile-time environment data structure, as an abstract type. */
struct structured_static_environment;

/* Return a pointer to a fresh static environment. */
struct structured_static_environment*
structured_static_environment_make (void);

/* Free resources for the pointed static environment. */
void
structured_static_environment_destroy (struct structured_static_environment *e);




/* Compile-time environment: access.
 * ************************************************************************** */

/* Bind a new variable with the given name in the static environment,
   associating it to a currently unused register.  Return the register index for
   the new variable.
   The given name may shadow another existing variable. */
structured_register_index
structured_static_environment_bind (struct structured_static_environment *e,
                                    const structured_variable v);

/* Unbind the last variable which was bound and not yet unbound.  This works in
   a strictly LIFO fashion: only the last recently bound variable can be
   unbound. */
void
structured_static_environment_unbind (struct structured_static_environment *e);

/* Return non-false iff the given variable is bound in the pointed
   environment. */
bool
structured_static_environment_has (struct structured_static_environment *e,
                                   const structured_variable v);

/* Return the register index associated to the given variable in the pointed
   environment.  If a variable name has been bound multiple time, return the
   register from the most recent binding.
   Fail fatally if no binding for the variable exists. */
structured_register_index
structured_static_environment_lookup (struct structured_static_environment *e,
                                      const structured_variable v);

/* Return a register index which is unused in the pointed environment. */
structured_register_index
structured_static_environment_fresh_register (struct
                                              structured_static_environment *e);

#endif // #ifndef STRUCTURED_CODE_GENERATOR_H_
