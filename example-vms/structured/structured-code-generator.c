/* Jittery structured language example: code generator common machinery.

   Copyright (C) 2107, 2019 Luca Saiu
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


#include <string.h>

#include "structured-code-generator.h"


/* Compile-time environment: definitions.
 * ************************************************************************** */

/* Each instance of this structure holds a <variable, register-index> pair. */
struct structured_variable_register_pair
{
  /* A variable name.  The pointed string is shared with the AST, and doesn't
     need to be freed here. */
  structured_variable variable;

  /* The 0-based index used to keep the variable.  This will be used as the
     index for an r-class register. */
  structured_register_index register_index;
};

/* A static environment structure contains a variable-to-register-index
   mapping.  Rationale: using a hash would have required less code and
   might even have been faster, but this idea is much easier to extend
   to nested scopes. */
struct structured_static_environment
{
  /* A dynamic array of struct structured_variable_register_pair elements. */
  struct jitter_dynamic_buffer pairs;

  /* Index of the lowest register index to be used next.  Indices are allocated
     sequentially, so the first variable will be assigned to 0, the second to 1
     and so on. */
  structured_register_index next_register_index;
};




/* Compile-time environment: initialization and finalization.
 * ************************************************************************** */

/* Initialize the pointed static-environment struct. */
static void
structured_static_environment_initialize (struct structured_static_environment *e)
{
  jitter_dynamic_buffer_initialize (& e->pairs);
  e->next_register_index = 0;
}

/* Finalize the pointed static-environment struct, without freeing the struct
   itself. */
static void
structured_static_environment_finalize (struct structured_static_environment *e)
{
  jitter_dynamic_buffer_finalize (& e->pairs);
}

struct structured_static_environment*
structured_static_environment_make (void)
{
  struct structured_static_environment *res
    = jitter_xmalloc (sizeof (struct structured_static_environment));
  structured_static_environment_initialize (res);
  return res;
}

void
structured_static_environment_destroy (struct structured_static_environment *e)
{
  structured_static_environment_finalize (e);
  free (e);
}




/* Compile-time environment: accessors.
 * ************************************************************************** */

/* Return the register-index associated to the given variable in the pointed
   environment; if no binding for the variable exists, add one. */
structured_register_index
structured_static_environment_lookup (struct structured_static_environment *e,
                                      const structured_variable v)
{
  struct structured_variable_register_pair *p
    = ((struct structured_variable_register_pair *)
       (e->pairs.region
        + e->pairs.used_size
        - sizeof (struct structured_variable_register_pair)));
  while (p >= (struct structured_variable_register_pair *) e->pairs.region)
    {
      if (! strcmp (p->variable, v))
        return p->register_index;
      p --;
    }
  struct structured_variable_register_pair *new_pair
    = jitter_dynamic_buffer_reserve
         (& e->pairs, sizeof (struct structured_variable_register_pair));
  new_pair->variable = v;
  return new_pair->register_index = e->next_register_index ++;
}
