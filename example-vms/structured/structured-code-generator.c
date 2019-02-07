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
#include <jitter/jitter-fatal.h>

#include "structured-code-generator.h"


/* Compile-time environment: definitions.
 * ************************************************************************** */

/* A binding for a variable, as known at compile time. */
struct structured_binding
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
  /* A dynamic array of struct structured_binding elements.  This
     is used as a stack, with the top on the right (high indices) at the bottom
     on the left (low indices). */
  struct jitter_dynamic_buffer bindings;
};




/* Compile-time environment: initialization and finalization.
 * ************************************************************************** */

/* Initialize the pointed static-environment struct. */
static void
structured_static_environment_initialize (struct structured_static_environment *e)
{
  jitter_dynamic_buffer_initialize (& e->bindings);
}

/* Finalize the pointed static-environment struct, without freeing the struct
   itself. */
static void
structured_static_environment_finalize (struct structured_static_environment *e)
{
  jitter_dynamic_buffer_finalize (& e->bindings);
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




/* Compile-time environment: binding access utility.
 * ************************************************************************** */

/* Given a pointer to static environment return an initial pointer to they
   payload in bindings, which is much more convenient for read access.
   The returned pointer remains valid until no more bindings are added or
   removed. */
static struct structured_binding *
structured_static_environment_to_bindings (struct structured_static_environment
                                           *e)
{
  return ((struct structured_binding *) e->bindings.region);
}

/* Given a pointer to a static environment return the number of bindings. */
static size_t
structured_static_environment_to_binding_no (const struct
                                             structured_static_environment *e)
{
  return (e->bindings.used_size / sizeof (struct structured_binding));
}

/* Given a pointer to a static environment return a pointer to the last binding.
   At least one binding is supposed to exist. */
static struct structured_binding *
structured_static_environment_last_binding (struct structured_static_environment
                                            *e)
{
  size_t binding_no = structured_static_environment_to_binding_no (e);
  if (binding_no < 1)
    jitter_fatal ("structured_static_environment_last_binding: "
                  "empty static environment");
  struct structured_binding *bindings
    = structured_static_environment_to_bindings (e);
  return bindings + (binding_no - 1);
}

/* Add a new binding to the binding buffer in the pointed static environment,
   and return a pointer to it.  The pointer will remain valid until other
   bindings are pushed or popped. */
static struct structured_binding*
structured_static_environment_push_binding (struct structured_static_environment
                                            *e)
{
  struct jitter_dynamic_buffer *bindings = & e->bindings;
  return jitter_dynamic_buffer_reserve
           (bindings, sizeof (struct structured_binding));
}

/* Remove the last binding from the binding buffer in the pointed static
   environment. */
static void
structured_static_environment_pop_binding (struct structured_static_environment
                                           *e)
{
  jitter_dynamic_buffer_pop (& e->bindings,
                             sizeof (struct structured_binding));
}




/* Compile-time environment: accessors.
 * ************************************************************************** */

structured_register_index
structured_static_environment_bind (struct structured_static_environment *e,
                                    const structured_variable v)
{
  structured_register_index new_register
    = structured_static_environment_fresh_register (e);
  struct structured_binding *new_binding
    = structured_static_environment_push_binding (e);
  new_binding->variable = v;
  new_binding->register_index = new_register;
  //fprintf (stderr, "+ Binding %s to %i\n", new_binding->variable, new_binding->register_index);
  return new_register;
}

void
structured_static_environment_unbind (struct structured_static_environment *e)
{
  struct structured_binding *last_binding __attribute__ ((unused)) = structured_static_environment_last_binding (e);
  //fprintf (stderr, "- Unbinding %s from %i\n", last_binding->variable, last_binding->register_index);
  structured_static_environment_pop_binding (e);
}

bool
structured_static_environment_has (struct structured_static_environment *e,
                                   const structured_variable v)
{
  size_t binding_no = structured_static_environment_to_binding_no (e);
  struct structured_binding *bindings
    = structured_static_environment_to_bindings (e);
  int i;
  for (i = binding_no - 1; i >= 0; i --)
    if (! strcmp (bindings [i].variable, v))
      return true;
  return false;
}

structured_register_index
structured_static_environment_lookup (struct structured_static_environment *e,
                                      const structured_variable v)
{
  /* Look for the register index associated to v in the binding array, starting
     from the end: the most recent binding shadows any previous binding. */
  size_t binding_no = structured_static_environment_to_binding_no (e);
  struct structured_binding *bindings
    = structured_static_environment_to_bindings (e);
  int i;
  for (i = binding_no - 1; i >= 0; i --)
    if (! strcmp (bindings [i].variable, v))
      return bindings [i].register_index;

  /* If we arrived here then v is unbound. */
  jitter_fatal ("unbound variable %s", v);
}

structured_register_index
structured_static_environment_fresh_register (struct
                                              structured_static_environment *e)
{
  /* First pass: look for the highest-index currently used register.  Notice
     that highest must be signed, as I need to initialize the maximum to -1.
     See the comment below. */
  structured_register_index highest = -1;
  size_t binding_no = structured_static_environment_to_binding_no (e);
  struct structured_binding *bindings
    = structured_static_environment_to_bindings (e);
  int i;
  for (i = 0; i < binding_no; i ++)
    if (bindings [i].register_index > highest)
      highest = bindings [i].register_index;

  /* Second pass: make a boolean array indexed by register indices, saying which
     register is being used, and fill it.  The result we are looking for will be
     highest + 1, or smaller; therefore highest + 2 is a safe array size.
     Notice that if no register is being used at this time then highest will be -1,
     and array_size will be 1: there's no need for a special case. */
  size_t array_size = highest + 2;
  bool *used_registers = jitter_xmalloc (array_size * sizeof (bool));
  memset (used_registers, 0, array_size * sizeof (bool));
  for (i = 0; i < binding_no; i ++)
    used_registers [bindings [i].register_index] = true;

  /* Third pass: look for the smallest unused register using the Boolean array. */
  for (i = 0; i < array_size; i ++)
    if (! used_registers [i])
      {
        free (used_registers);
        return i;
      }
  jitter_fatal ("boolean array not marking any register as unused: bug");
}
