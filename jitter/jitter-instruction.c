/* Jitter: VM-independent instruction implementation.

   Copyright (C) 2016, 2017 Luca Saiu
   Written by Luca Saiu

   This file is part of Jitter.

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


#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <jitter/jitter-instruction.h>

#include <jitter/jitter-malloc.h>
#include <jitter/jitter-fatal.h>
#include <jitter/jitter-hash.h>


/* Meta-instructions.
 * ************************************************************************** */

const struct jitter_meta_instruction*
jitter_lookup_meta_instruction (const struct jitter_hash_table *hash,
                                  const char *name)
{
  if (! jitter_string_hash_table_has (hash, name))
    jitter_fatal ("invalid instruction name \"%s\"", name);

  return jitter_string_hash_table_get (hash, name).pointer_to_void;
}

void
jitter_initialize_meta_instructions
   (struct jitter_hash_table *meta_instruction_string_hash,
    const struct jitter_meta_instruction *meta_instructions,
    size_t meta_instruction_no)
{
  jitter_hash_initialize (meta_instruction_string_hash);

  int i;
  for (i = 0; i < meta_instruction_no; i ++)
    {
      const struct jitter_meta_instruction *mi = meta_instructions + i;
      union jitter_word w = {.pointer_to_void = (void*) mi};
      jitter_string_hash_table_add (meta_instruction_string_hash,
                                    mi->name,
                                    w);
    }
}

void
jitter_finalize_meta_instructions (struct jitter_hash_table *
                                     meta_instruction_string_hash)
{
  jitter_string_hash_finalize (meta_instruction_string_hash, NULL);
}

struct jitter_instruction *
jitter_make_instruction (const struct jitter_meta_instruction * const mi)
{
  struct jitter_instruction *res
    = jitter_xmalloc (sizeof (struct jitter_instruction));
  const int arity = mi->parameter_no;;
  res->meta_instruction = mi;
  if (arity == 0)
    res->parameters = NULL;
  else
    {
      res->parameters
        = jitter_xmalloc (arity * sizeof (struct jitter_parameter*));
      int i;
      for (i = 0; i < arity; i ++)
        res->parameters[i] = jitter_make_instruction_parameter ();
    }
  return res;
}

void
jitter_destroy_instruction (struct jitter_instruction *i)
{
  if (i->parameters != NULL)
    {
      int j;
      size_t arity = i->meta_instruction->parameter_no;
      for (j = 0; j < arity; j ++)
        jitter_destroy_instruction_parameter (i->parameters [j]);
      free (i->parameters);
    }
  free (i);
}




/* Instructions
 * ************************************************************************** */

struct jitter_parameter *
jitter_make_instruction_parameter (void)
{
  struct jitter_parameter *res
    = jitter_xmalloc (sizeof (struct jitter_parameter));

  /* Try and prevent distraction mistakes by making the type field invalid. */
  res->type = jitter_parameter_type_uninitialized;

  /* Initialize the label_name field, which is critical with respect to
     allocation. */
  res->label_name = NULL;

  return res;
}

struct jitter_parameter*
jitter_clone_instruction_parameter (const struct jitter_program *program,
                                    const struct jitter_parameter *original)
{
  struct jitter_parameter *res = jitter_make_instruction_parameter ();
  memcpy (res, original, sizeof (struct jitter_parameter));
  if (res->label_name != NULL)
    {
      res->label_name = jitter_xmalloc (sizeof (res->label_name) + 1);
      strcpy (res->label_name, original->label_name);
    }
  return res;
}

void
jitter_destroy_instruction_parameter (struct jitter_parameter *p)
{
  if (p->label_name != NULL)
    free (p->label_name);

  free (p);
}


/* VM instruction parameter printing.
 * ************************************************************************** */

void
jitter_default_literal_parameter_printer (FILE *out, jitter_uint arg)
{
  fprintf (out, "0x%" JITTER_PRIx, arg);
}
