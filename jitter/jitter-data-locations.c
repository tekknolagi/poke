/* Jitter: data locations.

   Copyright (C) 2019 Luca Saiu
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
#include <string.h>

#include <jitter/jitter-data-locations.h>
#include <jitter/jitter-malloc.h>
#include <jitter/jitter-fatal.h>
#include <jitter/jitter-specialize.h> /* For special specialized instructions. */


/* Low-level debugging features relying on assembly: data locations.
 * ************************************************************************** */

/* Given a location as a string, return non-false iff the location represents a
   register. */
static bool
jitter_data_location_is_register (const char *location)
{
  /* Take some characters which can only occur within memory operands in
     assembly notation.  This default covers every architecture I know of, but
     if some architecture requires something different it suffices to define the
     macro JITTER_MEMORY_OPERAND_DISTINGUISHING_CHARACTERS in the appropriate
     machine.h file to support it. */
  char *memory_only_characters
#ifdef JITTER_MEMORY_OPERAND_DISTINGUISHING_CHARACTERS
    = JITTER_MEMORY_OPERAND_DISTINGUISHING_CHARACTERS
#else
    = "[(@";
#endif
  size_t memory_only_characters_no = strlen (memory_only_characters);

  /* Check every memory-only character.  If the string contains any of them,
     then it represents memory.  If it contains none of them, it represents a
     register. */
  int i;
  for (i = 0; i < memory_only_characters_no; i ++)
    if (strchr (location, memory_only_characters [i]) != NULL)
      return false;
  return true;
}

struct jitter_data_locations *
jitter_make_data_locations (const struct jitter_vm *vm)
{
  const char *s;
  size_t string_length;
  size_t string_no = 0;
#ifndef JITTER_DISPATCH_SWITCH
  /* First pass: find how many entries there are, by counting non-empty strings
     up to the final empty string used as a terminator.  They must come in an
     even number, since each entry contains one name and one location. */
  s = vm->data_locations;
  while ((string_length = strlen (s)) != 0)
    {
      s += string_length + 1;
      string_no ++;
    }
  if (string_no % 2 != 0)
    jitter_fatal ("impossible: data locations are odd in number");
#endif // #ifndef JITTER_DISPATCH_SWITCH
  size_t entry_no = string_no / 2;

  /* Allocate the result.  The actual strings point to memory from the constant
     bytes emitted by inline assembly, so there is no need to allocate them
     dynamically. */
  struct jitter_data_locations *res
    = jitter_xmalloc (sizeof (struct jitter_data_locations));
  res->data_locations
    = jitter_xmalloc (entry_no * sizeof (struct jitter_data_location));
  res->data_location_no = entry_no;

  /* Second pass: fill entries in the result array. */
  bool name = true;
  struct jitter_data_location *location = res->data_locations;
#ifndef JITTER_DISPATCH_SWITCH
  s = vm->data_locations;
#else // switch dispatch
  s = ""; /* End immediately. */
#endif // #ifndef JITTER_DISPATCH_SWITCH
  while ((string_length = strlen (s)) != 0)
    {
      if (name)
        location->name = s;
      else
        {
          location->location = s;
          location->register_
            = jitter_data_location_is_register (location->location);
          location ++;
        }
      s += string_length + 1;
      string_no ++;
      name = ! name;
    }

  /* The result is reliable as long as the !DATALOCATIONS special specialized
     instruction has the same size as the !NOP special specialized instruction.
     In other words, there must be no loads or moves in the compiled code for
     !DATALOCATIONS . */
  res->reliable
#ifndef JITTER_DISPATCH_SWITCH
    = (vm->thread_sizes [jitter_specialized_instruction_opcode_DATALOCATIONS]
       == vm->thread_sizes [jitter_specialized_instruction_opcode_NOP]);
#else  // switch dispatch
    = true;
#endif // #ifndef JITTER_DISPATCH_SWITCH

  /* Done. */
  return res;
}

void
jitter_destroy_data_locations (struct jitter_data_locations *locations)
{
  free (locations->data_locations);
  free (locations);
}




/* Data locations: human-readable output.
 * ************************************************************************** */

void
jitter_dump_data_locations (FILE *out, const struct jitter_vm *vm)
{
  struct jitter_data_locations *locations = jitter_make_data_locations (vm);
  if (! locations->reliable)
    {
      fprintf (out, "The following information is unreliable: at least\n");
      fprintf (out, "one datum needs more than one load instruction to be\n");
      fprintf (out, "accessed.\n");
      if (JITTER_ARCHITECTURE_IS ("sh"))
        {
          fprintf (out, "This might happen, on SH, because of the\n");
          fprintf (out, "restricted load offset ranges.\n");
        }
      else
        fprintf (out, "This should never happen.\n");
    }
  int i;
  size_t register_no = 0;
  for (i = 0; i < locations->data_location_no; i ++)
    {
      fprintf (out, "%2i. %24s: %-12s (%s)\n",
               i,
               locations->data_locations [i].name,
               locations->data_locations [i].location,
               locations->data_locations [i].register_ ? "register" : "memory");
      if (locations->data_locations [i].register_)
        register_no ++;
    }
  if (locations->data_location_no > 0)
    {
      int register_percentage
        = (register_no * 100) / locations->data_location_no;
      fprintf (out, "Register ratio: %i%%\n", register_percentage);
    }
  else
    fprintf (out, "Register ratio: undefined\n");
  fprintf (out, "\n");
  fflush (stderr);
  jitter_destroy_data_locations (locations);
}
