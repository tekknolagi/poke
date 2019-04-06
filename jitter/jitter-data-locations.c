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

#include <jitter/machine/jitter-machine.h>


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

/* Given a pointer to a datum location with already filled name and location,
   set its register field. */
static void
jitter_data_location_set_register (struct jitter_data_location *location)
{
  location->register_ = jitter_data_location_is_register (location->location);
}

struct jitter_data_locations *
jitter_make_data_locations (const char *beginning)
{
  /* First pass: find how many entries there are, by counting non-empty strings
     up to the final empty string used as a terminator.  They must come in an
     even number, since each entry contains one name and one location. */
  size_t string_no = 0;
  size_t string_length;
  const char *s = beginning;
  while ((string_length = strlen (s)) != 0)
    {
      s += string_length + 1;
      string_no ++;
    }
  if (string_no % 2 != 0)
    jitter_fatal ("impossible: data locations are odd in number");
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
  s = beginning;
  while ((string_length = strlen (s)) != 0)
    {
      if (name)
        location->name = s;
      else
        {
          location->location = s;
          jitter_data_location_set_register (location);
          location ++;
        }
      s += string_length + 1;
      string_no ++;
      name = ! name;
    }

  /* Done. */
  return res;
}

void
jitter_destroy_data_locations (struct jitter_data_locations *locations)
{
  free (locations->data_locations);
  free (locations);
}
