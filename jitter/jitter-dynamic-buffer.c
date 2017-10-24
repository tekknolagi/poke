/* Jitter: dynamic buffer data structure.

   Copyright (C) 2017 Luca Saiu
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


#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "jitter.h"

#include "jitter-malloc.h"
#include "jitter-dynamic-buffer.h"


/* Initial memory size.
 * ************************************************************************** */

/* Buffer sizes are all stored in chars, to make pointer arithmetic easier. */
#define INITIAL_ALLOCATED_SIZE 256




/* Initialization and finalization.
 * ************************************************************************** */

void
jitter_dynamic_buffer_initialize (struct jitter_dynamic_buffer *da)
{
  /* Allocate a region with the default initial size, keeping track of how much
     memory is available and how much is already used. */
  da->allocated_size = INITIAL_ALLOCATED_SIZE;
  da->used_size = 0;
  da->region = jitter_xmalloc (INITIAL_ALLOCATED_SIZE);
}

void
jitter_dynamic_buffer_finalize (struct jitter_dynamic_buffer *da)
{
  /* Free the malloc-allocated part. */
  free (da->region);

  /* For defensiveness, fill the struct with invalid data. */
  memset (da, 0xff, sizeof (struct jitter_dynamic_buffer));
}




/* Reserving or releasing memory.
 * ************************************************************************** */

void *
jitter_dynamic_buffer_reserve (struct jitter_dynamic_buffer *db,
                               size_t chars_to_reserve)
{
  /* Remember the offset of the reserved space from the region beginning. */
  size_t offset = db->used_size;

  /* Reserve space, which possibly entails reallocating the region.  This
     potentially changes the region pointer, which is why we can compute the
     result only after this. */
  db->used_size += chars_to_reserve;
  if (db->used_size > db->allocated_size)
    db->region = jitter_xrealloc (db->region,
                                  db->allocated_size = db->used_size * 2);

  /* Now we know where the reserved space begins: it comes offset chars after
     the region, as it is now. */
  return db->region + offset;
}

void *
jitter_dynamic_buffer_push (struct jitter_dynamic_buffer *db,
                            const void *new_element,
                            size_t new_element_size_in_chars)
{
  /* Reserve space, and remember where the new space begins. */
  void *res = jitter_dynamic_buffer_reserve (db, new_element_size_in_chars);

  /* Fill the space. */
  memcpy (res, new_element, new_element_size_in_chars);

  /* Return the beginning of the copy. */
  return res;
}

const void *
jitter_dynamic_buffer_push_const (struct jitter_dynamic_buffer *db,
                                  const void *new_element,
                                  size_t new_element_size_in_chars)
{
  return jitter_dynamic_buffer_push (db, new_element,
                                     new_element_size_in_chars);
}

void*
jitter_dynamic_buffer_pop (struct jitter_dynamic_buffer *db,
                           size_t chars_to_release)
{
  assert (db->used_size >= chars_to_release);
  db->used_size -= chars_to_release;
  return db->region + db->used_size;
}

const void*
jitter_dynamic_buffer_pop_const (struct jitter_dynamic_buffer *db,
                                 size_t chars_to_release)
{
  return jitter_dynamic_buffer_pop (db, chars_to_release);
}

void*
jitter_dynamic_buffer_first_unused_char (const struct jitter_dynamic_buffer *db)
{
  return db->region + db->used_size;
}

const void*
jitter_dynamic_buffer_first_unused_char_const
   (const struct jitter_dynamic_buffer *db)
{
  return jitter_dynamic_buffer_first_unused_char (db);
}

size_t
jitter_dynamic_buffer_size (const struct jitter_dynamic_buffer *db)
{
  return db->used_size;
}




/* Conversion to an ordinary pointer.
 * ************************************************************************** */

void*
jitter_dynamic_buffer_to_pointer (const struct jitter_dynamic_buffer *db)
{
  return db->region;
}

const void*
jitter_dynamic_buffer_to_const_pointer (const struct jitter_dynamic_buffer *db)
{
  return jitter_dynamic_buffer_to_pointer (db);
}
