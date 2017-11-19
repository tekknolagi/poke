/* Jitter: dynamic buffer data structure header.

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


#ifndef JITTER_DYNAMIC_BUFFER_H_
#define JITTER_DYNAMIC_BUFFER_H_

#include <stdlib.h>
#include <stdbool.h>
#include "jitter.h"

/* A dynamic buffer is a structure holding a dynamically-allocated contiguous
   region of memory, resized and moved as needed.  The memory region can grow to
   contain user data added and removed in a LIFO fashion.  Any operation
   reserving space (but not operations just releasing space) may move the memory
   area, which would invalidate pointers to addresses within the dynamic memory
   region, but it is safe to point within the dynamic memory region after no
   reservation occurs.  At that point a pointer to the beginning of the dynamic
   memory region can be obtained.

   The buffer can hold homogenous data, working in practice as an array, or it
   can hold elements of varying size, under the control of the user.  The size
   of each element is *not* stored within the data structure, and the user has
   to keep track of such information if needed.

   The dynamically memory region is allocated with malloc , entails some
   alignment restrictions.  No such constraint applies to individual elements,
   which are allocated as demanded by the user without any added padding. */


/* Dynamic buffer public data structures.
 * ************************************************************************** */

/* The dynamic buffer data structure.  The content should be treated as
   opaque. */
struct jitter_dynamic_buffer
{
  /* The currently allocated space, in chars. */
  size_t allocated_size;

  /* The currently used space, in chars. */
  size_t used_size;

  /* A pointer to the beginning of the dynamically-allocated memory region.  It
     is convenient to keep this as a char pointer, to make pointer arithmetic
     easier. */
  char *region;
};





/* Initialization and finalization.
 * ************************************************************************** */

/* Initialize the pointed structure to a dynamic buffer containing zero used
   bytes. */
void
jitter_dynamic_buffer_initialize (struct jitter_dynamic_buffer *db)
  __attribute__ ((nonnull (1)));

/* Finalize the pointed structure releasing its dynamically-allocated memory,
   without freeing the structure itself. */
void
jitter_dynamic_buffer_finalize (struct jitter_dynamic_buffer *db)
  __attribute__ ((nonnull (1)));

/* Functions to also allocate and deallocate are probably not needed.  If they
   are at some point add them here and call them jitter_dynamically_array_make
   and jitter_dynamically_array_destroy . */




/* Reserving or releasing memory.
 * ************************************************************************** */

/* Reserve memory of an element of the given size within the dynamic region of
   the pointed dynamic array, without filling the space.  Return a pointer to
   the beginning of the reserved part, which will remain valid as long as no
   memory is reserved for the same dynamic buffer. */
void *
jitter_dynamic_buffer_reserve (struct jitter_dynamic_buffer *db,
                               size_t chars_to_reserve)
  __attribute__ ((returns_nonnull, nonnull (1)));

/* Reserve memory in the pointed dynamic buffer, initializing the space with a
   copy of the given element; return a pointer to the beginning of the copy in
   the dynamic region.  This is equivalent to a call to
   jitter_dynamic_buffer_reserve followed by a call to memcpy filling the
   reserved space. */
void *
jitter_dynamic_buffer_push (struct jitter_dynamic_buffer *db,
                            const void *new_element,
                            size_t new_element_size_in_chars)
  __attribute__ ((returns_nonnull, nonnull (1)));

/* Like jitter_dynamic_buffer_push , but return a pointer to constant memory. */
const void *
jitter_dynamic_buffer_push_const (struct jitter_dynamic_buffer *db,
                                  const void *new_element,
                                  size_t new_element_size_in_chars)
  __attribute__ ((returns_nonnull, nonnull (1)));

/* Release the given amount of memory from the end of the dynamic region of the
   pointed dynamic buffer, without overwriting the freed space.  Return a
   pointer to the beginning of the freed area. */
void*
jitter_dynamic_buffer_pop (struct jitter_dynamic_buffer *db,
                           size_t chars_to_release)
  __attribute__ ((returns_nonnull, nonnull (1)));

/* Like jitter_dynamic_buffer_pop, but return a pointer to constant memory. */
const void*
jitter_dynamic_buffer_pop_const (struct jitter_dynamic_buffer *db,
                                 size_t chars_to_release)
  __attribute__ ((returns_nonnull, nonnull (1)));

/* Return a pointer to the first unused byte in the dynamic memory region. */
void*
jitter_dynamic_buffer_first_unused_char (const struct jitter_dynamic_buffer *db)
  __attribute__ ((returns_nonnull, nonnull (1), pure));

/* Like jitter_dynamic_first_unused_char , but return a pointer to constant
   memory. */
const void*
jitter_dynamic_buffer_first_unused_char_const
   (const struct jitter_dynamic_buffer *db)
  __attribute__ ((returns_nonnull, nonnull (1), pure));

/* Return the current size of the used part of the dynamic region, in chars.
   The allocated size might be larger. */
size_t
jitter_dynamic_buffer_size (const struct jitter_dynamic_buffer *db)
  __attribute__ ((nonnull (1), pure));




/* Conversion to an ordinary pointer.
 * ************************************************************************** */

/* Return the address of the beginning of the dynamically-allocated region.  The
   address will remain valid as long as dynamic buffer space is not allocated or
   relased, and as long as the dynamic buffer is not finalized. */
void*
jitter_dynamic_buffer_to_pointer (const struct jitter_dynamic_buffer *db)
  __attribute__ ((returns_nonnull, nonnull (1), pure));

/* Like jitter_dynamic_buffer_to_pointer , but return a pointer to constant
   memory. */
const void*
jitter_dynamic_buffer_to_const_pointer (const struct jitter_dynamic_buffer *db)
  __attribute__ ((returns_nonnull, nonnull (1), pure));




/* Extraction.
 * ************************************************************************** */

/* "Extracting" data from a dynamic buffer means returning the malloc-allocated
   buffer contained in the dynamic buffer struct, thus making it invalid.  It is
   *incorrect* to finalize a dynamic buffer after calling this, even if of course
   the struct itself, if heap-allocated, should be freed. */

/* Return the malloc-allocated data held in the pointed dynamic buffer, which is
   then no longer usable.  It is the caller's responsibility to free the data.
   Notice that the data will have size db->allocated_size , which may be larger
   than db->used_size . */
void *
jitter_dynamic_buffer_extract (struct jitter_dynamic_buffer *db)
  __attribute__ ((nonnull (1), returns_nonnull));

/* Like jitter_dynamic_buffer_extract , but trim the returned buffer to
   db->used_size using realloc. */
void *
jitter_dynamic_buffer_extract_trimmed (struct jitter_dynamic_buffer *db)
  __attribute__ ((nonnull (1), returns_nonnull));

#endif // #ifndef JITTER_DYNAMIC_BUFFER_H_
