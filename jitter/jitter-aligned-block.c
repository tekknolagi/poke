/* Jitter: allocated heap memory blocks.

   Copyright (C) 2020 Luca Saiu
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


#include <jitter/jitter-aligned-block.h>

#include <jitter/jitter-fatal.h>

/* Include the right header, according to the selected implementation. */
#if defined (JITTER_ALIGNED_BLOCK_USE_ALIGNED_ALLOC)
# include <stdlib.h>
#elif defined (JITTER_ALIGNED_BLOCK_USE_POSIX_MEMALIGN)
# include <stdlib.h>
#elif defined (JITTER_ALIGNED_BLOCK_USE_FALLBACK)
# include <jitter/jitter-malloc.h>
# include <jitter/jitter-bitwise.h>
#else
# error "no aligned block implementation defined.  This should never happen."
#endif




/* Aligned block allocation and destruction.
 * ************************************************************************** */

void *
jitter_aligned_block_make (jitter_aligned_block_id *id,
                           size_t alignment_in_bytes, size_t size_in_bytes)
{
  void *res;
#if defined (JITTER_ALIGNED_BLOCK_USE_ALIGNED_ALLOC)
  res = aligned_alloc (alignment_in_bytes, size_in_bytes);
  id->aligned_alloced_buffer = res;
#elif defined (JITTER_ALIGNED_BLOCK_USE_POSIX_MEMALIGN)
  if (posix_memalign (& res, alignment_in_bytes, size_in_bytes) != 0)
    jitter_fatal ("posix_memalign failed");
  id->posix_memaligned_buffer = res;
#elif defined (JITTER_ALIGNED_BLOCK_USE_FALLBACK)
  /* Allocate a larger buffer which is guaranteed to contain an aligned buffer
     of the required size as a sub-buffer.  Keep a pointer to the initial
     buffer, in order to be able to free it, in the block id. */
  size_t double_size_in_bytes = size_in_bytes * 2;
  void *initial_pointer = jitter_xmalloc (double_size_in_bytes);
  res = ((void *)
         JITTER_NEXT_MULTIPLE_OF_POWER_OF_TWO ((jitter_uint) initial_pointer,
                                               alignment_in_bytes));
  id->initial_pointer = initial_pointer;
#else
# error "no aligned block implementation defined.  This should never happen."
#endif
  return res;
}

void
jitter_aligned_block_destroy (jitter_aligned_block_id id)
{
#if defined (JITTER_ALIGNED_BLOCK_USE_ALIGNED_ALLOC)
  free (id.aligned_alloced_buffer);
#elif defined (JITTER_ALIGNED_BLOCK_USE_POSIX_MEMALIGN)
  free (id.posix_memaligned_buffer);
#elif defined (JITTER_ALIGNED_BLOCK_USE_FALLBACK)
  free (id.initial_pointer);
#else
# error "no aligned block implementation defined.  This should never happen."
#endif
}
