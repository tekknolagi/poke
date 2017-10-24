/* Jitter: Forth-style stacks with optional TOS optimization: implementation.

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


#include <string.h>

#include <jitter/jitter-stack.h>
#include <jitter/jitter-malloc.h>


/* Stack backing initialization and finalization.
 * ************************************************************************** */

static void
jitter_stack_initialize_backing (struct jitter_stack_backing *backing,
                                 enum jitter_stack_optimization optimization,
                                 size_t element_size_in_bytes,
                                 size_t element_no)
{
  /* Keep debugging information. */
  backing->optimization = optimization;
  backing->element_size_in_bytes = element_size_in_bytes;
  backing->element_no = element_no;

  /* Allocate memory, or fail fatally. */
  backing->memory =
    jitter_xmalloc (element_size_in_bytes * element_no);
}

void
jitter_stack_initialize_tos_backing (struct jitter_stack_backing *backing,
                                     size_t element_size_in_bytes,
                                     size_t element_no)
{
  jitter_stack_initialize_backing (backing,
                                   jitter_stack_optimization_tos,
                                   element_size_in_bytes,
                                   element_no);
}

void
jitter_stack_initialize_ntos_backing (struct jitter_stack_backing *backing,
                                      size_t element_size_in_bytes,
                                      size_t element_no)
{
  jitter_stack_initialize_backing (backing,
                                   jitter_stack_optimization_no_tos,
                                   element_size_in_bytes,
                                   element_no);
}

void
jitter_stack_finalize_backing(struct jitter_stack_backing *backing)
{
  /* Release memory. */
  free (backing->memory);

  /* Invalidate the backing content, to catch mistakes more easily. */
  memset (backing, 0xff, sizeof (struct jitter_stack_backing));
}
