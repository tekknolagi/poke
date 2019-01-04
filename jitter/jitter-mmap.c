/* Jitter: mmap abstraction.

   Copyright (C) 2018, 2019 Luca Saiu
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


/* Everything below expands to nothing where replication is disabled.  This is
   enough not to use mmap where it doesn't exist. */
#include <jitter/jitter-dispatch.h>
#ifdef JITTER_REPLICATE


#include <unistd.h>   /* For sysconf . */
#include <sys/mman.h> /* For mmap and munmap . */

#include <jitter/jitter-mmap.h>
#include <jitter/jitter-heap.h>




/* Executable memory allocation: primitive allocation and deallocation.
 * ************************************************************************** */

/* The heap block size, as per jitter/jitter-heap.h .  This is an OS page, as
   used by mmap. */
static size_t
jitter_mmap_page_size;

/* The size of a block; this must still be a power of two, and a multiple of
   jitter_mmap_page_size. */
static size_t
jitter_executable_block_size;

/* These are simple wrappers around mmap and munmap. */

/* Allocate a new buffer of the right size and return a pointer to it, or NULL.
   This function is of type jitter_heap_primitive_allocate_function .

   This allocates space for (possibly more than) an entire heap block or big
   object.  Single object allocation will be based on heap functions, and will
   use space obtained from this function. */
static void *
jitter_executable_make_block_primitive (size_t size_in_bytes)
{
  void *res = mmap (NULL,
                    size_in_bytes,
                    PROT_READ | PROT_WRITE | PROT_EXEC, // FIXME: check for W^E
                    MAP_PRIVATE | MAP_ANONYMOUS,
                    -1,
                    0);
  if (res == MAP_FAILED)
    return NULL;
  else
    return res;
}

/* Destroy the pointed buffer of the given size.  This function is of type
   jitter_heap_primitive_free_function and is used for destroying entire blocks
   and also for unmapping the unaliged part of larger blocks. */
static void
jitter_executable_destroy_block_primitive (void *allocated_memory,
                                           size_t size_in_bytes)
{
  munmap (allocated_memory, size_in_bytes);
}




/* Executable memory allocation: initialization and finalization.
 * ************************************************************************** */

/* The heap for executable code, as a global. */
static struct jitter_heap
jitter_executable_heap;

void
jitter_initialize_executable (void)
{
  /* Find the mmap page size. */
  jitter_mmap_page_size = sysconf (_SC_PAGE_SIZE);

  /* Find a sensible size of a heap block.  FIXME: this could be made smaller on
     "small" machines. */
  jitter_executable_block_size = jitter_mmap_page_size;
  while (jitter_executable_block_size < (512 * 1024))
    jitter_executable_block_size *= 2;

  /* Initialize the global heap variable. */
  jitter_heap_initialize (& jitter_executable_heap,
                          jitter_executable_make_block_primitive,
                          jitter_executable_destroy_block_primitive,
                          jitter_mmap_page_size,
                          jitter_executable_destroy_block_primitive,
                          jitter_executable_block_size);
}

void
jitter_finalize_executable (void)
{
  /* Finalize the global heap variable. */
  jitter_heap_finalize (& jitter_executable_heap);
}




/* Executable memory allocation: allocation and release.
 * ************************************************************************** */

void *
jitter_executable_allocate (size_t size_in_bytes)
{
  return jitter_heap_allocate (& jitter_executable_heap, size_in_bytes);
}

void
jitter_executable_shrink_in_place (void *object, size_t new_size_in_bytes)
{
  jitter_heap_shrink_in_place (& jitter_executable_heap, object,
                               new_size_in_bytes);
}

void
jitter_executable_deallocate (void *buffer)
{
  jitter_heap_free (& jitter_executable_heap, buffer);
}


/* End of the conditionally-enabled part, and end of the source file as well. */
#endif // #ifdef JITTER_REPLICATE
