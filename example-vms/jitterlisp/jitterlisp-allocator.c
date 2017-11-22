/* Jittery Lisp: heap allocation.

   Copyright (C) 2017 Luca Saiu
   Written by Luca Saiu

   This file is part of the Jittery Lisp language implementation, distributed as
   an example along with Jitter under the same license.

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


/* Include the Gnulib header. */
#include <config.h>

#include "jitterlisp-allocator.h"

#include <string.h>

#include <jitter/jitter-fatal.h>
#include <jitter/jitter-malloc.h>

#include "jitterlisp-sexpression.h"
#include "jitterlisp-config.h"

/* Include headers from Bohem's GC, if used in this configuration. */
#ifdef JITTERLISP_BOEHM_GC
# include <gc/gc.h>
#endif // #ifdef JITTERLISP_BOEHM_GC


/* Global register variable hack.
 * ************************************************************************** */

/////////////////////////////////////////////////////
/* FIXME: this whole idea will not work here, with separate compilation.  But I
   can uncomment the #undef below and look at the generated code. */
#undef JITTER_HAS_ASSEMBLY

#ifdef JITTER_HAS_ASSEMBLY
  /* We need architecture-specific register names. */
# include <jitter/machine/jitter-machine.h>
#endif // #ifdef JITTER_HAS_ASSEMBLY

/* Expand to a global variable declaration -- a global *register* variable
   if we have machine support.  This doesn't currently run in a Jittery VM,
   so it doesn't matter if we choose registers which are already reserved
   for other purposes. */
#if defined(JITTERLISP_LITTER) && defined(JITTER_HAS_ASSEMBLY)
# define JITTERLISP_GLOBAL_REGISTER_VARIABLE_(_jitterlisp_type,            \
                                              _jitterlisp_name,            \
                                              _jitterlisp_register_index)  \
  register _jitterlisp_type _jitterlisp_name                               \
     asm (JITTER_STRINGIFY(                                                \
            JITTER_CONCATENATE_TWO(JITTER_RESIDUAL_REGISTER_,              \
                                   _jitterlisp_register_index)))
#else
# define JITTERLISP_GLOBAL_REGISTER_VARIABLE_(_jitterlisp_type,            \
                                              _jitterlisp_name,            \
                                              _jitterlisp_register_index)  \
  static _jitterlisp_type _jitterlisp_name
#endif // #if defined(JITTERLISP_LITTER) && defined(JITTER_HAS_ASSEMBLY)




/* The symbol table.
 * ************************************************************************** */

/* A string hash mapping each interned symbol name as a C string into a
   pointer to the unique struct jitterlisp_symbol for the symbol. */
static struct jitter_hash_table
jitterlisp_symbol_table;




/* Initialization and finalization of the memory subsystem.
 * ************************************************************************** */

/* If littering then we simply use global variables for limits, and a fixed
   pre-allocated area for the heap.  This is currently suboptimal, as these
   should stay in registers; but I can live with this until a proper VM for
   JitterLisp exists. */
#ifdef JITTERLISP_LITTER
#define JITTERLISP_LITTER_BYTE_NO (10 * 1024 * 1024)

  /* The lowest heap address. */
  static char *litter_heap_beginning;

  /* The address of the next heap-allocated buffer. */
  JITTERLISP_GLOBAL_REGISTER_VARIABLE_(char *, litter_allocator_pointer, 0);

  /* The lowest address *out* of the heap area. */
  JITTERLISP_GLOBAL_REGISTER_VARIABLE_(char *, litter_allocator_limit, 1);

  ///* Temporary testing stuff. */
  //JITTERLISP_GLOBAL_REGISTER_VARIABLE_(void *, jitterlisp_error_handler_register, 2);
#endif // #ifdef JITTERLISP_LITTER

void
jitterlisp_memory_initialize (void)
{
  /* Initialize the garbage-collected heap. */
#if   defined(JITTERLISP_LITTER)
  /* Set up the allocation pointer and its limit. */
  litter_allocator_pointer
    = litter_heap_beginning
    = jitter_xmalloc (JITTERLISP_LITTER_BYTE_NO);
  litter_allocator_limit = litter_allocator_pointer + JITTERLISP_LITTER_BYTE_NO;
#elif defined(JITTERLISP_BOEHM_GC)
  /* Initialize Boehm's GC. */
  GC_init ();
#else
# error "impossible or unimplemented"
#endif // #if defined(...

  /* Initialize the symbol table. */
  jitter_hash_initialize (& jitterlisp_symbol_table);
}

/* Forward-declaration. */
static void
jitterlisp_destroy_interned_symbol (const union jitter_word w);

void
jitterlisp_memory_finalize (void)
{
  /* Finalize the symbol table, destroying every interned symbol in the
     process. */
  jitter_string_hash_finalize (& jitterlisp_symbol_table,
                               jitterlisp_destroy_interned_symbol);

  /* Finalize the garbage-collected heap. */
#if   defined(JITTERLISP_LITTER)
  free (litter_heap_beginning);
#elif defined(JITTERLISP_BOEHM_GC)
  /* Boehm's GC doesn't seem to have a global finalization function -- not
     speaking of per-object finalization, of course. */
#else
# error "impossible or unimplemented"
#endif // #if defined(...
}




/* Allocation.
 * ************************************************************************** */

char *
jitterlisp_allocate (size_t size_in_bytes)
{
#if   defined(JITTERLISP_LITTER)
  char *res = litter_allocator_pointer;
  litter_allocator_pointer += size_in_bytes;
  if (__builtin_expect (litter_allocator_pointer > litter_allocator_limit,
                        false))
    jitter_fatal ("At this point I would call the GC, if there were one.\n");
  /* printf ("After allocating %luB the next allocation pointer is %p, "
          "whose misalignment is %lu\n",
          (unsigned long) size_in_bytes,
          litter_allocator_pointer,
          ((unsigned long) litter_allocator_pointer) & JITTERLISP_ALIGNMENT_BIT_MASK
          );*/
  return res;
#elif defined(JITTERLISP_BOEHM_GC)
  return GC_malloc (size_in_bytes);
#else
# error "impossible or unimplemented"
#endif // #if defined(...
}

struct jitterlisp_symbol *
jitterlisp_symbol_make_uninterned (void)
{
  struct jitterlisp_symbol *res
    = JITTERLISP_SYMBOL_UNINTERNED_MAKE_UNINITIALIZED_UNENCODED();
  res->name_or_NULL = NULL;
  return res;
}




/* Symbols and symbol interning.
 * ************************************************************************** */

struct jitterlisp_symbol *
jitterlisp_symbol_make_interned (const char *name)
{
  if (jitter_string_hash_table_has (& jitterlisp_symbol_table,
                                    name))
    return ((struct jitterlisp_symbol *)
            (jitter_string_hash_table_get (& jitterlisp_symbol_table,
                                           name).pointer_to_void));
  else
    {
      struct jitterlisp_symbol *res
        = jitter_xmalloc (sizeof (struct jitterlisp_symbol));
      res->name_or_NULL = jitter_xmalloc (strlen (name) + 1);
      strcpy (res->name_or_NULL, name);
      union jitter_word w = { .pointer = (void *) res };
      jitter_string_hash_table_add (& jitterlisp_symbol_table, name, w);
      return res;
    }
}

/* This is used in finalization.  Notice that interned symbols are not
   garbage-collected. */
static void
jitterlisp_destroy_interned_symbol (const union jitter_word w)
{
  struct jitterlisp_symbol *symbol
    = ((struct jitterlisp_symbol *) w.pointer);

  /* Destroy the symbol name, if any. */
  if (symbol->name_or_NULL != NULL)
    free (symbol->name_or_NULL);

  /* Destroy the struct as well.  This is correct, since symbols are allocated
     with malloc. */
  free (symbol);
}
