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

#include <jitter/jitter-dynamic-buffer.h>
#include <jitter/jitter-fatal.h>
#include <jitter/jitter-malloc.h>

#include "jitterlisp-sexpression.h"
#include "jitterlisp-config.h"

/* Include headers from Bohem's GC, if used in this configuration. */
#ifdef JITTERLISP_BOEHM_GC
# define GC_THREADS 1
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




/* Thread registration.
 * ************************************************************************** */

void
jitterlisp_register_current_thread (void)
{
#if defined(JITTERLISP_BOEHM_GC)
  struct GC_stack_base stack_base;
  GC_get_stack_base (& stack_base);
  GC_register_my_thread (& stack_base);
#endif // #if defined(JITTERLISP_BOEHM_GC)
}

void
jitterlisp_unregister_current_thread (void)
{
#if defined(JITTERLISP_BOEHM_GC)
  GC_unregister_my_thread ();
#endif // #if defined(JITTERLISP_BOEHM_GC)
}




/* Explicit GC root registration.
 * ************************************************************************** */

#warning "export the public part of this.  Add comments."

struct jitterlisp_gc_root
{
  jitterlisp_object *object_pointer;
};

static struct jitter_dynamic_buffer
jitterlisp_gc_root_stack;

static void
jitterlisp_gc_root_stack_initialize (void)
{
  jitter_dynamic_buffer_initialize (& jitterlisp_gc_root_stack);
}

static void
jitterlisp_gc_root_stack_finalize (void)
{
  jitter_dynamic_buffer_initialize (& jitterlisp_gc_root_stack);
}

void
jitterlisp_push_gc_root (jitterlisp_object *object_pointer)
{
  struct jitterlisp_gc_root root;
  root.object_pointer = object_pointer;
  jitter_dynamic_buffer_push (& jitterlisp_gc_root_stack,
                              & root,
                              sizeof (struct jitterlisp_gc_root));
#if   defined(JITTERLISP_LITTER)
  /* Do nothing. */
#elif defined(JITTERLISP_BOEHM_GC)
  GC_add_roots (object_pointer, ((char *) object_pointer) + sizeof (jitterlisp_object));
#else
# error "impossible or unimplemented"
#endif // #if defined(...
}

void
jitterlisp_pop_gc_roots (size_t how_many)
{
  struct jitterlisp_gc_root *popped_roots __attribute__ ((unused))
    = jitter_dynamic_buffer_pop (& jitterlisp_gc_root_stack,
                                 sizeof (struct jitterlisp_gc_root) * how_many);
#if defined(JITTERLISP_BOEHM_GC)
  int i;
  for (i = 0; i < how_many; i ++)
    GC_remove_roots (popped_roots [i].object_pointer,
                     ((char *) (popped_roots [i].object_pointer)
                      + sizeof (jitterlisp_object)));
#endif // #if defined(JITTERLISP_BOEHM_GC)
}

void
jitterlisp_pop_gc_root (void)
{
  jitterlisp_pop_gc_roots (1);
}

void
jitterlisp_pop_all_gc_roots (void)
{
  size_t root_no
    = (jitter_dynamic_buffer_size (& jitterlisp_gc_root_stack)
       / sizeof (struct jitterlisp_gc_root));
  jitterlisp_pop_gc_roots (root_no);
}




/* Initialization and finalization of the memory subsystem.
 * ************************************************************************** */

/* If littering then we simply use global variables for limits, and large
   infrequently allocated blocks for the heap.  This is currently suboptimal, as
   these should stay in registers; but I can live with this until a proper VM
   for JitterLisp exists. */
#ifdef JITTERLISP_LITTER
#define JITTERLISP_LITTER_BLOCK_BYTE_NO (1 * 1024L * 1024L)

/* The lowest heap address. */
static char *litter_heap_beginning;

/* The address of the next heap-allocated buffer. */
JITTERLISP_GLOBAL_REGISTER_VARIABLE_(char *, litter_allocator_pointer, 0);

/* The lowest address *out* of the heap area. */
JITTERLISP_GLOBAL_REGISTER_VARIABLE_(char *, litter_allocator_limit, 1);

///* Temporary testing stuff. */
//JITTERLISP_GLOBAL_REGISTER_VARIABLE_(void *, jitterlisp_error_handler_register, 2);

/* Information about one litter block. */
struct jitterlisp_litter_block
{
  void *beginning;
  size_t size_in_bytes;
};

/* A stack of struct jitter_litter_block objects, to keep track of which
   blocks have been allocated.  Even without releasing the individual
   objects we can free the entire blocks at finalization.
   This is currently non-reentrant. */
static struct jitter_dynamic_buffer
jitterlisp_litter_blocks;

/* Make a large block from which we will allocate, without ever releasing
   individual objects, and add it to the litter block stack.  Set allocation
   pointer, heap beginning and limit to refer to the new block.  Any previous
   existing block is kept as well, and objects from one block are allowed to
   point to objects from another. */
static void
jitterlisp_add_litter_block (void)
{
  /* Set up the allocation pointer and its limit. */
  litter_allocator_pointer
    = litter_heap_beginning
    = jitter_xmalloc (JITTERLISP_LITTER_BLOCK_BYTE_NO);
  litter_allocator_limit = (litter_allocator_pointer
                            + JITTERLISP_LITTER_BLOCK_BYTE_NO);

  /* Add information about it to the stack. */
  struct jitterlisp_litter_block block;
  block.beginning = litter_heap_beginning;
  block.size_in_bytes = JITTERLISP_LITTER_BLOCK_BYTE_NO;
  jitter_dynamic_buffer_push (& jitterlisp_litter_blocks,
                              & block,
                              sizeof (struct jitterlisp_litter_block));

  /* Log, unless this block is the first. */
  size_t block_no =
    jitterlisp_litter_blocks.used_size
    / sizeof (struct jitterlisp_litter_block);
  if (block_no > 1)
    printf ("New litter block.  %i bocks, %.1fMB heap.\n",
            (int) block_no,
            JITTERLISP_LITTER_BLOCK_BYTE_NO / 1024.0 / 1024.0 * block_no);
}

static void
jitterlisp_destroy_litter_blocks (void)
{
  struct jitterlisp_litter_block *popped_blocks
    = ((struct jitterlisp_litter_block *)
       jitterlisp_litter_blocks.region);
  size_t popped_block_no = (jitterlisp_litter_blocks.used_size
                            / sizeof (struct jitterlisp_litter_block));
  int i;
  for (i = 0; i < popped_block_no; i ++)
    free (popped_blocks [i].beginning);
}
#endif // #if defined(JITTERLISP_LITTER)

void
jitterlisp_memory_initialize (void)
{
  /* Initialize the garbage-collected heap. */
#if   defined(JITTERLISP_LITTER)
printf ("Initializing blocks\n");
  /* Initialize the litter block stack. */
  jitter_dynamic_buffer_initialize (& jitterlisp_litter_blocks);

  /* Make the first litter block. */
  jitterlisp_add_litter_block ();
printf ("Made the first block\n");

#elif defined(JITTERLISP_BOEHM_GC)
  fflush (stdout); fflush (stderr); fprintf (stderr, "Initializing Boehm GC...\n"); fflush (stdout); fflush (stderr);
  /* Initialize Boehm's GC. */
  GC_INIT ();
  GC_allow_register_threads ();

  fflush (stdout); fflush (stderr); fprintf (stderr, "...Initialized Boehm GC...\n"); fflush (stdout); fflush (stderr);

#else
# error "impossible or unimplemented"
#endif // #if defined(...

  jitterlisp_register_current_thread ();

  jitterlisp_gc_root_stack_initialize ();

  /* Initialize the symbol table. */
  jitter_hash_initialize (& jitterlisp_symbol_table);
}

/* Forward-declaration. */
static void
jitterlisp_destroy_interned_symbol (const union jitter_word w);

void
jitterlisp_memory_finalize (void)
{
  jitterlisp_pop_all_gc_roots ();
  jitterlisp_gc_root_stack_finalize ();

  /* Finalize the symbol table, destroying every interned symbol in the
     process. */
  jitter_string_hash_finalize (& jitterlisp_symbol_table,
                               jitterlisp_destroy_interned_symbol);

  jitterlisp_unregister_current_thread ();

  /* Finalize the garbage-collected heap. */
#if   defined(JITTERLISP_LITTER)
  /* Destroy every litter block referred on the litter block stack. */
  jitterlisp_destroy_litter_blocks ();
  /* Finalize the stack itself. */
  jitter_dynamic_buffer_finalize (& jitterlisp_litter_blocks);
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
    jitterlisp_add_litter_block ();
  /* printf ("After allocating %luB the next allocation pointer is %p, "
          "whose misalignment is %lu\n",
          (unsigned long) size_in_bytes,
          litter_allocator_pointer,
          ((unsigned long) litter_allocator_pointer) & JITTERLISP_ALIGNMENT_BIT_MASK
          );*/
  return res;

#elif defined(JITTERLISP_BOEHM_GC)
  return GC_MALLOC (size_in_bytes);

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
  res->global_value = JITTERLISP_UNDEFINED;
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
      res->global_value = JITTERLISP_UNDEFINED;
      jitterlisp_push_gc_root (& res->global_value);
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
