/* Jittery Lisp: heap allocation header.

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


#ifndef JITTERLISP_ALLOCATION_H_
#define JITTERLISP_ALLOCATION_H_

#include <stdlib.h>

#include <jitter/jitter-hash.h>

#include "jitterlisp-sexpression.h"




/* Cons allocation.
 * ************************************************************************** */

/* Expand to an rvalue of type struct jitterlisp_cons * whose evaluation points
   to a just allocated and uninitialized cons. */
#define JITTERLISP_CONS_MAKE_UNINITIALIZED_UNENCODED()                     \
  /* FIXME: this works, but of course the implementation is temporary. */  \
  ((struct jitterlisp_cons*)                                               \
   (jitterlisp_allocate (sizeof (struct jitterlisp_cons))))




/* Symbol allocation.
 * ************************************************************************** */

/* Uninterned symbols are allocated like any garbage-collected data structures;
   interned symbols have to work differently, since their identity depends on
   whether another symbol with the same name is already interned.

   Interned symbols are currently *not* garbage-collected. */




/* Uninterned symbol allocation.
 * ************************************************************************** */

/* Expand to an rvalue of type struct jitterlisp_symbol * whose evaluation
   points to a just-allocated and uninitialized symbol, not meant to be interned.
   Notice that the name_or_NULL field must be set to NULL for an uninterned symbol,
   but this macro does not do that. */
#define JITTERLISP_SYMBOL_UNINTERNED_MAKE_UNINITIALIZED_UNENCODED()        \
  /* FIXME: this works, but of course the implementation is temporary. */  \
  ((struct jitterlisp_symbol*)                                             \
   (jitterlisp_allocate (sizeof (struct jitterlisp_symbol))))

/* Return a pointer to a fresh unencoded uninterned symbol, already correctly
   initialized. */
struct jitterlisp_symbol *
jitterlisp_symbol_make_uninterned (void)
  __attribute__ ((returns_nonnull));




/* Interned symbol allocation.
 * ************************************************************************** */

/* Return a symbol with the given name (which must be non-NULL), either
   interning a new one if no other symbol with the same name is already
   interned, and returning the previously interned symbol with the same name
   otherwise.
   The returned symbol is allocated with malloc.  Right now there is no
   facility to garbage-collect interned symbols, which live as long as the
   symbol table lives, until finalization.
   The string pointed by the argument is not shared with the symbol and the
   caller may destroy it after this function returns. */
struct jitterlisp_symbol *
jitterlisp_symbol_make_interned (const char *name)
  __attribute__ ((nonnull (1), returns_nonnull));




/* Fallback allocation.
 * ************************************************************************** */

/* This is a slow fallback allocation facility, for objects whose size is only
   known at run time.  FIXME: for most heap-allocated objects, such as conses,
   some other facility should be provided, based on CPP macros. */

/* Return an unencoded pointer to a buffer of uninitialized memory with the
   given size.

   The size must be a multiple of the minimum required alignment in bytes. */
char *
jitterlisp_allocate (size_t size_in_bytes)
  __attribute__ ((returns_nonnull, malloc));




/* Not for the user: initialization and finalization of the memory subsystem.
 * ************************************************************************** */

/* The functions here are not for the user to call directly.  The user is
   supposed to initialize and finalize every JitterLisp subsystem at the same
   time by calling jitterlisp_initialize and jitterlisp_finalize , which in
   their turn will call these functions as needed. */

/* Initialize the memory subsystem.  It's forbidden to heap-allocate any Lisp
   object until this function has been called. */
void
jitterlisp_memory_initialize (void);

/* Finalize the memory subsystem.  It's forbidden to heap-allocate any Lisp
   object after this function has been called, until
   jitterlisp_memory_initialize is called again. */
void
jitterlisp_memory_finalize (void);


#endif // #ifndef JITTERLISP_ALLOCATION_H_
