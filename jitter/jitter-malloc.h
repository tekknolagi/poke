/* Jitter: safe malloc wrappers.

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


#ifndef JITTER_MALLOC_H_
#define JITTER_MALLOC_H_

#include <stdlib.h>


/* Gnulib "malloc" attribute workaround: disabling.
 * ************************************************************************** */

/* When Gnulib is used it may redefine "malloc" as a macro, in order to use a
   fixed version on some platforms.  That is all good, except that having it
   defined as a macro generates very distracting warnings when "malloc" is used
   as a function attribute, and breaks the intended optimization.  Let's avoid
   that. */
#ifdef malloc
# define JITTER_MALLOC_WAS_DEFINED_AS_A_MACRO  1
# define JITTER_MALLOC_PREVIOUS_DEFINITION     malloc
# undef malloc
#endif // #ifdef malloc




/* Safe malloc wrappers, not using Gnulib for minimality.
 * ************************************************************************** */

/* Allocate char_no chars with malloc and return its result, as long as it is
   non-NULL; otherwise fail fatally. */
void *
jitter_xmalloc (size_t char_no)
  __attribute__ ((malloc, returns_nonnull));

/* Allocate char_no chars with realloc in place of the pointed buffer and return
   realloc's result, as long as it is non-NULL; otherwise fail fatally. */
void *
jitter_xrealloc (void *previous, size_t char_no)
  __attribute__ ((returns_nonnull, warn_unused_result));




/* Gnulib "malloc" attribute workaround: re-enabling.
 * ************************************************************************** */

/* Restore the previous malloc redefinition from Gnulib, if any.  This assumes
   that malloc was defined without arguments, which is currently the case. */
#ifdef JITTER_MALLOC_WAS_DEFINED_AS_A_MACRO
# define malloc JITTER_MALLOC_PREVIOUS_DEFINITION
#endif


#endif // #ifndef JITTER_MALLOC_H_
