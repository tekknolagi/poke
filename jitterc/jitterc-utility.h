/* Jitter: utility header for jitterc.

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


#ifndef JITTERC_UTILITY_H_
#define JITTERC_UTILITY_H_

#include <config.h>
#include <gl_list.h>


/* Gnulib list utility functions.
 * ************************************************************************** */

/* Return a fresh empty (array-) list. */
gl_list_t
jitterc_make_empty_list (void)
  __attribute__ ((returns_nonnull));

/* Return a freshly-allocated copy of the given (array-) list.  The copy
   contains a fresh spine and a copy of the elements, but such elements are
   pointers; therefore the copy shares data with the original. */
gl_list_t
jitterc_clone_list (const gl_list_t list)
  __attribute__ ((returns_nonnull, nonnull (1)));

/* Copy every element from the from into into the to list, at the end, without
   altering the original.  The to list contains a copy of the elements but such
   elements are pointers; therefore the copy shares data with the original. */
void
jitterc_clone_list_from (gl_list_t to, const gl_list_t from)
  __attribute__ ((nonnull (1, 2)));

/* Remove every element from the given list.  The elements are actually
   pointers, and the pointed objects are not freed. */
void
jitterc_empty_list (gl_list_t list)
  __attribute__ ((nonnull (1)));


#endif // #ifndef JITTERC_UTILITY_H_
