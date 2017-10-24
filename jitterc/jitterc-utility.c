/* Jitter: utility functions for jitterc.

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


#include <config.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <xalloc.h>
#include <gl_xlist.h>
#include <gl_array_list.h>

#include <jitter/jitter-fatal.h>
#include "jitterc-utility.h"


/* Gnulib list utility functions.
 * ************************************************************************** */

gl_list_t
jitterc_make_empty_list (void)
{
  return gl_list_nx_create_empty (GL_ARRAY_LIST, NULL, NULL, NULL, true);
}

gl_list_t
jitterc_clone_list (const gl_list_t list)
{
  gl_list_t res
    = gl_list_nx_create_empty (GL_ARRAY_LIST, NULL, NULL, NULL, true);
  size_t size = gl_list_size (list);
  int i;
  for (i = 0; i < size; i ++)
    gl_list_add_last (res, gl_list_get_at (list, i));
  return res;
}

void
jitterc_clone_list_from (gl_list_t to, const gl_list_t from)
{
  size_t size = gl_list_size (from);
  int i;
  for (i = 0; i < size; i ++)
    gl_list_add_last (to, gl_list_get_at (from, i));
}

void
jitterc_empty_list (gl_list_t list)
{
  size_t size = gl_list_size (list);
  int i;
  for (i = size - 1; i >= 0; i --)
    gl_list_remove_at (list, i);
}
