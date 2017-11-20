/* Jittery Lisp: s-expression implementation.

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


#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>

#include <jitter/jitter.h>
#include <jitter/jitter-malloc.h>

#include "jitterlisp-sexpression.h"




/* S-expression initialization and finalization.
 * ************************************************************************** */

void
jitterlisp_sexpression_initialize (void)
{
  /* Do nothing. */
}

void
jitterlisp_sexpression_finalize (void)
{
  /* Do nothing. */
}




/* S-expression representation: unique object names.
 * ************************************************************************** */

/* This must exactly follow the order in jitterlisp-sexpression.h . */
const char * const
jitterlisp_unique_object_names []
  = {
      "#f",                      /* The unique object with index 0. */
      "#t",                      /* The unique object with index 1. */
      "()",                      /* The unique object with index 2. */
      "#<eof>",                  /* The unique object with index 3. */
      "#<nothing>",              /* The unique object with index 4. */
    };
