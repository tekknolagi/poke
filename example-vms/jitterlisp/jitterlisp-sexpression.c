/* Jittery Lisp: s-expression header.

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
#include <string.h>

#include <jitter/jitter.h>

#include "jitterlisp-sexpression.h"




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




/* S-expression printing.
 * ************************************************************************** */

// FIXME: explain the idea in a comment.
static void
jitterlisp_print_cdr (FILE *f, jitterlisp_object o)
{
  if (JITTERLISP_IS_EMPTY_LIST(o))
    {
      /* Print nothing. */
    }
  else if (JITTERLISP_IS_CONS(o))
    {
      struct jitterlisp_cons * const c = JITTERLISP_CONS_DECODE(o);
      fprintf (f, " ");
      jitterlisp_print (f, c->car);
      jitterlisp_print_cdr (f, c->cdr);
    }
  else
    {
      fprintf (f, " . ");
      jitterlisp_print (f, o);
    }
}

void
jitterlisp_print (FILE *f, jitterlisp_object o)
{
  if (JITTERLISP_IS_FIXNUM(o))
    fprintf (f, "%" JITTER_PRIi, JITTERLISP_FIXNUM_DECODE(o));
  else if (JITTERLISP_IS_UNIQUE(o))
    {
      jitter_uint index = JITTERLISP_UNIQUE_DECODE(o);
      if (index < JITTERLISP_UNIQUE_OBJECT_NO)
        fprintf (f, "%s", jitterlisp_unique_object_names [index]);
      else
        fprintf (f, "#<invalid-unique-object:%" JITTER_PRIu ">", index);
    }
  else if (JITTERLISP_IS_CHARACTER(o))
    {
      jitter_int c = JITTERLISP_CHARACTER_DECODE(o);
      switch (c)
        {
        case ' ':  fprintf (f, "#\\space");       break;
        case '\0': fprintf (f, "#\\zero");        break;
        case '\r': fprintf (f, "#\\return");      break;
        case '\n': fprintf (f, "#\\newline");     break;
        default:   fprintf (f, "#\\%c", (int) c); break;
        }
    }
  else if (JITTERLISP_IS_SYMBOL(o))
    {
      struct jitterlisp_symbol *s = JITTERLISP_SYMBOL_DECODE(o);
      if (s->name_or_NULL != NULL)
        fprintf (f, "%s", s->name_or_NULL);
      else
        fprintf (f, "#<uninterned-symbol:%p>", s);
    }
  else if (JITTERLISP_IS_CONS(o))
    {
      struct jitterlisp_cons * const c = JITTERLISP_CONS_DECODE(o);
      jitterlisp_object car = c->car;
      jitterlisp_object cdr = c->cdr;
      fprintf (f, "(");
      jitterlisp_print (f, car);
      jitterlisp_print_cdr (f, cdr);
      fprintf (f, ")");
    }
  else
    fprintf (f, "#<invalid-or-unknown>");
}
