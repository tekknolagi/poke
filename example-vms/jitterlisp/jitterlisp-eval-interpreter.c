/* Jittery Lisp: interpreter: naïve C version.

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


#include "jitterlisp-eval-interpreter.h"

#include <jitter/jitter-string.h> // for jitter_clone_string: possibly to remove.

#include "jitterlisp.h"


/* Non-Jittery interpreter.
 * ************************************************************************** */

jitterlisp_object
jitterlisp_eval_globally_interpreter (jitterlisp_object form)
{
  /* FIXME: before having a real eval I can still check for memory leaks using
     Valgrind; the critical case is freeing resources on error.  Here by
     convention the empty list causes a failure, symbols evaluate to 42,
     booleans to a symbol and everything else to #<nothing>. */
  if (form == JITTERLISP_EMPTY_LIST)
    jitterlisp_error (jitter_clone_string ("the empty list is evil"));
  else if (JITTERLISP_IS_SYMBOL(form))
    return JITTERLISP_FIXNUM_ENCODE(42);
  else if (JITTERLISP_IS_BOOLEAN(form))
    return JITTERLISP_SYMBOL_ENCODE(
              jitterlisp_symbol_make_interned("something"));
  else
    return JITTERLISP_NOTHING;
}
