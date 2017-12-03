/* Jittery Lisp: main file.

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


#include "jitterlisp.h"

/* Include the GNU Readline header for initialization.  This may not be the most
   appropriate place for this, but it's just one line. */
#ifdef JITTER_HAS_GNU_READLINE
# include <readline/readline.h>
#endif // #ifdef JITTER_HAS_GNU_READLINE


void
jitterlisp_initialize (void)
{
  fflush (stdout); fflush (stderr); fprintf (stderr, "Initializing...\n"); fflush (stdout); fflush (stderr);
  /* Provide default values for the global settings. */
  jitterlisp_settings_set_default ();

  /* Initialize every subsystem. */
  jitterlisp_memory_initialize ();
  jitterlisp_sexpression_initialize ();
  jitterlisp_error_initialize ();

#ifdef JITTER_HAS_GNU_READLINE
  /* Enable blink-matching-paren if using GNU Readline.  Some people might find
     this default somewhat Lisp-specific, which is why it's not in
     jitter-readline.c . */
  rl_variable_bind ("blink-matching-paren", "on");
#endif // #ifdef JITTER_HAS_GNU_READLINE
  fflush (stdout); fflush (stderr); fprintf (stderr, "...Initialized.\n"); fflush (stdout); fflush (stderr);
}

void
jitterlisp_finalize (void)
{
  fflush (stdout); fflush (stderr); fprintf (stderr, "Finalizing...\n"); fflush (stdout); fflush (stderr);
  /* Finalize every subsystem, in the opposite order of initialization. */
  jitterlisp_error_finalize ();
  jitterlisp_sexpression_finalize ();
  jitterlisp_memory_finalize ();
  jitterlisp_settings_finalize (); /* There's a dynamic buffer to finalize. */
  fflush (stdout); fflush (stderr); fprintf (stderr, "Finalized...\n"); fflush (stdout); fflush (stderr);
}
