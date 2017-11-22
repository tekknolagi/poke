/* Readline: either a GNU readline wrapper or a trivial emulator -- header.

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


#include "jitter-readline.h"

/* Import a macro saying whether we can use the actual GNU readline. */
#include "jitter-config.h"




/* Readline: GNU Readline wrapper.
 * ************************************************************************** */

#ifdef JITTER_HAS_GNU_READLINE
/* Include GNU Readline headers. */
# include <readline/readline.h>
# include <readline/history.h>

char *
jitter_readline (char *prompt_or_NULL)
{
  /* Get a line from the terminal. */
  char *res = readline (prompt_or_NULL);

  /* Add it to the history unless empty or NULL. */
  if (res != NULL && * res != '\0')
    add_history (res);

  /* Return the line, either malloc-allocated or NULL. */
  return res;
}
#endif // #ifdef JITTER_HAS_GNU_READLINE




/* Readline: trivial reimplementation with no line-editing.
 * ************************************************************************** */

#ifndef JITTER_HAS_GNU_READLINE
/* Include what we need for the crude Readline emulator. */
# include <stdbool.h>
# include <stdio.h>
# include "jitter-dynamic-buffer.h"

char *
jitter_readline (char *prompt_or_NULL)
{
  /* Show the prompt, if any. */
  if (prompt_or_NULL != NULL)
    printf ("%s", prompt_or_NULL);

  /* Use a dynamic buffer to hold the string. */
  struct jitter_dynamic_buffer db;
  jitter_dynamic_buffer_initialize (& db);

  /* Keep adding to the string until we find '\n' or EOF.  Notice that an input
     containing a single '\n' character must yield a malloc-allocated empty
     string, and not NULL. */
  bool malloced = false;
  int c;
  while ((c = getchar ()) != EOF)
    {
      malloced = true;
      char c_as_char = c;

      /* Ignore any '\r' character. */
      if (c == '\r')
        continue;

      /* End the string if we find a '\n' character without adding the character
         to the string; consider any other character as "ordinary", belonging to
         the string. */
      if (c == '\n')
        break;
      else
        jitter_dynamic_buffer_push (& db, & c_as_char, 1);
    }

  /* If we found EOF return NULL. */
  if (! malloced)
    {
      /* This is only for defensiveness. */
      jitter_dynamic_buffer_finalize (& db);

      return NULL;
    }

  /* We have an actual string to return.  Terminate it. */
  char terminator = '\0';
  jitter_dynamic_buffer_push (& db, & terminator, 1);

  /* Return the dynamic buffer content, trimmed to only use the required
     space.  When extracting we don't need to finalize. */
  return jitter_dynamic_buffer_extract_trimmed (& db);
}
#endif // #ifndef JITTER_HAS_GNU_READLINE
