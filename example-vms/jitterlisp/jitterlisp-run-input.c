/* Jittery Lisp: running from files, C strings, REPL.

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


#include "jitterlisp-run-input.h"

#include <stdio.h>
#include <string.h>

#include "jitterlisp.h"

#include <jitter/jitter-dynamic-buffer.h>
#include <jitter/jitter-fatal.h>
#include <jitter/jitter-string.h> // jitter_clone_string: probably to remove




/* Run multiple s-expressions from a reader state.
 * ************************************************************************** */

/* Keep reading and evaluating every s-expression from the given reader state
   until the reader returns #<eof>, then destroy the reader state.  In case
   of errors free the resources correctly and propagate. */
static void
jitterlisp_run_and_destroy_reader_state (struct jitterlisp_reader_state *rstate)
{
  bool success = true;
  JITTERLISP_HANDLE_ERRORS(
    {
      /* Read and execute until #<eof> or until the first error. */
      jitterlisp_object form;
      while (! JITTERLISP_IS_EOF (form = jitterlisp_read (rstate)))
        jitterlisp_eval_globally (form);
    },
    {
      /* If we arrived here there was an error. */
      success = false;
    });

  /* Free the resources in either case, success or error. */
  jitterlisp_destroy_reader_state (rstate);

  /* If we failed propagate the error outside. */
  if (! success)
    jitterlisp_reerror ();
}




/* Run s-expressions parsed from a C string.
 * ************************************************************************** */

void
jitterlisp_run_from_string (const char *string)
{
  printf ("Running from string \"%s\"...\n", string);
  struct jitterlisp_reader_state *rstate
    = jitterlisp_make_string_reader_state (string);
  jitterlisp_run_and_destroy_reader_state (rstate);
  printf ("...Done running from string \"%s\".\n", string);
}




/* Run s-expressions parsed from input streams.
 * ************************************************************************** */

void
jitterlisp_run_from_stream (FILE *input)
{
  printf ("Running from stream %p...\n", input);
  struct jitterlisp_reader_state *rstate
    = jitterlisp_make_stream_reader_state (input);
  jitterlisp_run_and_destroy_reader_state (rstate);
  printf ("...Done running from stream %p.\n", input);
}




/* Run s-expressions parsed from input files.
 * ************************************************************************** */

void
jitterlisp_run_from_named_file (const char *path_name)
{
  printf ("Running from file \"%s\"...\n", path_name);

  /* Open an input stream. */
  FILE *in;
  if (! strcmp (path_name, "-"))
    in = stdin;
  else
    in = fopen (path_name, "r");
  if (in == NULL)
    jitter_fatal ("could not read %s", path_name);

  /* Read forms from the input stream and run each of them. */
  jitterlisp_run_from_stream (in);

  /* Close the input stream. */
  fclose (in);
  printf ("...Done running from file \"%s\".\n", path_name);
}

void
jitterlisp_run_from_input_files (void)
{
  printf ("Running from input files...\n");
  size_t input_file_path_name_no
    = (jitterlisp_settings.input_file_path_names.used_size
       / sizeof (char *));
  const char **input_file_path_names =
    jitter_dynamic_buffer_to_pointer
       (& jitterlisp_settings.input_file_path_names);
  int i;
  for (i = 0; i < input_file_path_name_no; i ++)
    jitterlisp_run_from_named_file (input_file_path_names [i]);
  printf ("...Done running from input files.\n");
}




/* Run s-expressions interactively from a REPL.
 * ************************************************************************** */

void
jitterlisp_repl (void)
{
  printf ("Running the REPL...\n");

  /* Make a readline reader state.  It will be used to read every form
     from the REPL, even in case of parse errors. */
  struct jitterlisp_reader_state *rstate
    = jitterlisp_make_readline_reader_state ("jitterlisp> ");

  /* Loop: read a form, exit if it's #<eof>, otherwise evaluate it.  In case of
     any error, at either read or evaluation time, just continue with the next
     iteration. */
  do
    {
      JITTERLISP_HANDLE_ERRORS(
        {
          jitterlisp_object form = jitterlisp_read (rstate);
          if (JITTERLISP_IS_EOF (form))
            goto out;
          else
            {
              jitterlisp_object result = jitterlisp_eval_globally (form);
              printf ("  ");
              jitterlisp_print_to_stream (stdout, form);
              if (! JITTERLISP_IS_NOTHING (result))
                {
                  printf (" ==> ");
                  jitterlisp_print_to_stream (stdout, result);
                }
              printf ("\n");
            }
        },
        {
          /* Do nothing on error. */
          printf ("User error in the REPL.  Continuing.\n");
        });
    }
  while (true);

  /* We got out of the loop.  Only now we can destroy the reader. */
 out:
  jitterlisp_destroy_reader_state (rstate);

  printf ("...Done running the REPL.\n");
}
