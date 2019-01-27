/* JitterLisp: running from files, C strings, REPL.

   Copyright (C) 2017, 2018, 2019 Luca Saiu
   Written by Luca Saiu

   This file is part of the JitterLisp language implementation, distributed as
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
#include <time.h>
#include <sys/time.h>

#include "jitterlisp.h"

#include <jitter/jitter-dynamic-buffer.h>
#include <jitter/jitter-fatal.h>
#include <jitter/jitter-string.h> // jitter_clone_string: probably to remove




/* Run multiple s-expressions from a reader state.
 * ************************************************************************** */

/* Keep reading and evaluating every s-expression from the given reader state
   until the reader returns #<eof>, then destroy the reader state.  In case of
   errors free the resources correctly and propagate; otherwise return the
   result of the last form. */
static jitterlisp_object
jitterlisp_run_and_destroy_reader_state (struct jitterlisp_reader_state *rstate)
{
  /* The initialization of res is necessary for the case where the input
     contains no forms and therefore jitterlisp_eval_globally is never
     called. */
  jitterlisp_object res = JITTERLISP_NOTHING;
  bool success = true;
  JITTERLISP_HANDLE_ERRORS(
    {
      /* Read and execute until #<eof> or until the first error. */
      jitterlisp_object form;
      while (! JITTERLISP_IS_EOF (form = jitterlisp_read (rstate)))
        res = jitterlisp_eval_globally (form);
    },
    {
      /* If we arrived here there was an error. */
      success = false;
    });

  /* Free the resources in either case, success or error. */
  jitterlisp_destroy_reader_state (rstate);

  /* If we failed propagate the error outside, otherwise return the result of
     the last form. */
  if (! success)
    jitterlisp_reerror ();
  else
    return res;
}




/* Run s-expressions parsed from a C string.
 * ************************************************************************** */

/* A function factoring jitterlisp_run_from_string and jitterlisp_run_library ,
   with an additional boolean argument making the execution non-verbose as
   appropriate for the library. */
static void
jitterlisp_run_from_string_internal (const char *string, bool library_verbosity)
{
  if (! library_verbosity && jitterlisp_settings.verbose)
    printf ("Running from string \"%s\"...\n", string);
  struct jitterlisp_reader_state *rstate
    = jitterlisp_make_string_reader_state (string);
  jitterlisp_object result
    = jitterlisp_run_and_destroy_reader_state (rstate);
  if (! library_verbosity && (jitterlisp_settings.print_nothing_results
                              || ! JITTERLISP_IS_NOTHING(result)))
    {
      jitterlisp_print_to_stream (stdout, result);
      printf ("\n");
    }
  if (! library_verbosity && jitterlisp_settings.verbose)
    printf ("...Done running from string \"%s\".\n", string);
}

void
jitterlisp_run_from_string (const char *string)
{
  jitterlisp_run_from_string_internal (string, false);
}




/* Run the Lisp library.
 * ************************************************************************** */

void
jitterlisp_run_library (void)
{
  if (jitterlisp_settings.verbose)
    printf ("Running the library...\n");
  jitterlisp_run_from_string_internal (jitterlisp_library_string, true);
  if (jitterlisp_settings.verbose)
    printf ("...Done running the library.\n");
}




/* Run s-expressions parsed from input streams.
 * ************************************************************************** */

void
jitterlisp_run_from_stream (FILE *input)
{
  struct jitterlisp_reader_state *rstate
    = jitterlisp_make_stream_reader_state (input);
  jitterlisp_run_and_destroy_reader_state (rstate);
}




/* Run s-expressions parsed from input files.
 * ************************************************************************** */

void
jitterlisp_run_from_named_file (const char *path_name)
{
  if (jitterlisp_settings.verbose)
    printf ("Running from file \"%s\"...\n", path_name);

  /* Open an input stream. */
  FILE *in;
  if (! strcmp (path_name, "-"))
    in = stdin;
  else
    in = fopen (path_name, "r");
  if (in == NULL)
    jitterlisp_error_cloned ("could not open input file");

  /* Read forms from the input stream and run each of them. */
  jitterlisp_run_from_stream (in);

  /* Close the input stream. */
  fclose (in);
  if (jitterlisp_settings.verbose)
    printf ("...Done running from file \"%s\".\n", path_name);
}

void
jitterlisp_run_from_input_files (void)
{
  size_t input_file_path_name_no
    = (jitterlisp_settings.input_file_path_names.used_size
       / sizeof (char *));
  const char **input_file_path_names =
    jitter_dynamic_buffer_to_pointer
       (& jitterlisp_settings.input_file_path_names);
  int i;
  for (i = 0; i < input_file_path_name_no; i ++)
    jitterlisp_run_from_named_file (input_file_path_names [i]);
}




/* Run s-expressions interactively from a REPL.
 * ************************************************************************** */

void
jitterlisp_repl (void)
{
  if (jitterlisp_settings.verbose)
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
          struct timeval time_before;
          gettimeofday (& time_before, NULL);
          jitterlisp_object result = jitterlisp_eval_globally (form);
          struct timeval time_after;
          gettimeofday (& time_after, NULL);
          double elapsed_time
            = ((time_after.tv_usec * 1e-6 + time_after.tv_sec)
               - (time_before.tv_usec * 1e-6 + time_before.tv_sec));
          /* Print the elapsed time according to the settings. */
          switch (jitterlisp_settings.time)
            {
            case jitterlisp_time_yes:
              printf ("[Evaluated in %.3fs]\n", elapsed_time);
              break;
            case jitterlisp_time_verbose:
              printf ("[");
              jitterlisp_print_to_stream (stdout, form);
              printf (" evaluated in %.3fs]\n", elapsed_time);
              break;
            case jitterlisp_time_no:
              /* Do nothing. */
              break;
            default:
              jitter_fatal ("invalid jitterlisp_settings.time");
            }
          /* Print the result, unless it's #<nothing> and printing #<nothing>
             results is disabled. */
          if (jitterlisp_settings.print_nothing_results
              || ! JITTERLISP_IS_NOTHING (result))
            {
              jitterlisp_print_to_stream (stdout, result);
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

  if (jitterlisp_settings.verbose)
    printf ("...Done running the REPL.\n");

  /* Print a goodbye message.  It is acceptable to do it unconditionally, since
     this is interactive use. */
  printf ("Goodbye.\n");
}
