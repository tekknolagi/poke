/* Jittery Lisp: driver.

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


/* Include the Gnulib header. */
#include <config.h>

#include <argp.h>
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <jitter/jitter.h>
#include <jitter/jitter-cpp.h>
#include <jitter/jitter-dynamic-buffer.h>
#include <jitter/jitter-fatal.h>

#include "jitterlisp.h"


/* Command line handling using argp.
 * ************************************************************************** */

/* An enumerate to represent options with no short version.  These
   are options negating some other option: any negative option serves
   to reset a setting to its default value. */
enum jitterlisp_negative_option
  {
    jitterlisp_negative_option_no_verbose = -1,
    jitterlisp_negative_option_library = -2,
    jitterlisp_negative_option_omit_nothing = -3,
    jitterlisp_negative_option_vm = -4,
    jitterlisp_negative_option_repl = -5,
    jitterlisp_negative_option_no_colorize = -6
  };

/* Numeric keys for options having only a long format.  These must not conflict
   with any value in enum jitterlisp_negative_option . */
enum jitterlisp_long_only_option
  {
    jitterlisp_long_only_option_no_library = -7,
    jitterlisp_long_only_option_no_omit_nothing = -8,
    jitterlisp_long_only_option_no_vm = -9,
    jitterlisp_long_only_option_no_repl = -10,
    jitterlisp_long_only_option_dump_version = -11
  };

/* Command-line option specification. */
static struct argp_option jitterlisp_option_specification[] =
  {/* File options. */
   {NULL, '\0', NULL, OPTION_DOC, "File options:", 10},
   {"no-repl", jitterlisp_long_only_option_no_repl, NULL, 0,
    "Run non-interactively, without a REPL" },
   {"batch", 'q', NULL, OPTION_ALIAS },
   /* File negative options. */
   {NULL, '\0', NULL, OPTION_DOC, "", 11},
   {"repl", jitterlisp_negative_option_repl, NULL, 0,
    "Run interactively, with a REPL (default)"},
   {"no-batch", '\0', NULL, OPTION_ALIAS },

   /* Interaction options. */
   {NULL, '\0', NULL, OPTION_DOC, "Interaction options:", 20},
   {"no-omit-nothing", jitterlisp_long_only_option_no_omit_nothing, NULL, 0,
    "Show #<nothing> evaluation results" },
   /* Interaction negative options. */
   {NULL, '\0', NULL, OPTION_DOC, "", 21},
   {"omit-nothing", jitterlisp_negative_option_omit_nothing, NULL, 0,
    "Omit #<nothing> evaluation results (default)"},

   /* Command-line s-expression evaluation. */
   {NULL, '\0', NULL, OPTION_DOC, "Command-line s-expression evaluation:", 30},
   {"eval", 'e', "SEXPRS", 0,
    "Evaluate the given s-expressions after running the files (if any) "
    "and before running the REPL (unless the REPL is disabled)" },

   /* Debugging options. */
   {NULL, '\0', NULL, OPTION_DOC, "Debugging options:", 40},
   {"colorize", 'c', NULL, 0,
    "Colorize s-expressions with ANSI terminal escape sequences" },
   {"verbose", 'v', NULL, 0,
    "Show progress information at run time" },
   {"no-vm", jitterlisp_long_only_option_no_vm, NULL, 0,
    "Use a naïf C interpreter instead of the Jittery VM" },
   {"no-jittery", '\0', NULL, OPTION_ALIAS },
   {"no-library", jitterlisp_long_only_option_no_library, NULL, 0,
    "Don't load the Lisp library" },
   /* Debugging negative options. */
   {NULL, '\0', NULL, OPTION_DOC, "", 41},
   {"no-colorize", jitterlisp_negative_option_no_colorize, NULL, 0,
    "Don't colorize s-expressions (default)"},
   {"no-verbose", jitterlisp_negative_option_no_verbose, NULL, 0,
    "Don't show progress information (default)"},
   {"vm", jitterlisp_negative_option_vm, NULL, 0,
    "Use the Jittery VM (default)"},
   {"jittery", '\0', NULL, OPTION_ALIAS },
   {"library", jitterlisp_negative_option_library, NULL, 0,
    "Load the Lisp library (default)" },

   {NULL, '\0', NULL, OPTION_DOC, "Scripting options:", 50},
   {"dump-version", jitterlisp_long_only_option_dump_version, NULL, 0,
    "Print the JitterLisp version only, without any surrounding text; this "
    "is convenient for scripts" },

   /* Common GNU-style options. */
   {NULL, '\0', NULL, OPTION_DOC, "Common GNU-style options:", -1},
   /* These are automatically generated. */

   /* Option terminator. */
   { 0 }};

const char *argp_program_version
  = "JitterLisp (" PACKAGE_NAME ") " PACKAGE_VERSION "\n"
    "Copyright (C) 2017 Luca Saiu.\n"
    "JitterLisp comes with ABSOLUTELY NO WARRANTY.\n"
    "You may redistribute copies of JitterLisp under the terms of the GNU General Public\n"
    "License, version 3 or any later version published by the Free Software Foundation.\n"
    "For more information see the file named COPYING in the source distribution.\n\n"
    "Written by Luca Saiu <http://ageinghacker.net>.";
const char *argp_program_bug_address = PACKAGE_BUGREPORT;

/* Forward-declaration.  I like having argp defined here, before parse_opt which
   is quite long. */
static error_t
parse_opt (int key, char *arg, struct argp_state *state);

/* The parser main data structure. */
static struct argp argp =
  {
    jitterlisp_option_specification,
    parse_opt,
    "[FILE.lisp|-]...",
    "Run a JitterLisp program and/or a JitterLisp interactive REPL."
  };

/* Update our option state with the information from a single command-line
   option. */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  /* Because of how parse_opt is called sp points to jitterlisp_settings . */
  struct jitterlisp_settings *sp = state->input;
  switch (key)
    {
    /* File options. */
    case jitterlisp_long_only_option_no_repl:
    case 'q':
      sp->repl = false;
      break;

    /* File negative options. */
    case jitterlisp_negative_option_repl:
      sp->repl = true;
      break;

    /* Interaction options. */
    case jitterlisp_long_only_option_no_omit_nothing:
      sp->print_nothing_results = true;
      break;

    /* Interaction negative options. */
    case jitterlisp_negative_option_omit_nothing:
      sp->print_nothing_results = false;
      break;

    /* Command-line s-expression evaluation. */
    case 'e':
      sp->sexps_string = arg;
      break;

    /* Debugging options. */
    case 'c':
      sp->colorize = true;
      break;
    case 'v':
      sp->verbose = true;
      break;
    case jitterlisp_long_only_option_no_vm:
      sp->vm = false;
      break;
    case jitterlisp_long_only_option_no_library:
      sp->library = false;
      break;

    /* Debugging negative options. */
    case jitterlisp_negative_option_no_colorize:
      sp->colorize = false;
      break;
    case jitterlisp_negative_option_no_verbose:
      sp->verbose = false;
      break;
    case jitterlisp_negative_option_vm:
      sp->vm = true;
      break;
    case jitterlisp_negative_option_library:
      sp->library = true;
      break;

    /* Scripting options. */
    case jitterlisp_long_only_option_dump_version:
      printf ("%s\n", JITTER_PACKAGE_VERSION);
      exit (EXIT_SUCCESS);

    /* Non-option arguments. */
    case ARGP_KEY_ARG:
      jitter_dynamic_buffer_push (& sp->input_file_path_names, & arg,
                                  sizeof (char *));
      break;

    /* Handle anything else. */
    default:
      return ARGP_ERR_UNKNOWN;
    }
  return 0;
}




/* Main function.
 * ************************************************************************** */

int
main (int argc, char **argv)
{
  /* Initialize JitterLisp.  This among the rest initializes the settings data
     structure with default values, so it must be called before argp_parse ,
     which may change those. */
  jitterlisp_initialize ();

  /* Parse our arguments; jitterlisp_settings will contain the information
     provided in the command line.  We also define the "settings pointer"
     sp as a pointer to it, for convenience. */
  argp_parse (& argp, argc, argv, 0, 0, & jitterlisp_settings);
  struct jitterlisp_settings * const sp = & jitterlisp_settings;

  /* Run input files and s-expressions from the command-line, halting at the
     first error; still free the resources before exiting, even in case of
     error.
     Rationale: this level of attention to memory leaks would not be justified
     here at the top level where failure is effectively fatal and the process is
     about to terminate anyway, automaically freeing resources; however this is
     a good place to test the non-local error handling mechanism, and an example
     for users. */
  int return_code = EXIT_SUCCESS;
  JITTERLISP_HANDLE_ERRORS(
    {
      /* Run the library, unless disabled. */
      if (sp->library)
        jitterlisp_run_library ();

      /* Run the input files. */
      jitterlisp_run_from_input_files ();

      /* Evaluate s-expressions from the command line, if any. */
      if (sp->sexps_string != NULL)
        jitterlisp_run_from_string (sp->sexps_string);
    },
    {
      /* On failure: */
      printf ("Failed when evaluating input files or command-line "
              "s-expressions.\n");
      return_code = EXIT_FAILURE;
    });

  /* Run the REPL if enabled, and only if we didn't fail before.  Failure works
     differently in the REPL: every single command can fail, but such failures
     are handled internally and don't propagate: we don't want to kill the
     process every time the user makes a mistake in interactive use. */
  if (sp->repl && return_code != EXIT_FAILURE)
    jitterlisp_repl ();

  /* Finalize JitterLisp, freeing up resources.  This of course wouldn't be
     needed right before exiting, but is convenient when checking for memory
     leaks with Valgrind which this way won't show false positives. */
  jitterlisp_finalize ();

  /* Return success or failure, as we decided before.  Yes, we go to the trouble
     of freeing resources even on fatal errors: see the comment above. */
  return return_code;
}
