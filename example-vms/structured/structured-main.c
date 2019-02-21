/* Jittery structured language example: main.

   Copyright (C) 2017, 2019 Luca Saiu
   Written by Luca Saiu

   This file is part of the Jitter structured-language example, distributed
   along with Jitter under the same license.

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
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdbool.h>

#include <jitter/jitter-fatal.h>

#include "structuredvm-vm.h"

#include "structured-syntax.h"
#include "structured-parser.h"
#include "structured-code-generator.h"
#include "structured-code-generator-stack.h"
#include "structured-code-generator-register.h"


/* Why this source file parses argc and argv directly.
 * ************************************************************************** */

/* This example does not use argp, differently from the rest of Jitter, in order
   to keep portability without requiring Gnulib.  The intent here is to simplify
   the example build system for users looking at how to adopt Jitter for their
   own code, even at a small cost in complexity within this source file.

   The command-line interface of the structued program is simple, but civilized
   enough to respect the GNU conventions. */




/* Global variables.
 * ************************************************************************** */

/* The program name as it was invoked from the shell, or in other words a copy
   of the pointer in argv [0] , globally visible. */
static char *structured_program_name;

/* See the comment about help_section_indentation in jitter-config.in . */
#define STRUCTURED_HELP_SECTION_INDENTATION  ""




/* Utility functions for the command line.
 * ************************************************************************** */

/* Print a fatal error message and exit with failure, in response to an
   incorrect command line. */
static void
structured_usage (char *error_message)
{
  fprintf (stderr, "%s: %s.\n", structured_program_name, error_message);
  fprintf (stderr, "Try '%s --help' for more information.\n",
           structured_program_name);

  exit (EXIT_FAILURE);
}

/* Print a section heading in --help , with the given heading title. */
static void
structured_help_section (const char *title)
{
  printf ("\n" STRUCTURED_HELP_SECTION_INDENTATION "%s:\n", title);
}

/* Print command-line interface help and exit with success. */
static void
structured_help (void)
{
  printf ("Usage: %s [OPTION...] FILE.structured\n", structured_program_name);
  printf ("   or: %s [OPTION...] -\n", structured_program_name);
  printf ("Run a structured-language program on a Jittery VM, using the\n");
  printf (JITTER_DISPATCH_NAME_STRING " dispatching model.\n");

  structured_help_section ("Debugging options");
  printf ("      --disassemble                print hardware machine instructions\n");
  printf ("      --cross-disassemble          use the cross-disassembler rather than\n");
  printf ("                                   the native disassembler; also enable\n");
  printf ("                                   disassembly as per --disassemble\n");
  printf ("      --dry-run                    do not actually run the program\n");
  printf ("      --print                      print VM instructions\n");
  printf ("      --no-dry-run                 run the program (default)\n");

  structured_help_section ("Benchmarking options");
  printf ("      --no-optimization-rewriting  disable optimization rewriting\n");
  printf ("      --slow-only                  disable fast literals and fast registers\n");
  printf ("      --no-slow-only               enable fast literals and fast registers\n");
  printf ("                                   (default)\n");
  printf ("      --optimization-rewriting     enable optimization rewriting (default)\n");

  structured_help_section ("Code generation options");
  printf ("      --stack                      generate stack-based instructions (default)\n");
  printf ("      --register                   generate register-based instructions\n");

  structured_help_section ("Common GNU-style options");
  printf ("      --help                       give this help list and exit\n");
  printf ("      --version                    print program version and exit\n");

  printf ("\n");
  printf ("An \"--\" argument terminates option processing.\n");

  printf ("\n");
  printf ("Report bugs to " JITTER_PACKAGE_BUGREPORT ".\n");

  exit (EXIT_SUCCESS);
}

/* Print version information and exit with success. */
static void
structured_version (void)
{
  printf ("structured (" JITTER_PACKAGE_NAME ") " JITTER_PACKAGE_VERSION "\n");
  printf ("Copyright (C) 2017 Luca Saiu.\n");
  printf ("Jitter comes with ABSOLUTELY NO WARRANTY.\n");
  printf ("You may redistribute copies of Jitter under the terms of the GNU\n"
          "General Public License, version 3 or any later version published\n"
          "by the Free Software Foundation.  For more information see the file\n"
          "named COPYING in the source distribution.\n");
  printf ("\n");
  printf ("Written by Luca Saiu  <http://ageinghacker.net>.\n");

  exit (EXIT_SUCCESS);
}




/* Command-line handling.
 * ************************************************************************** */

/* A specifier for the code generator which is being used. */
enum structured_code_generator
  {
    /* Generate stack-based instructions */
    structured_code_generator_stack,

    /* Generate register-based instructions */
    structured_code_generator_register
  };

/* The state encoded in a user command line. */
struct structured_command_line
{
  /* True iff we should print back the VM program. */
  bool print;

  /* True iff we should use the cross-disassembler rather than the native
     disassembler for the VM program.  If false, use the native
     disassembler.  */
  bool cross_disassemble;

  /* True iff we should disassemble the VM program. */
  bool disassemble;

  /* True iff we should not actually run the VM program. */
  bool dry_run;

  /* True iff we should disable fast literals and fast registers, for
     benchmarking a worst-case scenario or for comparing with some other
     implementation. */
  bool slow_only;

  /* True iff we should enable optimization rewriting. */
  bool optimization_rewriting;

  /* Which code generator is being used. */
  enum structured_code_generator code_generator;

  /* Pathname of the program source to be loaded. */
  char *program_path;
};

/* Inizialize the command-line state to sensible defaults; make the program path
   intentionally invalid to catch errors. */
static void
structured_initialize_command_line (struct structured_command_line *cl)
{
  cl->print = false;
  cl->cross_disassemble = false;
  cl->disassemble = false;
  cl->dry_run = false;
  cl->optimization_rewriting = true;
  cl->slow_only = false;
  cl->code_generator = structured_code_generator_stack;
  cl->program_path = NULL;
}

/* Set the program name in the pointed command line structure to the given
   value, or fail fatally if the name was already set. */
static void
structured_set_command_line_program (struct structured_command_line *cl,
                                     char *arg)
{
  if (cl->program_path != NULL)
    structured_usage ("program name provided more than once");
  cl->program_path = arg;
}

/* Fill the pointed command-line data structure with information from the
   actual command line. */
static void
structured_parse_command_line (struct structured_command_line *cl,
                               int argc, char **argv)
{
  structured_program_name = argv [0];
  structured_initialize_command_line (cl);

  int i;
  bool handle_options = true;
  for (i = 1; i < argc; i ++)
    {
      /* Get the current argument, be it option or non-option. */
      char *arg = argv [i];

      /* If we are still handling options but the current argument is "--" then
         stop doing that, and don't handle the current argument any further. */
      if (handle_options && ! strcmp (arg, "--"))
        {
          handle_options = false;
          continue;
        }

      /* Handle arg, as an option or a pathname. */
      if      (handle_options && ! strcmp (arg, "--help"))
        structured_help ();
      else if (handle_options && ! strcmp (arg, "--version"))
        structured_version ();
      else if (handle_options && ! strcmp (arg, "--disassemble"))
        cl->disassemble = true;
      else if (handle_options && ! strcmp (arg, "--cross-disassemble"))
        {
          cl->cross_disassemble = true;
          cl->disassemble = true;
        }
      else if (handle_options && ! strcmp (arg, "--slow-only"))
        cl->slow_only = true;
      else if (handle_options && ! strcmp (arg, "--no-slow-only"))
        cl->slow_only = false;
      else if (handle_options && ! strcmp (arg, "--optimization-rewriting"))
        cl->optimization_rewriting = true;
      else if (handle_options && ! strcmp (arg, "--no-optimization-rewriting"))
        cl->optimization_rewriting = false;
      else if (handle_options && ! strcmp (arg, "--stack"))
        cl->code_generator = structured_code_generator_stack;
      else if (handle_options && ! strcmp (arg, "--register"))
        cl->code_generator = structured_code_generator_register;
      else if (handle_options && ! strcmp (arg, "--print"))
        cl->print = true;
      else if (handle_options && ! strcmp (arg, "--dry-run"))
        cl->dry_run = true;
      else if (handle_options && ! strcmp (arg, "--no-dry-run"))
        cl->dry_run = false;
      else if (handle_options && strlen (arg) > 1 && arg [0] == '-')
        structured_usage ("unrecognized option");
      else if (handle_options && strlen (arg) > 1 && arg [0] != '-')
        structured_set_command_line_program (cl, arg);
      else
        structured_set_command_line_program (cl, arg);
    }

  /* Still not having a program name at the end is an error. */
  if (cl->program_path == NULL)
    structured_usage ("program name missing");
}




/* Execute what the command line says.
 * ************************************************************************** */

/* Do what the pointed command line data structure says. */
static void
structured_work (struct structured_command_line *cl)
{
  /* Parse a structured-language program into an AST. */
  struct structured_program *p;
  if (! strcmp (cl->program_path, "-"))
    p = structured_parse_file_star (stdin);
  else
    p = structured_parse_file (cl->program_path);

  /* Initialize the structured-VM subsystem. */
  structuredvm_initialize ();

  /* Make an empty Jittery program and set options for it as needed. */
  struct structuredvm_program *vmp = structuredvm_make_program ();
  if (cl->slow_only)
    {
      structuredvm_set_program_option_slow_literals_only (vmp, true);
      structuredvm_set_program_option_slow_registers_only (vmp, true);
    }
  if (! cl->optimization_rewriting) // FIXME: make this into a VM program option.
    structuredvm_disable_optimization_rewriting ();

  /* Translate the AST program into a jittery program. */
  switch (cl->code_generator)
    {
    case structured_code_generator_stack:
      structured_translate_program_stack (vmp, p);
      break;
    case structured_code_generator_register:
      structured_translate_program_register (vmp, p);
      break;
    default:
      jitter_fatal ("unknwon code generator (bug): %i", (int) cl->code_generator);
    }

  /* Specialize the jittery program, so that we can run it. */
  structuredvm_specialize_program (vmp);

  /* Print and/or disassemble the program as requested. */
  if (cl->print)
    structuredvm_print_program (stdout, vmp);
  if (cl->cross_disassemble)
    cl->disassemble = true;
  if (cl->disassemble)
    structuredvm_disassemble_program (vmp, true,
                                      (cl->cross_disassemble
                                       ? JITTER_CROSS_OBJDUMP
                                       : JITTER_OBJDUMP),
                                      NULL);

  /* Run the Jittery program in a temporary state, unless this is a dry run. */
  if (! cl->dry_run)
    {
      struct structuredvm_state s;
      structuredvm_state_initialize (& s);
      structuredvm_interpret (vmp, &s);
      structuredvm_state_finalize (& s);
    }

  /* Destroy the Jittery program. */
  structuredvm_destroy_program (vmp);

  /* Finalize the structured-VM subsystem. */
  structuredvm_finalize ();
}




/* Main function.
 * ************************************************************************** */

int
main (int argc, char **argv)
{
  /* Parse the command-line arguments, including options. */
  struct structured_command_line cl;
  structured_parse_command_line (& cl, argc, argv);

  /* Do what was requested on the command line. */
  structured_work (& cl);

  /* Exit with success, if we're still alive. */
  return EXIT_SUCCESS;
}
