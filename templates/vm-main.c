/* VM default frontend for vmprefix VM.

   Copyright (C) 2016, 2017 Luca Saiu
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


/* Generated file warning.
 * ************************************************************************** */

/* Unless this file is named exactly "vm-main.c" , without any prefix, you are
   looking at a machine-generated derived file.  The original source is the
   vm-main.c template from Jitter. */




/* Include headers.
 * ************************************************************************** */

#ifdef HAVE_CONFIG_H
/* Use Gnulib, if available; on GNU everything should work even without it.

   FIXME: I might want to find a solution for other systems, but they are not
   prioritary. */
# include <config.h>
#endif // #ifdef HAVE_CONFIG_H

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdbool.h>

#include <argp.h>


/* Include optional headers. */
#include <jitter/jitter-config.h>
#ifdef HAVE_SETRLIMIT
# include <sys/resource.h> /* For getrlimit and setrlimit . */
#endif // #ifdef HAVE_SETRLIMIT

#include <jitter/jitter-parse-int.h>
#include <jitter/jitter-fatal.h>

#include "vmprefix-vm.h"




/* Command line handling.
 * ************************************************************************** */

/* All the information encoded by the user in the command line. */
struct vmprefix_main_command_line
{
  bool debug;
  bool progress_on_stderr;
  bool print_program, disassemble_program, run_program, slow_registers_only;
  char *input_file;
  char *objdump_name;
  char *objdump_options; /* Use default options when NULL. */
  bool objdump_name_overridden;

#ifdef HAVE_ALARM
  /* The wall-clock run time limit in seconds, or 0 if there is no limit. */
  unsigned int wall_clock_run_time_limit;
#endif // #ifdef HAVE_ALARM

#ifdef HAVE_SETRLIMIT
  /* The CPU time limit in seconds, or RLIM_INFINITY if there is no limit. */
  rlim_t cpu_time_limit;
#endif // #ifdef HAVE_SETRLIMIT
};

/* Numeric identifiers for --no-* , or more in general "default" options having
   no short equivalent and specifying the default behavior; these are
   particularly convenient for interactive use where a complex command line is
   modified frequently.  Each case must have a value which is not an ASCII
   character. */
enum vmprefix_vm_negative_option
  {
    vmprefix_vm_negative_option_no_cross_disassemble = -1,
    vmprefix_vm_negative_option_no_disassemble = -2,
    vmprefix_vm_negative_option_no_debug = -3,
    vmprefix_vm_negative_option_no_dry_run = -4,
    vmprefix_vm_negative_option_no_print_program = -5,
    vmprefix_vm_negative_option_no_progress_on_stderr = -6,
    vmprefix_vm_negative_option_no_slow_registers_only = -7
  };

/* Numeric keys for options having only a long format.  These must not conflict
   with any value in enum vmprefix_vm_negative_option . */
enum vmprefix_vm_long_only_option
  {
    vmprefix_vm_long_only_option_dump_jitter_version = -8
  };

/* Update our option state with the information from a single command-line
   option. */
static error_t
parse_opt (int key, char *arg, struct argp_state *state)
{
  struct vmprefix_main_command_line *cl = state->input;
  switch (key)
    {
      /* FIXME: within the case I could use state->arg_num to get the current
         non-option argument index, if needed. */
    case ARGP_KEY_INIT:
      /* Set reasonable default values. */
      cl->debug = false;
      cl->progress_on_stderr = false;
      cl->print_program = false;
      cl->disassemble_program = false;
      cl->run_program = true;
      cl->slow_registers_only = false;
      cl->input_file = NULL;
      cl->objdump_name = JITTER_OBJDUMP;
      cl->objdump_name_overridden = false;
      cl->objdump_options = NULL;
#ifdef HAVE_ALARM
      cl->wall_clock_run_time_limit = 0;
#endif // #ifdef HAVE_ALARM
#ifdef HAVE_SETRLIMIT
      cl->cpu_time_limit = RLIM_INFINITY;
#endif // #ifdef HAVE_SETRLIMIT
      break;
    case 't':
      {
        long long limit;
        if (jitter_string_to_long_long_inconvenient (arg, & limit) != 0)
          argp_error (state, "--time-limit: invalid integer %s", arg);
#ifdef HAVE_ALARM
        if (limit < 0)
          argp_error (state, "--time-limit: negative time %lli", limit);
        else if (limit > UINT_MAX)
          argp_error (state, "--time-limit: integer %s out of range", arg);
        else if (limit > 0)
          cl->wall_clock_run_time_limit = limit;
#else
        fprintf (stderr, "warning: alarm is disabled\n");
#endif // #ifdef HAVE_ALARM
        }
      break;
    case 'c':
      {
        long long limit;
        if (jitter_string_to_long_long_inconvenient (arg, & limit) != 0)
          argp_error (state, "--cpu-time-limit: invalid integer %s", arg);
#ifdef HAVE_SETRLIMIT
        if (limit < 0)
          argp_error (state, "--cpu-time-limit: negative time %lli", limit);
        else if (limit > 0)
          cl->cpu_time_limit = limit;
#else
        fprintf (stderr, "warning: setrlimit is disabled\n");
#endif // #ifdef HAVE_SETRLIMIT
        }
      break;
    case 'd':
      cl->debug = true;
      break;
    case vmprefix_vm_long_only_option_dump_jitter_version:
      printf ("%s\n", JITTER_PACKAGE_VERSION);
      exit (EXIT_SUCCESS);
    case vmprefix_vm_negative_option_no_debug:
      cl->debug = false;
      break;
    case 'e':
      cl->progress_on_stderr = true;
      break;
    case vmprefix_vm_negative_option_no_progress_on_stderr:
      cl->progress_on_stderr = false;
      break;
    case 'p':
      cl->print_program = true;
      break;
    case vmprefix_vm_negative_option_no_print_program:
      cl->print_program = false;
      break;
    case 'b':
      cl->objdump_name = arg;
      cl->objdump_name_overridden = true;
      break;
    case 'B':
      cl->objdump_options = arg;
      break;
    case 'D':
      cl->disassemble_program = true;
      if (! cl->objdump_name_overridden)
        cl->objdump_name = (arg != NULL) ? arg : JITTER_OBJDUMP;
      break;
    case vmprefix_vm_negative_option_no_disassemble:
    case vmprefix_vm_negative_option_no_cross_disassemble:
      cl->disassemble_program = false;
      break;
    case 'C':
      cl->disassemble_program = true;
#if defined(JITTER_CROSS_COMPILING) && defined (JITTER_CROSS_OBJDUMP)
      if (! cl->objdump_name_overridden)
        cl->objdump_name = JITTER_CROSS_OBJDUMP;
#else
      if (! cl->objdump_name_overridden)
        cl->objdump_name = JITTER_OBJDUMP;
#endif // #if defined(JITTER_CROSS_COMPILING) && defined (JITTER_CROSS_OBJDUMP)
      break;
    case 'n':
      cl->run_program = false;
      break;
    case vmprefix_vm_negative_option_no_dry_run:
      cl->run_program = true;
      break;
    case 's':
      cl->slow_registers_only = true;
      break;
    case vmprefix_vm_negative_option_no_slow_registers_only:
      cl->slow_registers_only = false;
      break;
    case ARGP_KEY_ARG:
      cl->input_file = arg;
      break;

    /* case ARGP_KEY_NO_ARGS: /\* If this case is omitted, the default is sensible. *\/ */
    /*   argp_error (state, "you have to supply the input file\n"); */
    /*   break; */

    /* case ARGP_KEY_SUCCESS: /\* If this case is omitted, the default is sensible. *\/ */
    /*   break; */

    case ARGP_KEY_END:
      if (state->arg_num != 1)
        argp_error (state, "you gave %i input files instead of one.",
                    (int)state->arg_num);
      break;

    /* case ARGP_KEY_FINI: /\* If this case is omitted, the default is sensible. *\/ */
    /*   break; */

    default:
      return ARGP_ERR_UNKNOWN;
    }
  return 0;
}

/* Command-line option specification. */
static struct argp_option vmprefix_main_option_specification[] =
  {/* Most frequently used options. */
   {NULL, '\0', NULL, OPTION_DOC, "Frequently used options:", 10},
   {"print-program", 'p', NULL, 0,
    "Print back the parsed program"},
   {"dry-run", 'n', NULL, 0,
    "Do not actually run the program"},
   {"no-run", '\0', NULL, OPTION_ALIAS },
   /* Most frequently used negative options. */
   {NULL, '\0', NULL, OPTION_DOC, "", 11},
   {"no-dry-run", vmprefix_vm_negative_option_no_dry_run, NULL, 0,
    "Actually run the parsed program (default)"},
   {"no-print-program", vmprefix_vm_negative_option_no_print_program, NULL, 0,
    "Don't print back the parsed program (default)"},

   /* Disassembly options. */
   {NULL, '\0', NULL, OPTION_DOC, "Disassembly options:", 20},
   {"objdump", 'b', "OBJDUMP", 0,
    "If disassemblying use the given objdump program, overriding the default"},
   {"objdump-options", 'B', "OPTS", 0,
    "If disassemblying use the given objdump options, overriding the default"
    " (" JITTER_OBJDUMP_OPTIONS
    ").  Options for raw dumping and for setting the correct endianness are "
    " provided in any case, but user options take precedence"},
   {"disassemble", 'D', "OBJDUMP", OPTION_ARG_OPTIONAL,
    "Disassemble native " JITTER_HOST_TRIPLET " code using the given objdump"
    " program (default: " JITTER_OBJDUMP
    ") with, unless overridden, the default options"},
   {"cross-disassemble", 'C', NULL, 0,
#if defined(JITTER_CROSS_COMPILING) && defined (JITTER_CROSS_OBJDUMP)
    "Cross-disassemble " JITTER_HOST_TRIPLET " code (presumably through"
    " an emulator running on " JITTER_BUILD_TRIPLET
    " , on which this program was compiled) using "
    JITTER_CROSS_OBJDUMP " by default"
#else
    "Disassemble native " JITTER_HOST_TRIPLET " code, using " JITTER_OBJDUMP
    " by default with, unless overridden, the default options"
    " (this option exists for compatibility with cross-compiled builds)"
#endif // #if defined(JITTER_CROSS_COMPILING) && defined (JITTER_CROSS_OBJDUMP)
   },
   /* Disassembly negative options. */
   {NULL, '\0', NULL, OPTION_DOC, "", 21},
   {"no-disassemble", vmprefix_vm_negative_option_no_disassemble, NULL, 0,
    "Don't disassemble the parsed program (default)"},
   {"no-cross-disassemble", vmprefix_vm_negative_option_no_cross_disassemble,
    NULL, 0,
    "Don't cross-disassemble the parsed program (default)"},

   /* Debugging, testing and benchmarking options. */
   {NULL, '\0', NULL, OPTION_DOC,
    "Debugging, testing and benchmarking options:", 30},
   {"progress-on-stderr", 'e', NULL, 0,
    "Show progress information on stderr instead of stdout"},
   {"debug", 'd', NULL, 0, "Enable debugging" },
   {"slow-registers-only", 's', NULL, 0,
    "Use slow registers even when fast registers would be available"
    " (this is mostly useful to measure the speedup introduced by fast"
    " registers, or to benchmark a worst-case scenario)"},
   {"dump-jitter-version", vmprefix_vm_long_only_option_dump_jitter_version,
    NULL, 0,
    "Print the Jitter version only, without any surrounding text; this "
    "is convenient for scripts" },
   {"dump-version", '\0', NULL, OPTION_ALIAS },
   /* Debugging, testing and benchmarking negative options. */
   {NULL, '\0', NULL, OPTION_DOC, "", 31},
   {"no-progress-on-stderr", vmprefix_vm_negative_option_no_progress_on_stderr,
    NULL, 0, "Show progress information on stdout (default)"},
   {"no-debug", vmprefix_vm_negative_option_no_debug,
    NULL, 0, "Disable debugging (default)"},
   {"no-slow-registers-only", vmprefix_vm_negative_option_no_slow_registers_only,
    NULL, 0, "Use fast registers when possible (default)"},

   /* Test suite options. */
   {NULL, '\0', NULL, OPTION_DOC, "Options mostly useful for test suites:", 40},
   {"time-limit", 't', "S", 0,
    "Fail if wall-clock run time exceeds S seconds (0 for no limit)"
#ifndef JITTER_HOST_OS_IS_GNU
    "; this uses the alarm function, which may interfere with sleep on"
    " non-GNU systems where sleep is implemented with SIGALRM"
#endif // #ifndef JITTER_HOST_OS_IS_GNU
#ifndef HAVE_ALARM
    "  (Ignored on this configuration.)"
#endif // #ifndef HAVE_SETRLIMIT
   },
   {"cpu-time-limit", 'c', "S", 0,
    "Fail if CPU time exceeds S seconds (0 for no limit)"
#ifndef HAVE_SETRLIMIT
    "  (Ignored on this configuration.)"
#endif // #ifndef HAVE_SETRLIMIT
   },

   /* Common GNU-style options. */
   {NULL, '\0', NULL, OPTION_DOC, "Common GNU-style options:", -1},
   /* These are automatically generated. */

   /* Option terminator. */
   { 0 }};

/* Short help text to print on --help and --version .  Defaults are
   supplied by the library if these are not defined; I guess they
   use weak symbols on reasonable platforms. */
const char *argp_program_version
/*  = VMPREFIX_VM_NAME " VM driver ("
       JITTER_DISPATCH_NAME_STRING " dispatch, " JITTER_PACKAGE_NAME
       " " JITTER_PACKAGE_VERSION ")\n"*/
  = VMPREFIX_VM_NAME " VM driver (" JITTER_PACKAGE_NAME ") "
    JITTER_PACKAGE_VERSION "\n"
    "(Dispatching model: " JITTER_DISPATCH_NAME_STRING ")\n"
    "Copyright (C) 2016, 2017 Luca Saiu.\n"
    "Jitter comes with ABSOLUTELY NO WARRANTY.\n"
    "You may redistribute copies of Jitter under the terms of the\n"
    "GNU General Public License, version 3 or any later version published by\n"
    "the Free Software Foundation.  For more information see the file named\n"
    "COPYING in the source distribution or the git repository.\n\n"
    "Written by Luca Saiu <http://ageinghacker.net> (Jitter, its runtime, "
    "this driver program).";
const char *argp_program_bug_address = JITTER_PACKAGE_BUGREPORT;

/* The parser main data structure. */
static struct argp argp =
  {
    vmprefix_main_option_specification,
    parse_opt,
    "FILE.vm",
    "Run a program encoded as a text file on the " VMPREFIX_VM_NAME
    " VM, using " JITTER_DISPATCH_NAME_STRING " dispatch."
  };




/* Main function.
 * ************************************************************************** */

int
main (int argc, char **argv)
{
  /* Parse our arguments; every option seen by 'parse_opt' will
     be reflected in 'arguments'. */
  struct vmprefix_main_command_line cl;
  argp_parse (&argp, argc, argv,
              0,//ARGP_IN_ORDER,
              0, &cl);
  FILE *progress;
  if (cl.progress_on_stderr)
    progress = stderr;
  else
    progress = stdout;

#ifdef HAVE_ALARM
  /* Set a limit to the wall-clock run time, if so requested on the command
     line. */
  if (cl.wall_clock_run_time_limit != 0)
    alarm (cl.wall_clock_run_time_limit);
#endif // #ifdef HAVE_ALARM

#ifdef HAVE_SETRLIMIT
  /* Set a limit to the CPU time, if so requested on the command line. */
  if (cl.cpu_time_limit != RLIM_INFINITY)
    {
      if (cl.debug)
        fprintf (progress, "Setting resource limits...\n");
      struct rlimit limit;
      if (getrlimit (RLIMIT_CPU, & limit) != 0)
        jitter_fatal ("getrlimit failed");
      limit.rlim_cur = cl.cpu_time_limit;
      if (setrlimit (RLIMIT_CPU, & limit) != 0)
        jitter_fatal ("setrlimit failed");
    }
#endif // #ifdef HAVE_SETRLIMIT

  if (cl.debug)
    fprintf (progress, "Initializing...\n");
  vmprefix_initialize ();

  /* Print the VM configuration if in debugging mode. */
  if (cl.debug)
    vmprefix_print_vm_configuration (progress,
                                     vmprefix_vm_configuration);

  struct vmprefix_program *p;
  if (cl.debug)
    fprintf (progress, "Parsing (slow_registers_only: %s)...\n",
             cl.slow_registers_only ? "yes" : "no");
  if (! strcmp (cl.input_file, "-"))
    p = vmprefix_parse_file_star_possibly_with_slow_registers_only
           (stdin, cl.slow_registers_only);
  else
    p = vmprefix_parse_file_possibly_with_slow_registers_only
           (cl.input_file, cl.slow_registers_only);
  if (cl.debug)
    fprintf (progress, "The requried slow register number is %li per class.\n",
             (long) p->slow_register_per_class_no);

  if (cl.debug)
    fprintf (progress, "Specializing...\n");
  vmprefix_specialize_program (p);

  if (cl.print_program)
    {
      if (cl.debug)
        fprintf (progress, "Printing back the program...\n");
      vmprefix_print_program_possibly_with_slow_registers_only
         (stdout, p, cl.slow_registers_only);
    }

  if (cl.disassemble_program)
    {
      if (cl.debug)
        fprintf (progress, "Disassembling...\n");
      vmprefix_disassemble_program (p, true, cl.objdump_name,
                                    cl.objdump_options);
    }

  /* If we printed back the program or disassembled, this run is not performance
     critical and we afford a couple more syscalls.  Flush the output buffer, so
     that the program is visible before running, and possibly crashing. */
  if (cl.print_program || cl.disassemble_program)
    {
      fflush (stdout);
      fflush (stderr);
    }

  if (cl.run_program)
    {
      if (cl.debug)
        fprintf (progress, "Initializing VM state...\n");
      struct vmprefix_state s;
      vmprefix_state_initialize (& s);

      if (cl.debug)
        fprintf (progress, "Interpreting...\n");
      vmprefix_interpret (p, &s);

      /*
      printf ("%%r0: %li 0x%lx\n",
              (long) s.vmprefix_state_runtime.jitter_fast_register_r_0.fixnum,
              (unsigned long) s.vmprefix_state_runtime.jitter_fast_register_r_0.fixnum);
      */
      if (cl.debug)
        fprintf (progress, "Finalizing VM state...\n");
      vmprefix_state_finalize (& s);
    }

  if (cl.debug)
    fprintf (progress, "Destroying the program data structure...\n");
  vmprefix_destroy_program (p);

  if (cl.debug)
    fprintf (progress, "Finalizing...\n");
  vmprefix_finalize ();

  if (cl.debug)
    fprintf (progress, "Still alive at exit.\n");
  return 0;
}
