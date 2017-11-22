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
#include <string.h>
#include <unistd.h>

#include <jitter/jitter.h>
#include <jitter/jitter-cpp.h>
#include <jitter/jitter-dynamic-buffer.h>
#include <jitter/jitter-fatal.h>
#include <jitter/jitter-readline.h>

#include "jitterlisp.h"


/* Command line handling using argp.
 * ************************************************************************** */

/* An enumerate to represent options with no short version.  These
   are options negating some other option: any negative option serves
   to reset a setting to its default value. */
enum jitterlisp_negative_option
  {
    jitterlisp_negative_option_no_verbose = -1,
    jitterlisp_negative_option_repl = -2,
    jitterlisp_negative_option_no_colorize = -3
  };

/* Numeric keys for options having only a long format.  These must not conflict
   with any value in enum jitterlisp_negative_option . */
enum jitterlisp_long_only_option
  {
    jitterlisp_long_only_option_no_repl = -4,
    jitterlisp_long_only_option_dump_version = -5
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

   /* Command-line s-expression evaluation. */
   {NULL, '\0', NULL, OPTION_DOC, "Command-line s-expression evaluation:", 20},
   {"eval", 'e', "SEXPRS", 0,
    "Evaluate the given s-expressions after running the files (if any) "
    "and before running the REPL (unless the REPL is disabled)" },

   /* Debugging options. */
   {NULL, '\0', NULL, OPTION_DOC, "Debugging options:", 30},
   {"colorize", 'c', NULL, 0,
    "Colorize s-expressions with ANSI terminal escape sequences" },
   {"verbose", 'v', NULL, 0,
    "Show progress information at run time" },
   /* Debugging negative options. */
   {NULL, '\0', NULL, OPTION_DOC, "", 31},
   {"no-colorize", jitterlisp_negative_option_no_colorize, NULL, 0,
    "Don't colorize s-expressions (default)"},
   {"no-verbose", jitterlisp_negative_option_no_verbose, NULL, 0,
    "Don't show progress information (default)"},

   {NULL, '\0', NULL, OPTION_DOC, "Scripting options:", 40},
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
    /* Command-line initialization. */
    case ARGP_KEY_INIT:
      /* Nothing particular to do, at this point.. */
      break;

    /* File options. */
    case jitterlisp_long_only_option_no_repl:
    case 'q':
      sp->repl = false;
      break;

    /* File negative options. */
    case jitterlisp_negative_option_repl:
      sp->repl = true;
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

    /* Debugging negative options. */
    case jitterlisp_negative_option_no_verbose:
      sp->verbose = false;
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




/* Allocation (very tentative).
 * ************************************************************************** */

struct jitterlisp_cons*
jitterlisp_make_unencoded_cons (jitterlisp_object a, jitterlisp_object b)
{
  struct jitterlisp_cons *res = JITTERLISP_CONS_MAKE_UNINITIALIZED_UNENCODED();
  res->car = a;
  res->cdr = b;
  return res;
}


/* Binary printing.
 * ************************************************************************** */

/* FIXME: move these to a new file under jitter/ after I rebase all the
   changesets about JitterLisp into something clean.  Changing the history on
   this might be messy, unless I do it after everything else. */

// FIXME: I might want to keep this around.
/* A helper function for jitter_print_binary_padded, with the same API, but not
   printing the "0b" prefix and not printing any "0" digit for u == 0, unless
   padding requires it. */
static void
jitter_print_binary_recursive (FILE *stream, jitter_uint u, int digit_no)
{
  /* When the number is zero we have nothing more to print, except if we have to
     fill at least digit_no digits; in that case we print the approporiate
     number of zero digits, and we're done.  Notice that the recursive call is
     on the left, so these padding zeroes correctly end up in the least
     significant digits of the output. */
  if (u == 0)
    {
      int i;
      for (i = 0; i < digit_no; i ++)
        fputc ('0', stream);
      return;
    }

  /* Recursively print half the number rounded down, which is to say every
     binary digit but the least significant, in order. */
  jitter_print_binary_recursive (stream, u / 2, digit_no - 1);

  /* Print the least significant digit. */
  fputc ('0' + (u & 1), stream);
}

// FIXME: I might want to keep this around.
/* Print the given number in binary to the pointed stream, using at least
   digit_no binary digits (left-padding with zeroes), and prepending a "0b"
   prefix. */
void
jitter_print_binary_padded (FILE *stream, jitter_uint u, int digit_no)
{
  fputs ("0b", stream);
  jitter_print_binary_recursive (stream, u, digit_no);
}

// FIXME: I might want to keep this around.
/* Print the given number in binary to the pointed stream, prepending a "0b"
   prefix. */
void
jitter_print_binary (FILE *stream, jitter_uint u)
{
  /* Print a binary number with at least one digit (which may be zero). */
  jitter_print_binary_padded (stream, u, 1);
}



/* Scratch.
 * ************************************************************************** */

__attribute__ ((unused))
static void
print (jitterlisp_object o)
{
#define WIDTH "020"
  printf ("* 0x%" WIDTH JITTER_PRIx "  (tag ", o);
  jitter_print_binary_padded (stdout, JITTERLISP_GET_TAG(o),
                              JITTERLISP_TAG_BIT_NO);
  printf (") ");
  if (JITTERLISP_IS_FIXNUM(o))
    printf ("is a fixnum");
  else if (JITTERLISP_IS_UNIQUE(o))
    printf ("is a unique object");
  else if (JITTERLISP_IS_CHARACTER(o))
    printf ("is a character");
  else if (JITTERLISP_IS_UNIQUE_OR_CHARACTER(o))
    printf ("is a unique-or-character (this shouldn't happen)");
  else if (JITTERLISP_IS_SYMBOL(o))
    printf ("is a symbol");
  else if (JITTERLISP_IS_CONS(o))
    printf ("is a cons");
  else
    printf ("is unknown");
  printf ("\n");
  printf ("  0d%" WIDTH JITTER_PRIu "U\n", o);
  jitter_int signed_o = o;
  if (signed_o < 0)
    printf (" -0d%" WIDTH JITTER_PRIi "\n", - signed_o);
  else
    printf ("  0d%" WIDTH JITTER_PRIi "\n", signed_o);
  printf ("  ");
  jitter_print_binary_padded (stdout, o, JITTER_BITS_PER_WORD);
  printf ("\n");
  printf ("  "); jitterlisp_print_to_stream (stdout, o); printf ("\n");
#undef WIDTH
}

__attribute__ ((noreturn, noinline, noclone, cold, unused))
static void
type_error (void)
{
  jitter_fatal ("type error");
}

#define type_error()                             \
  do                                             \
    {                                            \
      /*goto * jitterlisp_error_handler_register;*/  \
      type_error ();                             \
    }                                            \
  while (false)

#define JITTERLISP_REQUIRE_TAG(_jitterlisp_object, _jitterlisp_tag)     \
  do                                                                    \
    {                                                                   \
      if (__builtin_expect (! JITTERLISP_HAS_TAG((_jitterlisp_object),  \
                                                 (_jitterlisp_tag)),    \
                            false))                                     \
        {                                                               \
          type_error ();                                                \
        }                                                               \
    }                                                                   \
  while (false)

#define JITTERLISP_REQUIRE_TYPE(_jitterlisp_object, _jitterlisp_TYPE)    \
  do                                                                     \
    {                                                                    \
      if (__builtin_expect (                                             \
            ! JITTER_CONCATENATE_TWO(JITTERLISP_IS_, _jitterlisp_TYPE)(  \
                 (_jitterlisp_object)),                                  \
            false))                                                      \
        {                                                                \
          type_error ();                                                 \
        }                                                                \
    }                                                                    \
  while (false)


jitterlisp_object
jitterlisp_fixnum_plus (jitterlisp_object a, jitterlisp_object b)
{
  return JITTERLISP_FIXNUM_PLUS(a, b);
}

jitterlisp_object
jitterlisp_fixnum_uminus (jitterlisp_object a)
{
  return JITTERLISP_FIXNUM_UNARY_MINUS(a);
}

jitterlisp_object
jitterlisp_fixnum_minus (jitterlisp_object a, jitterlisp_object b)
{
  return JITTERLISP_FIXNUM_MINUS(a, b);
}

jitterlisp_object
jitterlisp_fixnum_times (jitterlisp_object a, jitterlisp_object b)
{
  return JITTERLISP_FIXNUM_TIMES(a, b);
}

jitterlisp_object
jitterlisp_fixnum_divided (jitterlisp_object a, jitterlisp_object b)
{
  return JITTERLISP_FIXNUM_DIVIDED(a, b);
}

jitterlisp_object
jitterlisp_fixnum_remainder (jitterlisp_object a, jitterlisp_object b)
{
  return JITTERLISP_FIXNUM_REMAINDER(a, b);
}

jitterlisp_object
jitterlisp_fixnum_less (jitterlisp_object a, jitterlisp_object b)
{
  return JITTERLISP_FIXNUM_LESS(a, b);
}

jitterlisp_object
jitterlisp_fixnum_notgreater (jitterlisp_object a, jitterlisp_object b)
{
  return JITTERLISP_FIXNUM_NOTGREATER(a, b);
}

bool
jitterlisp_is_unique (jitterlisp_object a)
{
  return JITTERLISP_IS_UNIQUE(a);
}

bool
jitterlisp_is_character (jitterlisp_object a)
{
  return JITTERLISP_IS_CHARACTER(a);
}

bool
jitterlisp_is_empty_list (jitterlisp_object a)
{
  return JITTERLISP_IS_EMPTY_LIST(a);
}

bool
jitterlisp_is_boolean (jitterlisp_object a)
{
  return JITTERLISP_IS_BOOLEAN(a);
}

bool
jitterlisp_is_false (jitterlisp_object a)
{
  return JITTERLISP_IS_FALSE(a);
}

bool
jitterlisp_is_fixnum (jitterlisp_object a)
{
  return JITTERLISP_IS_FIXNUM(a);
}

int
jitterlisp_decode_character (jitterlisp_object a)
{
  return JITTERLISP_CHARACTER_DECODE(a);
}

jitterlisp_object
jitterlisp_encode_boolean (bool b)
{
  return JITTERLISP_BOOLEAN_ENCODE(b);
}

bool
jitterlisp_decode_boolean (jitterlisp_object a)
{
  return JITTERLISP_BOOLEAN_DECODE(a);
}

jitterlisp_object
jitterlisp_plus_of_constants (void)
{
  jitterlisp_object a = JITTERLISP_FIXNUM_ENCODE(40);
  jitterlisp_object b = JITTERLISP_FIXNUM_ENCODE(2);
  return JITTERLISP_FIXNUM_PLUS(a, b);
}

jitterlisp_object
jitterlisp_cons (jitterlisp_object a, jitterlisp_object b)
{
  struct jitterlisp_cons *c = JITTERLISP_CONS_MAKE_UNINITIALIZED_UNENCODED();
  c->car = a;
  c->cdr = b;
  return JITTERLISP_CONS_ENCODE(c);
}

jitterlisp_object
jitterlisp_symbol (char *name)
{
  return JITTERLISP_SYMBOL_ENCODE(jitterlisp_symbol_make_interned (name));
}

jitterlisp_object
jitterlisp_uninterned_symbol (void)
{
  return JITTERLISP_SYMBOL_ENCODE(jitterlisp_symbol_make_uninterned ());
}

jitterlisp_object
jitterlisp_car_unsafe (jitterlisp_object a)
{
  return JITTERLISP_CONS_DECODE(a)->car;
}

jitterlisp_object
jitterlisp_cdr_unsafe (jitterlisp_object a)
{
  return JITTERLISP_CONS_DECODE(a)->cdr;
}

jitterlisp_object
jitterlisp_car (jitterlisp_object a)
{
  JITTERLISP_REQUIRE_TYPE(a, CONS);

  return jitterlisp_car_unsafe (a);
}

jitterlisp_object
jitterlisp_cdr (jitterlisp_object a)
{
  JITTERLISP_REQUIRE_TYPE(a, CONS);

  return jitterlisp_cdr_unsafe (a);
}

jitterlisp_object
jitterlisp_cadddr (jitterlisp_object a)
{
  return jitterlisp_car (jitterlisp_cdr (jitterlisp_cdr (jitterlisp_cdr (a))));
}

void
jitterlisp_require_fixnum (jitterlisp_object a)
{
  JITTERLISP_REQUIRE_TYPE(a, FIXNUM);
}

jitterlisp_object
jitterlisp_nullp (jitterlisp_object a)
{
  return JITTERLISP_BOOLEAN_ENCODE(JITTERLISP_IS_CONS(a));
}

jitterlisp_object
jitterlisp_iota (jitterlisp_object limit)
{
  jitterlisp_object res = JITTERLISP_EMPTY_LIST;

  int i;
  for (i = JITTERLISP_FIXNUM_DECODE(limit) - 1; i >= 0; i --)
    res = jitterlisp_cons (JITTERLISP_FIXNUM_ENCODE(i), res);
  return res;
}

jitter_uint
jitterlisp_two_tags (jitterlisp_object a, jitterlisp_object b)
{
  jitter_uint tag_a = JITTERLISP_GET_TAG(a);
  jitter_uint tag_b = JITTERLISP_GET_TAG(b);
  return (tag_a << JITTERLISP_TAG_BIT_NO) | tag_b;
}

typedef
jitterlisp_object (*jitterlisp_two_arity_operator) (jitterlisp_object,
                                                    jitterlisp_object);
typedef
jitterlisp_object (*jitterlisp_one_arity_operator) (jitterlisp_object);

jitter_uint
jitterlisp_two_tag_dispatch_unary
  (jitterlisp_object a, jitterlisp_one_arity_operator *functions)
{
  jitter_uint tag_a = JITTERLISP_GET_TAG(a);
  return functions [tag_a] (a);
}

jitter_uint
jitterlisp_two_tag_dispatch_2d
  (jitterlisp_object a, jitterlisp_object b,
   jitterlisp_two_arity_operator **functions)
{
  jitter_uint tag_a = JITTERLISP_GET_TAG(a);
  jitter_uint tag_b = JITTERLISP_GET_TAG(b);
  return functions [tag_a] [tag_b] (a, b);
}

jitter_uint
jitterlisp_two_tag_dispatch_2d_one_known_dimenstion
  (jitterlisp_object a, jitterlisp_object b,
   const jitterlisp_two_arity_operator functions[][JITTERLISP_TAG_BIT_NO])
{
  jitter_uint tag_a = JITTERLISP_GET_TAG(a);
  jitter_uint tag_b = JITTERLISP_GET_TAG(b);
  return functions [tag_a] [tag_b] (a, b);
}

jitter_uint
jitterlisp_two_tag_dispatch_1d
  (jitterlisp_object a, jitterlisp_object b,
   const jitterlisp_two_arity_operator functions[])
{
  jitter_uint tag_a = JITTERLISP_GET_TAG(a);
  jitter_uint tag_b = JITTERLISP_GET_TAG(b);
  return functions [(tag_a << JITTERLISP_TAG_BIT_NO) | tag_b] (a, b);
}

jitter_uint
jitterlisp_two_tag_dispatch_1d_alt
  (jitterlisp_object a, jitterlisp_object b,
   const jitterlisp_two_arity_operator functions[])
{
  jitter_uint tag_a = JITTERLISP_GET_TAG(a);
  jitter_uint tag_b = JITTERLISP_GET_TAG(b);
  return (* (functions + ((tag_a * JITTERLISP_TAG_NO) | tag_b))) (a, b);
}

jitter_uint
jitterlisp_two_tag_dispatch_1d_alt2
  (jitterlisp_object a, jitterlisp_object b,
   const jitterlisp_two_arity_operator functions[])
{
  jitter_uint tag_a = JITTERLISP_GET_TAG(a);
  jitter_uint tag_b = JITTERLISP_GET_TAG(b);
  jitter_uint offset
    = ((tag_a << JITTERLISP_TAG_BIT_NO
        | tag_b)
       << JITTERLISP_TAG_BIT_NO);
  return ((* ((jitterlisp_two_arity_operator *)
             ((char *) functions) + offset))
          (a, b));
}

jitter_uint
jitterlisp_factorial (jitterlisp_object a)
{
  JITTERLISP_REQUIRE_TYPE(a, FIXNUM);

  jitter_int untagged_a = JITTERLISP_FIXNUM_DECODE (a);
  jitter_int untagged_res = 1;
  jitter_int i;
  for (i = 2; i <= untagged_a; i ++)
    untagged_res *= i;

  return JITTERLISP_FIXNUM_ENCODE(untagged_res);
}

jitterlisp_object
jitterlisp_last (jitterlisp_object a)
{
  jitterlisp_object candidate_last_cons = a;
  do
    {
      JITTERLISP_REQUIRE_TYPE(candidate_last_cons, CONS);

      jitterlisp_object cdr = JITTERLISP_CONS_DECODE(candidate_last_cons)->cdr;
      if (JITTERLISP_IS_EMPTY_LIST(cdr))
        return JITTERLISP_CONS_DECODE(candidate_last_cons)->car;
      else
        candidate_last_cons = cdr;
    }
  while (true);
}




/* Run from input.  FIXME: move this section.
 * ************************************************************************** */

jitterlisp_object
jitterlisp_run (jitterlisp_object form)
{
  printf ("Pretending to run ");
  jitterlisp_print_to_stream (stdout, form);
  printf ("\n");
  jitterlisp_object res = JITTERLISP_NOTHING;
  printf ("The result is ");
  jitterlisp_print_to_stream (stdout, res);
  printf ("\n");
  return res;
}

/* Parse s-expressions from the given string and run each of them. */
void
jitterlisp_run_from_string (const char *string)
{
  printf ("Running from string \"%s\"...\n", string);
  struct jitterlisp_reader_state *rstate
    = jitterlisp_make_string_reader_state (string);
  jitterlisp_object form;
  while (! JITTERLISP_IS_EOF (form = jitterlisp_read (rstate)))
    jitterlisp_run (form);
  jitterlisp_destroy_reader_state (rstate);
  printf ("...Done running from string \"%s\".\n", string);
}

/* Parse s-expressions from the named file and run each of them. */
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

  /* An s-expression. */
  jitterlisp_object form;

  /* Read forms from the input file and run each of them. */
  struct jitterlisp_reader_state *rstate
    = jitterlisp_make_stream_reader_state (in);
  while (! JITTERLISP_IS_EOF (form = jitterlisp_read (rstate)))
    jitterlisp_run (form);
  jitterlisp_destroy_reader_state (rstate);

  /* Close the input stream. */
  fclose (in);
  printf ("...Done running from file \"%s\".\n", path_name);
}




/* REPL.
 * ************************************************************************** */

struct jitterlisp_readline_char_reader_state
{
  const char *prompt;
  bool got_EOF;
  char *last_line_or_NULL;
  char *next_char_p;
};

static int
jitterlisp_readline_char_reader_function (jitterlisp_char_reader_state *crspp)
{
  struct jitterlisp_readline_char_reader_state *crsp
    = * (struct jitterlisp_readline_char_reader_state **) crspp;

  /* If we already saw EOF refuse to read any more lines, and return EOF. */
  if (crsp->got_EOF)
    {
      printf ("[returning a previously found EOF]\n");
      return EOF;
    }

  /* If we haven't got a line read one... */
  if (crsp->last_line_or_NULL == NULL)
    {
      printf ("[we need to read a line...]\n");
      /* ...But if the line we receive from readline is NULL we've found EOF. */
      if ((crsp->last_line_or_NULL = jitter_readline (crsp->prompt))
          == NULL)
        {
          printf ("  [returning a just-found EOF]\n");
          crsp->got_EOF = true;
          return EOF;
        }
      printf ("  [it was a non-EOF line]\n");
      /* If we haven't returned yet then we have a non-NULL line: set the next
         character pointer to its beginning, and go on. */
      crsp->next_char_p = crsp->last_line_or_NULL;
    }

  /* If we arrived here then we have an actual line to read from, and
     crsp->next_char_p points within it. */

  /* Does crsp->next_char_p point to a '\0' character?  If so, we have to
     interpret that as a '\n' character (which readline strips off), and prepare
     to read a new entire line at the next call. */
  if (* crsp->next_char_p == '\0')
    {
      printf ("[returning newline]\n");
      free (crsp->last_line_or_NULL);
      crsp->last_line_or_NULL = NULL;
      return '\n';
    }

  /* If we arrived here then the next character is ordinary. */
  printf ("[returning ordinary character '%c']\n", * crsp->next_char_p);
  return * (crsp->next_char_p ++);
}

void
jitterlisp_repl (void)
{
  printf ("Running the REPL...\n");
  /* Initialize a readline state data structure. */
  struct jitterlisp_readline_char_reader_state crstate;
  crstate.prompt = "jitterlisp> ";
  crstate.last_line_or_NULL = NULL;
  crstate.got_EOF = false;
  crstate.next_char_p = NULL;

  /* Use the readline state data structure and the readline char-reader
     function as a reader state.  Read forms from there until we get to
     #<eof>, and run each one. */
  struct jitterlisp_reader_state *rstate
    = jitterlisp_make_reader_state (jitterlisp_readline_char_reader_function,
                                    & crstate);
  jitterlisp_object form;
  while (! JITTERLISP_IS_EOF (form = jitterlisp_read (rstate)))
    jitterlisp_run (form);
  jitterlisp_destroy_reader_state (rstate);
  printf ("...Done running the REPL.\n");
}




/* Main function.
 * ************************************************************************** */

int
main (int argc, char **argv)
{
  /* Initialize JitterLisp.  This initializes the settings data structure
     with default values, so it must be called before argp_parse , which
     may change those. */
  jitterlisp_initialize ();

  /* Parse our arguments; jitterlisp_settings will contain the information
     provided in the command line.  We also define the "settings pointer"
     sp as a pointer to it, for convenience. */
  argp_parse (& argp, argc, argv, 0, 0, & jitterlisp_settings);
  struct jitterlisp_settings * const sp = & jitterlisp_settings;

  /* Run the input files.  The final free just serves to placate Valgrind and
     not to distract me from any actual problem. */
  size_t input_file_path_name_no
    = sp->input_file_path_names.used_size / sizeof (char *);
  char **input_file_path_names =
    jitter_dynamic_buffer_extract (& sp->input_file_path_names);
  int i;
  for (i = 0; i < input_file_path_name_no; i ++)
    jitterlisp_run_from_named_file (input_file_path_names [i]);
  free (input_file_path_names);

  /* Evaluate s-expressions from the command line, if any. */
  if (sp->sexps_string != NULL)
    jitterlisp_run_from_string (sp->sexps_string);

  /* Run the REPL, if enabled. */
  if (sp->repl)
    jitterlisp_repl ();

  /* Finalize JitterLisp. */
  jitterlisp_finalize ();

  return 0;
}
