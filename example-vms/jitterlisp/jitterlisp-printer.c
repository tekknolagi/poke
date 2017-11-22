/* Jittery Lisp: printer.

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

#include <stdio.h>

#include <jitter/jitter-dynamic-buffer.h>

#include "jitterlisp-printer.h"
#include "jitterlisp-settings.h"
#include "jitterlisp-sexpression.h"




/* Predefined char-printers.
 * ************************************************************************** */

void
jitterlisp_dynamic_buffer_char_printer_function (void *dynamic_buffer, char c)
{
  struct jitter_dynamic_buffer *db = dynamic_buffer;
  jitter_dynamic_buffer_push (db, & c, 1);
}

void
jitterlisp_stream_char_printer_function (void *file_star, char c)
{
  fputc (c, file_star);
}




/* Scratch: terminal sequences.
 * ************************************************************************** */

/* Define ANSI terminal escape sequences to print each object with attributes
   depending on its tag. */

#define ESC        "\033"
#define NOATTR     ESC"[0m"
#define BOLD       ESC"[1m"
#define FAINT      ESC"[2m"
#define ITALIC     ESC"[3m"
#define UNDERLINE  ESC"[4m"
#define REVERSE    ESC"[7m"
#define CROSSOUT   ESC"[9m"

#define BLACK        ESC"[0m"ESC"[30m"
#define WHITE        ESC"[1m"ESC"[37m"
#define BLUE         ESC"[0m"ESC"[34m"
#define LIGHTBLUE    ESC"[1m"ESC"[34m"
#define GREEN        ESC"[0m"ESC"[32m"
#define LIGHTGREEN   ESC"[1m"ESC"[32m"
#define CYAN         ESC"[0m"ESC"[36m"
#define LIGHTCYAN    ESC"[1m"ESC"[36m"
#define RED          ESC"[0m"ESC"[31m"
#define LIGHTRED     ESC"[1m"ESC"[31m"
#define MAGENTA      ESC"[0m"ESC"[35m"
#define LIGHTMAGENTA ESC"[1m"ESC"[35m"
#define BROWN        ESC"[0m"ESC"[33m"
#define LIGHTGRAY    ESC"[0m"ESC"[37m"
#define DARKGRAY     ESC"[1m"ESC"[30m"
#define LIGHTBLUE    ESC"[1m"ESC"[34m"
#define YELLOW       ESC"[1m"ESC"[33m"

//#define NOTERMINAL

#ifdef NOTERMINAL
# define CONSATTR             ""
# define CHARACTERATTR        ""
# define FIXNUMATTR           ""
# define INTERNEDSYMBOLATTR   ""
# define UNINTERNEDSYMBOLATTR ""
# define UNIQUEATTR           ""
# define ERRORATTR            ""
#else
# define CONSATTR             LIGHTRED
# define CHARACTERATTR        BROWN UNDERLINE ITALIC
# define FIXNUMATTR           LIGHTGREEN UNDERLINE
# define INTERNEDSYMBOLATTR   YELLOW ITALIC
# define UNINTERNEDSYMBOLATTR YELLOW ITALIC UNDERLINE
# define UNIQUEATTR           LIGHTMAGENTA UNDERLINE ITALIC
# define ERRORATTR            RED REVERSE
#endif // #ifdef NOTERMINAL




/* Char-printing utility.
 * ************************************************************************** */

/* Print the pointed '\0'-terminated string using the given char-printer. */
static void
jitterlisp_print_string (jitterlisp_char_printer_function char_printer,
                         void *char_printer_state,
                         const char *s)
{
  const char *p;
  for (p = s; * p != '\0'; p ++)
    char_printer (char_printer_state, *p);
}

/* A helper function for jitterlisp_print_long_long .  The argument n is
   required to be strictly positive. */
static void
jitterlisp_print_long_long_recursive (jitterlisp_char_printer_function cp,
                                      void *cps,
                                      jitter_long_long n)
{
  /* If the number is zero we have nothing more to print.  Notice that this is
     only reached if the original number to print was non-zero, in which case
     we are printing the other digits on the right. */
  if (n == 0)
    return;

  /* Recursively print every digit but the last one, which is to say the number
     divided by ten, rounded down.  We are going to print the least significant
     digit right after this call, so that it correctly ends up on the right. */
  jitterlisp_print_long_long_recursive (cp, cps, n / 10);

  /* Print the least significant digit but the last one. */
  cp (cps, '0' + n % 10);
}

/* Print the given jitter_long_long signed integer using the given
   char-printer. */
static void
jitterlisp_print_long_long (jitterlisp_char_printer_function char_printer,
                            void *char_printer_state,
                            jitter_long_long n)
{
  /* Print a minus sign, if negative; in that case change n to be positive, so
     that we can forget about the sign in what follows. */
  if (n < 0)
    {
      char_printer (char_printer_state, '-');
      n = - n;
    }

  /* If the number is zero print a zero digit, and we're done. */
  if (n == 0)
    {
      char_printer (char_printer_state, '0');
      return;
    }

  /* The number we have to print if we arrived at this point is strictly
     positive.  Use the recursive helper. */
  jitterlisp_print_long_long_recursive (char_printer, char_printer_state, n);
}

/* Print the given character using the given char-printer.  This is defined simply
   to have a function with similar arguments to jitterlisp_print_string. */
static void
jitterlisp_print_char (jitterlisp_char_printer_function char_printer,
                       void *char_printer_state,
                       char c)
{
  char_printer (char_printer_state, c);
}

/* Print a terminal escape sequence for color/font decorations, if colorization
   is enabled; do nothing otherwise.  By convention every printing function is
   supposed to print the NOATTR decoration at the end. */
static void
jitterlisp_print_decoration (jitterlisp_char_printer_function char_printer,
                             void *char_printer_state,
                             const char *s)
{
  if (jitterlisp_settings.colorize)
    jitterlisp_print_string (char_printer, char_printer_state, s);
}




/* S-expression printer.
 * ************************************************************************** */

/* Print o as the cdr of a cons, with the car already printed and the
   surrounding parentheses printed by the caller, using the given
   char-printer. */
static void
jitterlisp_print_cdr (jitterlisp_char_printer_function cp, void *cps,
                      jitterlisp_object o)
{
  if (JITTERLISP_IS_EMPTY_LIST(o))
    {
      /* There is nothing to print, not even a space: the caller has already
         written the open parens and will append the matching closed parens
         right after this function returns. */
    }
  else if (JITTERLISP_IS_CONS(o))
    {
      /* So, o is another cons: print o's car as the next list element, but
         first separate it from the previous element, which must exist if we got
         here, with a space. */
      jitterlisp_print_char (cp, cps, ' ');
      struct jitterlisp_cons * const c = JITTERLISP_CONS_DECODE(o);
      jitterlisp_print (cp, cps, c->car);

      /* We're still within a list or improper/dotted list and so we'll keep
         using cdr notation for o's cdr, without adding more parens.  If o's cdr
         is still a cons then the recursive call will prepend a space to the
         elements. */
      jitterlisp_print_cdr (cp, cps, c->cdr);
    }
  else
    {
      /* The innermost cdr of the spine is not (): this is an improper/dotted
         list. */
      jitterlisp_print_decoration (cp, cps, CONSATTR);
      jitterlisp_print_string (cp, cps, " . ");
      jitterlisp_print_decoration (cp, cps, NOATTR);
      jitterlisp_print (cp, cps, o);
    }
}

void
jitterlisp_print (jitterlisp_char_printer_function cp, void *cps,
                  jitterlisp_object o)
{
  if (JITTERLISP_IS_FIXNUM(o))
    {
      jitter_int decoded = JITTERLISP_FIXNUM_DECODE(o);
      jitterlisp_print_decoration (cp, cps, FIXNUMATTR);
      jitterlisp_print_long_long (cp, cps, decoded);
      jitterlisp_print_decoration (cp, cps, NOATTR);
    }
  else if (JITTERLISP_IS_UNIQUE(o))
    {
      jitter_uint index = JITTERLISP_UNIQUE_DECODE(o);
      if (index < JITTERLISP_UNIQUE_OBJECT_NO)
        {
          jitterlisp_print_decoration (cp, cps, UNIQUEATTR);
          jitterlisp_print_string (cp, cps,
                                   jitterlisp_unique_object_names [index]);
        }
      else
        {
          jitterlisp_print_decoration (cp, cps, NOATTR);
          jitterlisp_print_decoration (cp, cps, ERRORATTR);
          jitterlisp_print_string (cp, cps, "#<invalid-unique-object:");
          jitterlisp_print_long_long (cp, cps, index);
          jitterlisp_print_char (cp, cps, '>');
        }
      jitterlisp_print_decoration (cp, cps, NOATTR);
    }
  else if (JITTERLISP_IS_CHARACTER(o))
    {
      jitterlisp_print_decoration (cp, cps, CHARACTERATTR);
      jitter_int c = JITTERLISP_CHARACTER_DECODE(o);
      switch (c)
        {
        case ' ':  jitterlisp_print_string (cp, cps, "#\\space");       break;
        case '\0': jitterlisp_print_string (cp, cps, "#\\nul");         break;
        case '\r': jitterlisp_print_string (cp, cps, "#\\return");      break;
        case '\n': jitterlisp_print_string (cp, cps, "#\\newline");     break;
        default:
          jitterlisp_print_string (cp, cps, "#\\");
          jitterlisp_print_char (cp, cps, c);
          break;
        }
      jitterlisp_print_decoration (cp, cps, NOATTR);
    }
  else if (JITTERLISP_IS_SYMBOL(o))
    {
      struct jitterlisp_symbol *s = JITTERLISP_SYMBOL_DECODE(o);
      if (s->name_or_NULL != NULL)
        {
          jitterlisp_print_decoration (cp, cps, INTERNEDSYMBOLATTR);
          jitterlisp_print_string (cp, cps, s->name_or_NULL);
        }
      else
        {
          jitterlisp_print_decoration (cp, cps, UNINTERNEDSYMBOLATTR);
          jitterlisp_print_string (cp, cps, "#<uninterned-symbol>");
        }
      jitterlisp_print_decoration (cp, cps, NOATTR);
    }
  else if (JITTERLISP_IS_CONS(o))
    {
      struct jitterlisp_cons * const c = JITTERLISP_CONS_DECODE(o);
      jitterlisp_object car = c->car;
      jitterlisp_object cdr = c->cdr;
      jitterlisp_print_decoration (cp, cps, CONSATTR);
      jitterlisp_print_char (cp, cps, '(');
      jitterlisp_print_decoration (cp, cps, NOATTR);
      jitterlisp_print (cp, cps, car);
      jitterlisp_print_cdr (cp, cps, cdr);
      jitterlisp_print_decoration (cp, cps, CONSATTR);
      jitterlisp_print_char (cp, cps, ')');
      jitterlisp_print_decoration (cp, cps, NOATTR);
    }
  else
    {
      jitterlisp_print_string (cp, cps, ERRORATTR);
      jitterlisp_print_string (cp, cps, "#<invalid-or-unknown>");
      jitterlisp_print_string (cp, cps, NOATTR);
    }
}




/* S-expression printer: convenience API hiding char-printers.
 * ************************************************************************** */

void
jitterlisp_print_to_stream (FILE *f, jitterlisp_object o)
{
  jitterlisp_print (jitterlisp_stream_char_printer_function, f, o);
}

char *
jitterlisp_print_to_string (jitterlisp_object o)
{
  /* Make a temporary dynamic buffer. */
  struct jitter_dynamic_buffer db;
  jitter_dynamic_buffer_initialize (& db);

  /* Print to it. */
  jitterlisp_print (jitterlisp_dynamic_buffer_char_printer_function, & db, o);

  /* Add a '\0' terminating character. */
  char zero = '\0';
  jitter_dynamic_buffer_push (& db, & zero, 1);

  /* Extract the dynamic buffer data, trim it and return it.  Here finalizing
     the dynamic buffer is not needed and indeed would be incorrect, since we
     are reusing its heap-allocated data. */
  return jitter_dynamic_buffer_extract_trimmed (& db);
}
