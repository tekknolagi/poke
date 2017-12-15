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

#include "jitterlisp-printer.h"

#include <stdio.h>

#include <jitter/jitter-dynamic-buffer.h>
#include <jitter/jitter-fatal.h>

#include "jitterlisp-settings.h"
#include "jitterlisp-sexpression.h"
#include "jitterlisp-ast.h"




/* Character names.
 * ************************************************************************** */

const struct jitterlisp_character_name_binding
jitterlisp_non_ordinary_character_name_bindings []
  =
    {
      { '\0', "nul" },
      { ' ',  "space" },
      { '\n', "newline" },
      { '\n', "linefeed" },
      { '\r', "cr" },
      { '\r', "return" },
      { '\f', "page" }
    };

const size_t
jitterlisp_non_ordinary_character_name_binding_no
  = (sizeof (jitterlisp_non_ordinary_character_name_bindings)
     / sizeof (const struct jitterlisp_character_name_binding));




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
# define CONSATTR              ""
# define CHARACTERATTR         ""
# define FIXNUMATTR            ""
# define INTERNEDSYMBOLATTR    ""
# define UNINTERNEDSYMBOLATTR  ""
# define UNIQUEATTR            ""
# define CLOSUREATTR           ""
# define NONPRIMITIVEMACROATTR ""
# define PRIMITIVEATTR         ""
# define PRIMITIVEMACROATTR    ""
# define VECTORATTR            ""
# define ASTATTR               ""
# define ERRORATTR             ""
#else
# define CONSATTR              LIGHTRED // LIGHTRED // WHITE //LIGHTRED // YELLOW //LIGHTMAGENTA
# define CHARACTERATTR         BROWN UNDERLINE ITALIC
# define FIXNUMATTR            LIGHTCYAN UNDERLINE
# define INTERNEDSYMBOLATTR    LIGHTGREEN
# define UNINTERNEDSYMBOLATTR  LIGHTGREEN ITALIC UNDERLINE
# define UNIQUEATTR            LIGHTMAGENTA UNDERLINE //YELLOW UNDERLINE ITALIC //LIGHTMAGENTA UNDERLINE ITALIC
# define CLOSUREATTR           LIGHTMAGENTA ITALIC // WHITE
# define NONPRIMITIVEMACROATTR LIGHTMAGENTA ITALIC UNDERLINE // WHITE
# define PRIMITIVEATTR         LIGHTMAGENTA
# define PRIMITIVEMACROATTR    LIGHTMAGENTA UNDERLINE
# define VECTORATTR            LIGHTRED ITALIC UNDERLINE
# define ASTATTR               LIGHTMAGENTA ITALIC UNDERLINE
# define ERRORATTR             RED REVERSE
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
   required to be strictly positive, and can use all the available bits. */
static void
jitterlisp_print_long_long_recursive (jitterlisp_char_printer_function cp,
                                      void *cps,
                                      jitter_ulong_long n,
                                      unsigned radix)
{
  /* If the number is zero we have nothing more to print.  Notice that this is
     only reached if the original number to print was non-zero, in which case
     we are printing the other digits on the right. */
  if (n == 0)
    return;

  /* Recursively print every digit but the last one, which is to say the number
     divided by ten, rounded down.  We are going to print the least significant
     digit right after this call, so that it correctly ends up on the right. */
  jitterlisp_print_long_long_recursive (cp, cps, n / radix, radix);

  /* Print the least significant digit. */
  int digit = n % radix;
  char character = (digit < 10) ? '0' + digit : 'a' + digit - 10;
  cp (cps, character);
}

/* Print the given jitter_long_long signed integer using the given
   char-printer. */
static void
jitterlisp_print_long_long (jitterlisp_char_printer_function char_printer,
                            void *char_printer_state,
                            jitter_long_long signed_n,
                            bool signed_,
                            unsigned radix)
{
  /* Special case for the NULL pointer: follow the GNU convention rather
     than printing "0x0" . */
  if (radix == 16 && ! signed_
      && signed_n == (jitter_long_long) (jitter_int) NULL)
    {
      jitterlisp_print_string (char_printer, char_printer_state, "(nil)");
      return;
    }

  /* We will deal with the sign at the very beginning, then forget about it and
     just work on the number to print as an unsigned quantity. */
  jitter_ulong_long n;

  /* Print a minus sign, if negative; in either case set n to be the absolute
     value of signed_n, so that we can forget about the sign in what follows.
     This works even for the most negative number, since n is unsigned and
     therefore has one more magnitude bit available than signed_n.

     A little language lawyering to justify this solution:
     - I'm converting a signed value to an unsigned type of the same rank,
       which has well-defined behavior (differently from the converse);
     - I'n not negating a signed quantity, which would be undefined
       behavior. */
  n = signed_n;
  if (signed_ && signed_n < 0)
    {
      char_printer (char_printer_state, '-');
      n = - n;
    }

  /* Print a radix prefix, unless the prefix is the default. */
  switch (radix)
    {
    case 2:
      jitterlisp_print_string (char_printer, char_printer_state, "0b"); break;
    case 8:
      jitterlisp_print_string (char_printer, char_printer_state, "0o"); break;
    case 10:
      break;
    case 16:
      jitterlisp_print_string (char_printer, char_printer_state, "0x"); break;
    default:
      jitter_fatal ("unsupported radix %u", radix);
    }

  /* If the number is zero print a zero digit, and we're done. */
  if (n == 0)
    {
      char_printer (char_printer_state, '0');
      return;
    }

  /* The number we have to print if we arrived at this point is strictly
     positive.  Use the recursive helper. */
  jitterlisp_print_long_long_recursive (char_printer, char_printer_state,
                                        n, radix);
}

/* Print the given pointer, as a hexadecimal address, using the given
   char-printer. */
static void
jitterlisp_print_pointer (jitterlisp_char_printer_function char_printer,
                          void *char_printer_state,
                          void *p)
{
  jitterlisp_print_long_long (char_printer, char_printer_state,
                              (jitter_uint) p, false,
                              16);
}

/* Print the given character using the given char-printer.  This is defined
   simply to have a function with similar arguments to
   jitterlisp_print_string. */
static void
jitterlisp_print_char (jitterlisp_char_printer_function char_printer,
                       void *char_printer_state,
                       char c)
{
  char_printer (char_printer_state, c);
}

/* Use the given char-printer to emit a printed representation of the given
   character, be it ordinary or non-ordinary. */
static void
jitterlisp_print_character_name (jitterlisp_char_printer_function char_printer,
                                 void *char_printer_state,
                                 jitter_int c)
{
  /* Print the #\ prefix, which is the same for ordinary and non-ordinary
     characters. */
  jitterlisp_print_string (char_printer, char_printer_state, "#\\");

  /* Look for the first name binding for c as a non-ordinary character.  If one
     exists, print it and return. */
  int i;
  for (i = 0; i < jitterlisp_non_ordinary_character_name_binding_no; i ++)
    if (jitterlisp_non_ordinary_character_name_bindings [i].character == c)
      {
        jitterlisp_print_string
           (char_printer,
            char_printer_state,
            jitterlisp_non_ordinary_character_name_bindings [i].name);
        return;
      }

  /* Since we haven't found a binding c must be an ordinary character.  Print it
     as it is. */
  jitterlisp_print_char (char_printer, char_printer_state, c);
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

/* Print the pointed s-expressions in order starting from the given initial
   pointer and going on for element_no elements.  Use a single space as a
   separator before each element, including the first. */
static void
jitterlisp_print_subs (jitterlisp_char_printer_function cp, void *cps,
                       jitterlisp_object *elements, size_t element_no)
{
  int i;
  for (i = 0; i < element_no; i ++)
    {
      jitterlisp_print_decoration (cp, cps, ASTATTR);
      jitterlisp_print_char (cp, cps, ' ');
      jitterlisp_print_decoration (cp, cps, NOATTR);
      jitterlisp_print (cp, cps, elements [i]);
    }
}

static void
jitterlisp_print_ast (jitterlisp_char_printer_function cp, void *cps,
                      struct jitterlisp_ast *ast)
{
  jitterlisp_print_decoration (cp, cps, ASTATTR);
  jitterlisp_print_string (cp, cps, "[");
  switch (ast->case_)
    {
    case jitterlisp_ast_case_literal:
      jitterlisp_print_string (cp, cps, "literal");
      break;
    case jitterlisp_ast_case_variable:
      jitterlisp_print_string (cp, cps, "variable");
      break;
    case jitterlisp_ast_case_define:
      jitterlisp_print_string (cp, cps, "define");
      break;
    case jitterlisp_ast_case_if:
      jitterlisp_print_string (cp, cps, "if");
      break;
    case jitterlisp_ast_case_setb:
      jitterlisp_print_string (cp, cps, "set!");
      break;
    case jitterlisp_ast_case_while:
      jitterlisp_print_string (cp, cps, "while");
      break;
    case jitterlisp_ast_case_primitive:
      jitterlisp_print_string (cp, cps, "primitive");
      break;
    case jitterlisp_ast_case_call:
      jitterlisp_print_string (cp, cps, "call");
      break;
    case jitterlisp_ast_case_lambda:
      jitterlisp_print_string (cp, cps, "lambda");
      break;
    case jitterlisp_ast_case_let:
      jitterlisp_print_string (cp, cps, "let");
      break;
    case jitterlisp_ast_case_sequence:
      jitterlisp_print_string (cp, cps, "sequence");
      break;
    case jitterlisp_ast_case_current_environment:
      jitterlisp_print_string (cp, cps, "current-environment");
      break;
    default:
      jitterlisp_print_string (cp, cps, "invalid]");
      jitterlisp_print_decoration (cp, cps, NOATTR);
      return;
    }
  jitterlisp_print_decoration (cp, cps, NOATTR);
  jitterlisp_print_subs (cp, cps, ast->subs, ast->sub_no);
  jitterlisp_print_decoration (cp, cps, ASTATTR);
  jitterlisp_print_string (cp, cps, "]");
  jitterlisp_print_decoration (cp, cps, NOATTR);
}

void
jitterlisp_print (jitterlisp_char_printer_function cp, void *cps,
                  jitterlisp_object o)
{
  if (JITTERLISP_IS_FIXNUM(o))
    {
      jitter_int decoded = JITTERLISP_FIXNUM_DECODE(o);
      jitterlisp_print_decoration (cp, cps, FIXNUMATTR);
      jitterlisp_print_long_long (cp, cps, decoded, true, 10);
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
          jitterlisp_print_long_long (cp, cps, index, true, 10);
          jitterlisp_print_char (cp, cps, '>');
        }
      jitterlisp_print_decoration (cp, cps, NOATTR);
    }
  else if (JITTERLISP_IS_CHARACTER(o))
    {
      jitterlisp_print_decoration (cp, cps, CHARACTERATTR);
      jitter_int c = JITTERLISP_CHARACTER_DECODE(o);
      jitterlisp_print_character_name (cp, cps, c);
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
          jitterlisp_print_string (cp, cps, "#<uninterned:");
          jitterlisp_print_pointer (cp, cps, s);
          jitterlisp_print_string (cp, cps, ">");
        }
      jitterlisp_print_decoration (cp, cps, NOATTR);
    }
  else if (JITTERLISP_IS_CLOSURE(o))
    {
      struct jitterlisp_closure * const closure = JITTERLISP_CLOSURE_DECODE(o);
      jitterlisp_print_decoration (cp, cps, CLOSUREATTR);
      jitterlisp_print_string (cp, cps, "#<closure ");
      jitterlisp_print_decoration (cp, cps, NOATTR);
      jitterlisp_print (cp, cps, closure->environment);
      jitterlisp_print_decoration (cp, cps, CLOSUREATTR);
      jitterlisp_print_char (cp, cps, ' ');
      jitterlisp_print_decoration (cp, cps, NOATTR);
      jitterlisp_print (cp, cps, closure->formals);
      jitterlisp_print_decoration (cp, cps, CLOSUREATTR);
      jitterlisp_print_decoration (cp, cps, NOATTR);
      jitterlisp_print_char (cp, cps, ' ');
      jitterlisp_print (cp, cps, closure->body);
      jitterlisp_print_decoration (cp, cps, CLOSUREATTR);
      jitterlisp_print_string (cp, cps, ">");
      jitterlisp_print_decoration (cp, cps, NOATTR);
    }
  else if (JITTERLISP_IS_NON_PRIMITIVE_MACRO(o))
    {
      struct jitterlisp_closure * const closure
        = JITTERLISP_NON_PRIMITIVE_MACRO_DECODE(o);
      jitterlisp_print_decoration (cp, cps, NONPRIMITIVEMACROATTR);
      jitterlisp_print_string (cp, cps, "#<macro ");
      jitterlisp_print_decoration (cp, cps, NOATTR);
      jitterlisp_print (cp, cps, closure->environment);
      jitterlisp_print_decoration (cp, cps, NONPRIMITIVEMACROATTR);
      jitterlisp_print_char (cp, cps, ' ');
      jitterlisp_print_decoration (cp, cps, NOATTR);
      jitterlisp_print (cp, cps, closure->formals);
      jitterlisp_print_decoration (cp, cps, NONPRIMITIVEMACROATTR);
      jitterlisp_print_decoration (cp, cps, NOATTR);
      jitterlisp_print_char (cp, cps, ' ');
      jitterlisp_print (cp, cps, closure->body);
      jitterlisp_print_decoration (cp, cps, NONPRIMITIVEMACROATTR);
      jitterlisp_print_string (cp, cps, ">");
      jitterlisp_print_decoration (cp, cps, NOATTR);
    }
  else if (JITTERLISP_IS_PRIMITIVE(o))
    {
      struct jitterlisp_primitive * const primitive
        = JITTERLISP_PRIMITIVE_DECODE(o);
      jitterlisp_print_decoration (cp, cps, PRIMITIVEATTR);
      jitterlisp_print_string (cp, cps, "#<");
      jitterlisp_print_long_long (cp, cps, primitive->in_arity, false, 10);
      jitterlisp_print_string (cp, cps, "-ary primitive ");
      jitterlisp_print_string (cp, cps, primitive->name);
      jitterlisp_print_string (cp, cps, ">");
      jitterlisp_print_decoration (cp, cps, NOATTR);
    }
  else if (JITTERLISP_IS_PRIMITIVE_MACRO(o))
    {
      struct jitterlisp_primitive * const primitive
        = JITTERLISP_PRIMITIVE_MACRO_DECODE(o);
      jitterlisp_print_decoration (cp, cps, PRIMITIVEMACROATTR);
      jitterlisp_print_string (cp, cps, "#<primitive macro ");
      jitterlisp_print_string (cp, cps, primitive->name);
      jitterlisp_print_string (cp, cps, ">");
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
  else if (JITTERLISP_IS_AST(o))
    {
      struct jitterlisp_ast * const ast = JITTERLISP_AST_DECODE(o);
      jitterlisp_print_ast (cp, cps, ast);
    }
  else if (JITTERLISP_IS_VECTOR(o))
    {
      const struct jitterlisp_vector * const v = JITTERLISP_VECTOR_DECODE(o);
      jitterlisp_print_decoration (cp, cps, VECTORATTR);
      jitterlisp_print_string (cp, cps, "#(");
      jitterlisp_print_decoration (cp, cps, NOATTR);
      int i;
      int element_no = JITTERLISP_FIXNUM_DECODE(v->element_no);
      for (i = 0; i < element_no; i ++)
        {
          jitterlisp_print (cp, cps, v->elements [i]);
          if (i < (element_no - 1))
            jitterlisp_print_char (cp, cps, ' ');
        }
      jitterlisp_print_decoration (cp, cps, VECTORATTR);
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
