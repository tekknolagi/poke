/* Jittery Lisp: printer header.

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


#ifndef JITTERLISP_PRINTER_H_
#define JITTERLISP_PRINTER_H_

#include <stdio.h>

#include "jitterlisp-sexpression.h"


/* Char-printing definitions.
 * ************************************************************************** */

/* This API makes it easy to use the same functions for printing to a stream, to
   a dynamically-allocated string in memory or to some other sink defined by the
   user.

   This has nothing to do with character names, below. */

/* A char-printing function, at every call, prints the given character to the
   sink referred in the pointed state, updating it if needed.  The exact nature
   of the state depends on the printer function. */
typedef void (*jitterlisp_char_printer_function) (void *printer_state, char c);




/* Predefined char-printers.
 * ************************************************************************** */

/* A char-printing function adding to a dynamic buffer.  The first argument,
   declared as void * for compatibility with jitterlisp_char_printer_function ,
   is actually of type struct jitter_dynamic_buffer * , and the pointed
   structure must be already initialized when this function is called.  The
   function pushes a character to the dynamic buffer, which is automatically
   enlarged as needed.

   After printing is over the user can finalize the dynamic buffer, or extract
   its data. */
void
jitterlisp_dynamic_buffer_char_printer_function (void *dynamic_buffer, char c)
  __attribute__ ((nonnull (1)));

/* A char-printing function writing to a FILE * output stream.  The first argument,
   declared as void * for compatibility with jitterlisp_char_printer_function ,
   is actually of type FILE * , and must be open for writing at at the correct
   position.

   After printing is over the caller may close the stream. */
void
jitterlisp_stream_char_printer_function (void *file_star, char c)
  __attribute__ ((nonnull (1)));




/* S-expression printer.
 * ************************************************************************** */

/* Print the given s-expression using the given printer state, without
   terminating with '\n'. */
void
jitterlisp_print (jitterlisp_char_printer_function char_printer,
                  void *char_printer_state,
                  jitterlisp_object o)
  __attribute__ ((nonnull (1)));




/* S-expression printer: convenience API hiding char-printers.
 * ************************************************************************** */

/* Print the given JitterLisp object to the given stream. */
void
jitterlisp_print_to_stream (FILE *f, jitterlisp_object o)
  __attribute__ ((nonnull (1)));


/* Print the given JitterLisp object into a fresh malloc-allocated
   '\0'-terminated string, and return a pointer to it.  The user is responsible
   for calling free to release memory for the string. */
char *
jitterlisp_print_to_string (jitterlisp_object o)
  __attribute__ ((returns_nonnull));




/* Character names.
 * ************************************************************************** */

/* Characters are either ordinary and simply written with a #\ prefix, like for
   example #\a , or with longer names for other, not so easy to print
   characters such as #\newline .  In the case of longer non-ordinary characters
   we need to keep a mapping between actual characters such as '\n' and their
   name such as "newline".

   These definitions, used in the printer and in the reader as well, have
   nothing to do with char-printing, above. */

/* The name for a non-ordinary character. */
struct jitterlisp_character_name_binding
{
  /* The actual character, for example '\n'. */
  jitter_int character;

  /* The character name without the #\ prefix, for example "newline".  The
     string length is always strictly greater than one. */
  char *name;
};

/* The name bindings for every non-ordinary character, in no particular order.
   It is allowed for one character to have multiple names, but not the
   converse. */
extern const struct jitterlisp_character_name_binding
jitterlisp_non_ordinary_character_name_bindings [];

/* How many non-ordinary characters there are.  This is the size of
   jitterlisp_non_ordinary_character_name_bindings in elements. */
extern const size_t
jitterlisp_non_ordinary_character_name_binding_no;

#endif // #ifndef JITTERLISP_PRINTER_H_
