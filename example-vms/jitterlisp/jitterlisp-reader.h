/* Jittery Lisp: reader header.

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


#ifndef JITTERLISP_READER_H_
#define JITTERLISP_READER_H_

#include <stdio.h>

#include "jitterlisp-sexpression.h"




/* Char-reading definitions.
 * ************************************************************************** */

/* This API makes it easy to use the same functions for reading from a stream,
   from a string in memory or from some other source defined by the user. */

/* A char-reading function, at every call, returns either a valid char from the
   input or EOF.  The exact nature of the state depends on the reader function,
   but the function will update it after reading each character. */
typedef int (*jitterlisp_char_reader_function) (void *reader_state);




/* Predefined char-readers.
 * ************************************************************************** */

/* A char-reader function reading from a '\0'-terminated string.  The argument,
   declared as void * for compatibility with jitterlisp_char_reader_function ,
   is actually of type const char ** .  The function increments the pointed
   const char * pointer after reading each character, unless it has reached the
   terminator already.
   The user can destroy the pointed string after reading is over. */
int
jitterlisp_string_char_reader_function (void *const_char_star_star)
  __attribute__ ((nonnull (1)));

/* A char-reader function reading from a FILE * input stream.  The argument, declared
   as void * for compatibility with jitterlisp_char_reader_function , is
   actually of type FILE * , and must be open for reading at at the correct
   position.
   After reading is over the caller may close the stream. */
int
jitterlisp_stream_char_reader_function (void *file_star)
  __attribute__ ((nonnull (1)));




/* S-expression reader.
 * ************************************************************************** */

/* Return the next s-expression read using the given char-reader in the given
   char-reader-state, or #<eof> if there is nothing more to read. */
jitterlisp_object
jitterlisp_read_from_char_reader (jitterlisp_char_reader_function char_reader,
                                  void *char_reader_state)
  __attribute__ ((nonnull (1))); /* I omit nonnull (2) just for generality. */




/* S-expression reader: convenience functions hiding char readers.
 * ************************************************************************** */

/* Return the first s-expression read from the given string.  This is a simple
   wrapper around jitterlisp_read_from_char_reader , hiding the char reader
   and its state. */
jitterlisp_object
jitterlisp_read_from_string (const char *string)
  __attribute__ ((nonnull (1)));

/* Return the first s-expression read from the given input stream.  This is a
   simple wrapper around jitterlisp_read_from_char_reader , hiding the char
   reader and its state. */
jitterlisp_object
jitterlisp_read_from_stream (FILE *f)
  __attribute__ ((nonnull (1)));

#endif // #ifndef JITTERLISP_READER_H_
