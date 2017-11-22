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

/* This type is purely conventional: the actual char-reader state type depends
   on the char-reader function, which receives a pointer to a char-reader
   state which it will cast to the appropriate pointer type. */
typedef void * jitterlisp_char_reader_state;

/* A char-reading function, at every call, returns either a valid char from the
   input or EOF.  The exact nature of the state depends on the reader function,
   but the function will update the pointed state after reading each
   character. */
typedef int (*jitterlisp_char_reader_function)
   (jitterlisp_char_reader_state *reader_state_pointer);




/* Predefined char-readers.
 * ************************************************************************** */

/* A char-reader function reading from a '\0'-terminated string.  The argument,
   declared as jitterlisp_char_reader_state * for compatibility with
   jitterlisp_char_reader_function , is actually of type const char ** .  The
   function increments the pointed const char * pointer after reading each
   character, unless it has reached the terminator already.
   The user can destroy the pointed string after reading is over; in that case,
   however, it's her responsibility to keep a pointer to the original beginning
   of the string: at every character read the pointer-to-pointer-to-char which
   is passed as the char-reader state will be dereferenced, and the pointed
   pointer-to-char advanced. */
int
jitterlisp_string_char_reader_function
   (jitterlisp_char_reader_state *file_star_star)
  __attribute__ ((nonnull (1)));

/* A char-reader function reading from a FILE * input stream.  The argument,
   declared as jitterlisp_char_reader_state * for compatibility with
   jitterlisp_char_reader_function , is actually of type FILE ** (two levels of
   pointers), and the stream must be open for reading at at the correct
   position.  After reading is over the caller may close the stream. */
int
jitterlisp_stream_char_reader_function
   (jitterlisp_char_reader_state *file_star_star)
  __attribute__ ((nonnull (1)));




/* Reader state.
 * ************************************************************************** */

/* In order to read s-expressions from a stream, a string or something else
   the user needs to make a "reader state" structure, containing some data
   to be kept between one s-expression and the next. */

/* The reader state is an abstract type, whose definition is subject to change.
   The user should only use the functions below to make and destroy instances of
   this struct. */
struct jitterlisp_reader_state;

/* Return a pointer to a new reader state made from the given char-reader
   function and the given char-reader state.

   Notice that the char-reader state is not pointed, differently from the
   argument in jitterlisp_char_reader_function : the char-reader state here is
   *copied*, and then a pointer to it is passed to
   jitterlisp_char_reader_function .

   This function needs to read the first token, and therefore is potentially
   blocking. */
struct jitterlisp_reader_state*
jitterlisp_make_reader_state
   (jitterlisp_char_reader_function char_reader,
    jitterlisp_char_reader_state char_reader_state)
  __attribute__ ((nonnull (1, 2), returns_nonnull));

/* Destroy the given reader state, freeing up its resources.  Calling this does
   not explicitly close files or streams, however. */
void
jitterlisp_destroy_reader_state (struct jitterlisp_reader_state *rs)
  __attribute__ ((nonnull (1)));




/* Reader state convenience functions.
 * ************************************************************************** */

/* The functions below, all simple wrappers around jitterlisp_make_reader_state
   , have the advantage of hiding char readers from the user.  The user is still
   responsible for releasing any external resource for the input such as file
   descriptors, streams or string memory. */

/* Return a pointer to a fresh reader state reading from the pointed stream,
   which must be open for reading.
   This function needs to read the first token, and therefore is potentially
   blocking. */
struct jitterlisp_reader_state*
jitterlisp_make_stream_reader_state (FILE *input)
  __attribute__ ((nonnull (1), returns_nonnull));

/* Return a pointer to a fresh reader state reading from the pointed
   '\0'-terminated string.
   This function needs to read the first token, and therefore is potentially
   blocking. */
struct jitterlisp_reader_state*
jitterlisp_make_string_reader_state (const char *string)
  __attribute__ ((nonnull (1), returns_nonnull));




/* S-expression reader.
 * ************************************************************************** */

/* Return the next s-expression read from the given reader state, or #<eof> if
   there is nothing more to read. */
jitterlisp_object
jitterlisp_read (struct jitterlisp_reader_state *rs)
  __attribute__ ((nonnull (1))); /* I omit nonnull (2) just for generality. */


#endif // #ifndef JITTERLISP_READER_H_
