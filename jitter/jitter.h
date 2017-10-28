/* Jitter: VM-independent library.

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


#ifndef JITTER_H_
#define JITTER_H_

#include <stdlib.h> // for size_t .
#include <limits.h> // for CHAR_BIT .
#include <stdint.h>
#include <inttypes.h> // format strings for standard integer types.


/* Include the host-dependent header , and make sure that it actually contains
   some definitions. */
#include "jitter/jitter-config.h"
#ifndef SIZEOF_VOID_P
#  error "jitter/jitter-config.h is probably incorrect"
#endif // #ifndef SIZEOF_VOID_P




/* Fundamental type definitions.
 * ************************************************************************** */

/* Configurations where a char is not 8 bits have never been tested. */
#if CHAR_BIT != 8
# warning "The char type of a size different from 8 bits: untested."
#endif // #if CHAR_BIT != 8

/* Define jitter_int and jitter_uint as word-sized integer type, respectively
   signed and unsigned; also define the format strings, JITTER_PRIi ,
   JITTER_PRIu , JITTER_PRIo and JITTER_PRIx to be used with printf and scanf
   for those types, in the style of PRIi64 , PRIu64 , PRIo64 and PRIx64 . */
#if   (SIZEOF_VOID_P * CHAR_BIT == 16)
  /* This will not happen on GNU, but one check costs almost nothing. */
  typedef int16_t  jitter_int;
  typedef uint16_t jitter_uint;
# define JITTER_PRIi PRIi16
# define JITTER_PRIu PRIu16
# define JITTER_PRIo PRIo16
# define JITTER_PRIx PRIx16
#elif (SIZEOF_VOID_P * CHAR_BIT == 32)
  typedef int32_t  jitter_int;
  typedef uint32_t jitter_uint;
# define JITTER_PRIi PRIi32
# define JITTER_PRIu PRIu32
# define JITTER_PRIo PRIo32
# define JITTER_PRIx PRIx32
#elif (SIZEOF_VOID_P * CHAR_BIT == 64)
  typedef int64_t  jitter_int;
  typedef uint64_t jitter_uint;
# define JITTER_PRIi PRIi64
# define JITTER_PRIu PRIu64
# define JITTER_PRIo PRIo64
# define JITTER_PRIx PRIx64
#else
# error "can't find a word-sized integer type."
#endif // #if   (SIZEOF_VOID_P == ...)

/* Define two more format strings for convenience, JITTER_INT_FORMAT and
   JITTER_UINT_FORMAT; they can be used like "%li" and "%lu" . */
#define JITTER_INT_FORMAT  "%" JITTER_PRIi
#define JITTER_UINT_FORMAT "%" JITTER_PRIu

/* Define a word-sized floating-point type. */
#if   (SIZEOF_VOID_P == SIZEOF_FLOAT)
  typedef float jitter_float;
#elif (SIZEOF_VOID_P == SIZEOF_DOUBLE)
  typedef double jitter_float;
#elif (SIZEOF_VOID_P == SIZEOF_LONG_DOUBLE)
/* This should not happen anywhere, I guess -- on PowerPC long double is 64-bit
   but so is double which is checked before.  Anyway, it costs nothing. */
  typedef long double jitter_float;
#else
# error "can't find a word-sized floating-point type"
#endif // #if   (SIZEOF_VOID_P == ...)


/* A thread, which is to say a label-as-value using the GNU C extension.  From
   the interpreter point of view the memory is constant, so restrict here is
   correct and might possibly enable some optimization. */
typedef const void * restrict jitter_thread;

/* A register index as occurring in an instruction.  A pair <class, index>
   uniquely identifies a register. */
typedef jitter_int jitter_register_index;

/* The index of an unspecialized instruction, 0-based. */
typedef jitter_int jitter_label_as_index;

/* Define a word-sized union holding an integer (signed or unsigned) or a
   pointer value.  This fits in a general register, by design; in order to allow
   this we prefer not to include a floating-point case as well in the same
   union.
   The integer fields are word-sized, and therefore they can also hold any enum
   value without loss of information, given the appropriate signedness; in this
   project enum values are always non-negative.

   FIXME: is this design a problem on m68k, which has different registers for
   integers and addresses?  Not very high-priority, but I'm curious. */
union jitter_word
{
  /* A signed word-sized integer. */
  jitter_int fixnum;

  /* An unsigned word-sized integer. */
  jitter_uint ufixnum;

  /* A word pointer.  The restrict qualifier may enable some optimization and is
     correct here: when memory is changed thru the pointer the program being
     interpreted will explicitly reload when needed; it's useless to let GCC be
     pessimistic here and assume that everything may be changed by a store thru
     this. */
  union jitter_word * restrict pointer;

  /* A label is in practice a pointer, with the difference that the pointed
     memory is constant. */
  const void *label;

  /* FIXME: are these useful?  Pointers to different types, just as a
     convenience. */
  void * restrict pointer_to_void;
  char * restrict pointer_to_char;

  /* A thread. */
  jitter_thread thread;
};




/* The long long type or some approximation of it.
 * ************************************************************************** */

/* Jitter uses integer types as wide as pointers in most cases, but in a few
   circumstances, particularly for textual I/O, it is convenient to use the
   widest integer type available. */

/* Provide some replacement for long long if it's not available.  In either
   case define:
   - the type jitter_long_long , defined as either long long or long ;
   - the format string JITTER_PRIll (not including the "%" prefix, and just
     for signed types printed in radix 10), expanding to either "lli" or "li";
   - the macro jitter_strtoll , expanding to either strtoll or strtol . */
#ifdef HAVE_LONG_LONG_INT
  /* We have a real long long type.  Define the type and macros above as trivial
     wrappers. */
  typedef long long jitter_long_long;
# define JITTER_PRIll "lli"
# define jitter_strtoll strtoll
#else
  /* The type long long is not available in this configuration.  Use long in its
     place. */
  typedef long jitter_long_long;
# define JITTER_PRIll "li"
# define jitter_strtoll strtol
#endif // #ifdef HAVE_LONG_LONG_INT




/* Word size in bits and bytes and its binary logarithm.
 * ************************************************************************** */

/* Word size in bytes (actually in chars). */
#define JITTER_BYTES_PER_WORD \
  SIZEOF_VOID_P

/* Word size in bits. */
#define JITTER_BITS_PER_WORD \
  (JITTER_BYTES_PER_WORD * CHAR_BIT)

/* Define the binary logarithm of the word size in bits (a common parameter to
   use for shifting operations), and the name for a Gas pseudo-op generating a
   word-size object.  These definitions need a dispatch on the word size. */
#if   JITTER_BYTES_PER_WORD == 8
#  define JITTER_LG_BYTES_PER_WORD 3
#  define JITTER_ASM_WORD ".quad"
#elif JITTER_BYTES_PER_WORD == 4
#  define JITTER_LG_BYTES_PER_WORD 2
#  define JITTER_ASM_WORD ".long"
#elif JITTER_BYTES_PER_WORD == 2
#  define JITTER_LG_BYTES_PER_WORD 1
#  define JITTER_ASM_WORD ".word"
#  warning "Weird: running Jitter on a 16-bit machine; this is untested."
#else
#  error "Weird: this machine's word size is not 8, 4 or 2 bytes"
#endif // #if JITTER_BYTES_PER_WORD == ...


#endif // #ifndef JITTER_CONFIG_H_
