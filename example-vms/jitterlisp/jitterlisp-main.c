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


#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <jitter/jitter.h>
#include <jitter/jitter-cpp.h>
#include <jitter/jitter-fatal.h>

#ifdef JITTER_HAS_ASSEMBLY
  /* We need architecture-specific register names. */
# include <jitter/machine/jitter-machine.h>
#endif // #ifdef JITTER_HAS_ASSEMBLY


#include "jitterlisp-sexpression.h"


/* Global register variable hack.
 * ************************************************************************** */

/* Expand to a global variable declaration -- a global *register* variable
   if we have machine support.  This doesn't currently run in a Jittery VM,
   so it doesn't matter if we choose registers which are already reserved
   for other purposes. */
#ifdef JITTER_HAS_ASSEMBLY
# define JITTERLISP_GLOBAL_REGISTER_VARIABLE_(_jitterlisp_type,            \
                                              _jitterlisp_name,            \
                                              _jitterlisp_register_index)  \
  register _jitterlisp_type _jitterlisp_name                               \
     asm (JITTER_STRINGIFY(                                                \
            JITTER_CONCATENATE_TWO(JITTER_RESIDUAL_REGISTER_,              \
                                   _jitterlisp_register_index)))
#else
# define JITTERLISP_GLOBAL_REGISTER_VARIABLE_(_jitterlisp_type,            \
                                              _jitterlisp_name,            \
                                              _jitterlisp_register_index)  \
  _jitterlisp_type _jitterlisp_name
#endif // #ifdef JITTER_HAS_ASSEMBLY




/* Allocation (very tentative).
 * ************************************************************************** */

#define JITTERLISP_LITTER_BYTE_NO (10 * 1024 * 1024)

JITTERLISP_GLOBAL_REGISTER_VARIABLE_(char *, litter_allocator_pointer, 0);
JITTERLISP_GLOBAL_REGISTER_VARIABLE_(char *, litter_allocator_limit, 1);

__attribute__ ((unused))
inline static char*
jitterlisp_allocate_litter (size_t size)
{
  char *res = litter_allocator_pointer;
  litter_allocator_pointer += sizeof (struct jitterlisp_cons);
  if (__builtin_expect (litter_allocator_pointer > litter_allocator_limit,
                        false))
    jitter_fatal ("At this point I would call the GC, if there were one.\n");
  return res;
}

__attribute__ ((unused))
inline static char*
jitterlisp_allocate_malloc (size_t size)
{
  char *res = malloc (size);
  assert (res != NULL);
  return res;
}

#if 0
# define JITTER_ALLOCATE(_jitter_byte_no) jitterlisp_allocate_malloc(_jitter_byte_no)
#else
# define JITTER_ALLOCATE(_jitter_byte_no) jitterlisp_allocate_litter(_jitter_byte_no)
#endif

struct jitterlisp_cons*
jitterlisp_make_unencoded_cons (jitterlisp_object a, jitterlisp_object b)
{
  struct jitterlisp_cons *res
    = ((struct jitterlisp_cons *)
       JITTER_ALLOCATE(sizeof (struct jitterlisp_cons)));
  res->car = a;
  res->cdr = b;
  return res;
}

struct jitterlisp_symbol*
jitterlisp_make_unencoded_symbol (const char *name_or_NULL)
{
  struct jitterlisp_symbol *res
    = ((struct jitterlisp_symbol *)
       JITTER_ALLOCATE(sizeof (struct jitterlisp_symbol)));
  if (name_or_NULL == NULL)
    res->name_or_NULL = NULL;
  else
    {
      // FIXME: intern for real.
      res->name_or_NULL = malloc (strlen (name_or_NULL));
      assert (res->name_or_NULL != NULL);
      strcpy (res->name_or_NULL, name_or_NULL);
    }
  return res;
}


/* Binary printing.
 * ************************************************************************** */

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
   digit_no binary digits (left-padding with zeroes). */
void
jitter_print_binary_padded (FILE *stream, jitter_uint u, int digit_no)
{
  fputs ("0b", stream);
  jitter_print_binary_recursive (stream, u, digit_no);
}

// FIXME: I might want to keep this around.
/* Print the given number in binary to the pointed stream. */
void
jitter_print_binary (FILE *stream, jitter_uint u)
{
  /* Print a binary number with at least one digit (which may be zero). */
  jitter_print_binary_padded (stream, u, 1);
}



/* Scratch.
 * ************************************************************************** */

void
print (jitterlisp_object o)
{
#define WIDTH "020"
  printf ("* 0x%" WIDTH JITTER_PRIx "  (tag ", o);
  jitter_print_binary_padded (stdout, JITTERLISP_GET_TAG(o),
                              JITTERLISP_TAG_BIT_NO);
  printf (") ");
  if (JITTERLISP_IS_FIXNUM(o))
    printf ("is the fixnum %" JITTER_PRIi, JITTERLISP_FIXNUM_DECODE(o));
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
  printf ("  "); jitterlisp_print (stdout, o); printf ("\n");
#undef WIDTH
}

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
  return JITTERLISP_CONS(a, b);
}

jitterlisp_object
jitterlisp_symbol (char *name)
{
  return JITTERLISP_SYMBOL_ENCODE(jitterlisp_make_unencoded_symbol (name));
}

jitterlisp_object
jitterlisp_uninterned_symbol (void)
{
  return JITTERLISP_SYMBOL_ENCODE(jitterlisp_make_unencoded_symbol (NULL));
}

jitterlisp_object
jitterlisp_car (jitterlisp_object a)
{
  return JITTERLISP_CONS_DECODE(a)->car;
}

jitterlisp_object
jitterlisp_cdr (jitterlisp_object a)
{
  return JITTERLISP_CONS_DECODE(a)->cdr;
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




/* Main function.
 * ************************************************************************** */

int
main (void)
{
  /* Initialize the litter-allocator.  I must not delete this while I'm
     testing. */
  litter_allocator_pointer = malloc (JITTERLISP_LITTER_BYTE_NO);
  litter_allocator_limit = litter_allocator_pointer + JITTERLISP_LITTER_BYTE_NO;
  assert (litter_allocator_pointer != NULL);

  print (JITTERLISP_CHARACTER_ENCODE('a'));
  print (JITTERLISP_CHARACTER_ENCODE(' '));
  print (JITTERLISP_TRUE);
  print (JITTERLISP_EMPTY_LIST);
  print (JITTERLISP_EOF);

  print (JITTERLISP_FIXNUM_ENCODE(0));
  print (JITTERLISP_FIXNUM_ENCODE(1));
  print (JITTERLISP_FIXNUM_ENCODE(14));
  print (JITTERLISP_FIXNUM_ENCODE(-1));
  print (JITTERLISP_FIXNUM_ENCODE(-2));
  print (JITTERLISP_NOTHING);
  printf ("\n");
  jitterlisp_object fa = JITTERLISP_FIXNUM_ENCODE(10);
  jitterlisp_object fb = JITTERLISP_FIXNUM_ENCODE(14);
  print (JITTERLISP_FIXNUM_PLUS(fa, fb));

  //print (jitterlisp_cons(fa, fb));
  print (jitterlisp_cons(fa, JITTERLISP_EMPTY_LIST));
  print (jitterlisp_cons(JITTERLISP_EMPTY_LIST, fa));
  long list_length;
  if (1)
    list_length = 10;
  else
    list_length = JITTERLISP_LITTER_BYTE_NO / sizeof (struct jitterlisp_cons);
  print (jitterlisp_iota(JITTERLISP_FIXNUM_ENCODE(list_length)));
  print (jitterlisp_symbol ("foo"));
  print (jitterlisp_uninterned_symbol ());
  print (jitterlisp_cons(jitterlisp_symbol ("bar"),
                         JITTERLISP_EMPTY_LIST));
  return 0;
}
