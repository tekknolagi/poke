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

#include "jitterlisp.h"




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

static void
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




/* Main function.
 * ************************************************************************** */

int
main (void)
{
  jitterlisp_initialize ();

  /* Initialize the litter-allocator.  I must not delete this while I'm
     testing. */

  // int useless; jitterlisp_error_handler_register = & useless;

  jitterlisp_object c = JITTERLISP_CHARACTER_ENCODE('a');
  print (c);

  //print (JITTERLISP_CHARACTER_ENCODE('a'));
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
  //asm volatile ("nop\n\tnop\n\tnop");
  jitterlisp_object fa = JITTERLISP_FIXNUM_ENCODE(10);
  jitterlisp_object fb = JITTERLISP_FIXNUM_ENCODE(14);
  //asm volatile ("": "+r" (fb));
  //asm volatile ("addq $2, %rax\n\taddq $-2, %rax\n\t");
  print (JITTERLISP_FIXNUM_PLUS(fa, fb));
  //asm volatile ("addq $1, %rax\n\taddq $-1, %rax\n\t");

  //print (jitterlisp_cons(fa, fb));
  print (jitterlisp_cons(fa, JITTERLISP_EMPTY_LIST));
  print (jitterlisp_cons(JITTERLISP_EMPTY_LIST, fa));

  {
    int q;
#define L 1024
#define T (10)
    for (q = 0; q < T; q ++)
      jitterlisp_iota (JITTERLISP_FIXNUM_ENCODE(L));
  }

  long list_length = 10;//10000000;
  print (jitterlisp_iota (JITTERLISP_FIXNUM_ENCODE(list_length)));
  print (jitterlisp_symbol ("foo"));
  print (jitterlisp_uninterned_symbol ());
  print (jitterlisp_cons(jitterlisp_symbol ("bar"),
                         JITTERLISP_EMPTY_LIST));
  print (jitterlisp_factorial(JITTERLISP_FIXNUM_ENCODE(10)));
  printf ("\n\n");

  const char *s __attribute__ ((unused));
  const char *s_ __attribute__ ((unused));
  s
    = ("(define (fact n);; Foo!\n"
       "\n" // An empty line, just to test.
       "\r" // And even a '\r' character.
       "  (if (= n 0) ;; here's another comment\n"
       "    1\n"
       "    (* n (fact (1- n)))))");
  //char *s = "a b c d e f";
  //char *s = ";;;abc\n 42";
  s_ = "`(+ ,a 1)";
  print (jitterlisp_read_from_string (s));
  printf ("\n\n");
  //print (jitterlisp_read_from_stream (stdin));
  char *ds = jitterlisp_print_to_string (jitterlisp_read_from_string (s));
  printf ("s:  %s\n\n", s);
  printf ("ds: %s\n\n", ds);
  free (ds);

  jitterlisp_finalize ();
  return 0;
}
