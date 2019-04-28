/* Jitter: general-purpose bitwise macro header.

   Copyright (C) 2018, 2019 Luca Saiu
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


#ifndef JITTER_BITWISE_H_
#define JITTER_BITWISE_H_


#include <limits.h> /* For CHAR_BIT . */

#include <jitter/jitter.h>


/* Introduction.
 * ************************************************************************** */

/* This header contains some general-purpose macros for bitwise operations. */




/* Powers of two.
 * ************************************************************************** */

/* Expand to an expression evaluating to nonzero if the given argument, assumed
   to be nonzero, does *not* evaluate to a power of two.
   This may evaluate the argument multiple times, but is otherwise efficient and
   always branchless. */
#define JITTER_ISNT_NONZERO_A_POWER_OF_TWO(x)  \
  ((x) & ((x) - 1))

/* Expand to an expression evaluating to nonzero if the given argument, assumed
   to be nonzero, evaluates to a power of two.
   This may evaluate the argument multiple times. */
#define JITTER_IS_NONZERO_A_POWER_OF_TWO(x)  \
  (! JITTER_ISNT_NONZERO_A_POWER_OF_TWO(x))

/* Expand to an expression evaluating to nonzero if the given argument evaluates
   to a power of two.
   This may evaluate the argument multiple times. */
#define JITTER_IS_A_POWER_OF_TWO(x)  \
  (((x) != 0) && JITTER_IS_NONZERO_A_POWER_OF_TWO(x))

/* Expand to an expression evaluating to nonzero if the given argument does
   *not* evaluate to a power of two.
   This may evaluate the argument multiple times. */
#define JITTER_ISNT_A_POWER_OF_TWO(x)  \
  (((x) == 0) || JITTER_ISNT_NONZERO_A_POWER_OF_TWO(x))

/* Given a possibly integer x and a power of two p, expand to an expression
   evaluating to the maximum integer less than or equal to x which is a multiple
   of p.
   The signed version relies on two's complement representation.
   This assumes that p is a power of two and may evaluate both arguments
   multiple times.  However, if both arguments are constant expression, the
   expansion is also a constant expression. */
#define JITTER_PREVIOUS_MULTIPLE_OF_POWER_OF_TWO(x, p)  \
  ((x) & ~ ((p) - 1))

/* Given a possibly signed integer x and a power of two p, expand to an
   expression evaluating to the minimum integer greater than or equal to x which
   is a multiple of p.

   This assumes that p is a power of two and may evaluate both arguments
   multiple times.  However, if both arguments are constant expression, the
   expansion is also a constant expression.

   I learned this technique from from Hacker's Delight, §3-1.
   Let y be the smallest multiple of p greater than or equal to x, where p
   is a power of two.  Then:
     y = (x + p - 1) & - p     [1]
   and also
     y = x + (- x & (p - 1))   [2]
   I am currently using [1].  [2] can be useful in other contexts, because
   the second addend expresses a displacement from x to y. */
#define JITTER_NEXT_MULTIPLE_OF_POWER_OF_TWO(x, p)  \
  (((x) + ((p) - 1)) & ~ ((p) - 1))

/* Given a positive integer a and a positive integer b, expand to a constant
   expression evaluating to the smallest integer greater than or equal to a
   which is a multiple of b.
   This may evaluate both arguments multiple times.  However, if both arguments
   are constant expression, the expansion is also a constant expression. */
#define JITTER_NEXT_MULTIPLE_OF_POSITIVE(a, b)  \
  (((a) + (b) - 1) / (b) * (b))




/* Type sizes in bits.
 * ************************************************************************** */

/* Given a type expand to a constant expression evaluating to the type size in
   bits.  This is useful for playing with bit masks. */
#define JITTER_SIZEOF_IN_BITS(_jitter_type)  \
  (sizeof (_jitter_type) * CHAR_BIT)

/* Jitter word size in bits. */
#define JITTER_BITS_PER_WORD \
  JITTER_SIZEOF_IN_BITS (jitter_int)




/* Arithmetic right shifts.
 * ************************************************************************** */

/* Expand to an r-value evaluating to the given word arithmetically
   right-shifted by _jitter_bit_nom , using the given types which must be
   unsigned and signed integer types of the same width.

   There are basically two separate implementations of this, one relying on >>
   sign-extending on signed operands, like GCC does, and another generic but
   slow solution.  Which implementation is used depends on a constant expression
   checking how >> behaves at compile time.  I cannot think of any way of moving
   this logic to configure, as the check would depend on run-time behavior and
   would break under cross-compilation.

   Notice that the seemingly obvious alternative of doing a signed division by
   a power of two does not always compute the correct result with a negative
   operand in two's complement: the rounding direction for signed division is
   not what we need here. */
#define JITTER_ARITHMETIC_SHIFT_RIGHT(_jitter_unsigned_type,        \
                                      _jitter_signed_type,          \
                                      _jitter_word,                 \
                                      _jitter_bit_no)               \
  (JITTER_RIGHT_SHIFT_SIGN_EXTENDS (_jitter_unsigned_type,          \
                                    _jitter_signed_type)            \
   ? JITTER_ARITHMETIC_SHIFT_RIGHT_GCC (_jitter_unsigned_type,      \
                                        _jitter_signed_type,        \
                                        (_jitter_word),             \
                                        (_jitter_bit_no))           \
   : JITTER_ARITHMETIC_SHIFT_RIGHT_GENERIC (_jitter_unsigned_type,  \
                                            _jitter_signed_type,    \
                                            (_jitter_word),         \
                                            (_jitter_bit_no)))

/* Expand to a constant expression, nonzero iff >> sign-extends (at least on an
   argument of size jitter_int , which is what we care about here).
   This is used in the implementation of JITTER_ARITHMETIC_SHIFT_RIGHT . */
#define JITTER_RIGHT_SHIFT_SIGN_EXTENDS(_jitter_unsigned_type,                \
                                        _jitter_signed_type)                  \
  /* Just do on one simple test.  Some ridiculous C compiler might in theory  \
     behave in some different way than actually performing an arithmetic      \
     shift and still give the expected result here, but I do not feel like    \
     being overly pedantic. */                                                \
  ((((_jitter_signed_type) -56) >> 3)                                         \
   == ((_jitter_signed_type) -7))

/* One of the two implementations for JITTER_ARITHMETIC_SHIFT_RIGHT .  This
   definition is more efficient than the alternative but relies on >>
   sign-extending on signed operands like GCC does; the C standards doesn't
   define a behavior in this case. */
#define JITTER_ARITHMETIC_SHIFT_RIGHT_GCC(_jitter_unsigned_type,  \
                                          _jitter_signed_type,    \
                                          _jitter_word,           \
                                          _jitter_bit_no)         \
  ((_jitter_unsigned_type)                                        \
   (((_jitter_signed_type) (_jitter_word))                        \
    >> (_jitter_bit_no)))

/* One of the two implementations for JITTER_ARITHMETIC_SHIFT_RIGHT .  This
   definition does not rely on implementation-defined behavior. */
#define JITTER_ARITHMETIC_SHIFT_RIGHT_GENERIC(_jitter_unsigned_type,  \
                                              _jitter_signed_type,    \
                                              _jitter_word,           \
                                              _jitter_bit_no)         \
  /* The technique comes from from Hacker's Delight, §2. */           \
  ((_jitter_signed_type)                                              \
   ((((_jitter_unsigned_type) (_jitter_word)) >> (_jitter_bit_no))    \
    | - (((_jitter_unsigned_type)                                     \
          (((_jitter_unsigned_type) (_jitter_word))                   \
           >> (JITTER_SIZEOF_IN_BITS (_jitter_unsigned_type) - 1)))   \
         << (JITTER_SIZEOF_IN_BITS (_jitter_unsigned_type) - 1        \
             - (_jitter_bit_no)))))

#endif // #ifndef JITTER_BITWISE_H_
