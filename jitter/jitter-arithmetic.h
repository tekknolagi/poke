/* Jitter: general-purpose integer arithmetic macro header.

   Copyright (C) 2019 Luca Saiu
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


#ifndef JITTER_ARITHMETIC_H_
#define JITTER_ARITHMETIC_H_

#include <stdint.h>

#include <jitter/jitter.h>
#include <jitter/jitter-bitwise.h>


/* This header assumes two's complement arithmetic.
 * ************************************************************************** */

/* This all assumes two's complement arithmetic, in the meaning of the values
   which are computed and in many cases also for the purpose of the computation
   itself. */




/* Constants related to the representation of integers.
 * ************************************************************************** */

/* Expand to an expression whose result is an uint64_t bitmask, holding a single
   1 bit at the position of the sign bit and 0 elsewhere, assuming a two's
   complement representation in the given number of bits.  If the expression
   given for the number of bit is a constant expression then the expansion is a
   constant expression as well.  The number of bits may be any natural number up
   to 64 included. */
#define JITTER_SIGN_BIT_FOR_BITS(bit_no)  \
  ((uint64_t) 1 << ((bit_no) - 1))

/* Expand to a constant expression, like the expansion of
   JITTER_SIGN_BIT_FOR_BITS , for the number of bits in a Jitter word. */
#define JITTER_SIGN_BIT                            \
  JITTER_SIGN_BIT_FOR_BITS (JITTER_BITS_PER_WORD)




/* Integer representation limits.
 * ************************************************************************** */

/* Expand to a constant int64_t expression whose value is the most negative
   number representable with the given number of bits in two's complement.  The
   given number of bits must not be greater than 64. */
#define JITTER_MOST_NEGATIVE_SIGNED_IN_BITS(bit_no)  \
  ((int64_t) - (((uint64_t) 1) << ((bit_no) - 1)))

/* Expand to a constant int64_t expression whose value is the most positive
   number representable with the given number of bits in two's complement.  The
   given number of bits must not be greater than 64. */
#define JITTER_MOST_POSITIVE_SIGNED_IN_BITS(bit_no)     \
  ((int64_t) ((((uint64_t) 1) << ((bit_no) - 1)) - 1))

/* Expand to a constant uint64_t expression whose value is the largest number
   representable, unsigned, with the given number of bits.  The given number of
   bits must not be greater than 64. */
#define JITTER_MAXIMUM_UNSIGNED_IN_BITS(bit_no)  \
  ((((uint64_t) 1) << (bit_no)) - 1)

/* Expand to an expression evaluating to a Boolean.  The result will be
   non-false iff the given argument, taken as a word-sized unsigned integer,
   fits within the specified number of bits.  For example 0, 4, 45 or 255 fit in
   8 bits, but 256 does not.  The number of bits does not need to be a power of
   two and can even be greater than 63 -- in which case the result will evaluate
   to non-false, since any given number will be exactly representable in 64 or
   more bits.
   The expansion will be a constant expression if the arguments are constant
   expressions; however the arguments may be evaluated multiple times. */
#define JITTER_FITS_IN_BITS_ZERO_EXTENDED(word, bit_no)  \
  (((bit_no) >= 64)                                      \
   || (((uint64_t) (word))                               \
       <= JITTER_MAXIMUM_SIGNED_IN_BITS (bit_no)))

/* Like JITTER_FITS_IN_BITS_ZERO_EXTENDED, but use sign extension rather than
   zero extension. */
#define JITTER_FITS_IN_BITS_SIGN_EXTENDED(word, bit_no)        \
  (((bit_no) >= 64)                                            \
   || ((((int64_t) (uint64_t) (word))                          \
        >= JITTER_MOST_NEGATIVE_SIGNED_IN_BITS (bit_no))       \
       && (((int64_t) (uint64_t) (word))                       \
           <= JITTER_MOST_POSITIVE_SIGNED_IN_BITS (bit_no))))




/* Integer overflow checking.
 * ************************************************************************** */

/* This functionality provides ways of checking for overflow on signed before
   actually attempting an operation which would result in undefined behavior in
   C.

   At this time only signed integer operations are supported. */

/* Given a C integer type in its unsigned and signed variants, expressions a and
   b evaluating to integers of the given signed type and a number of significant
   bits (which a and b must fit in), expand to a Boolean expression, non-false
   iff the sum of a and b would overflow the same given number of bits.
   Arguments may be evaluated multiple times; bit_no should be a constant
   expression for an efficient expansion.
   The implementation will use GCC's overflow-checking primitives if available
   and if supported in the given number of bits. */
#ifdef JITTER_HAVE_GCC_OVERFLOW_CHECKING
# define JITTER_WOULD_PLUS_OVERFLOW(__jitter_unsigned_type,                \
                                    __jitter_signed_type,                  \
                                    a, b, bit_no)                          \
  (((bit_no) == 64)                                                        \
   ? JITTER_WOULD_PLUS_OVERFLOW_GCC (int64_t, (a), (b))                    \
   : (((bit_no) == 32)                                                     \
      ? JITTER_WOULD_PLUS_OVERFLOW_GCC (int32_t, (a), (b))                 \
      : (((bit_no) == 16)                                                  \
         ? JITTER_WOULD_PLUS_OVERFLOW_GCC (int16_t, (a), (b))              \
         : (((bit_no) == 8)                                                \
            ? JITTER_WOULD_PLUS_OVERFLOW_GCC (int8_t, (a), (b))            \
            : JITTER_WOULD_PLUS_OVERFLOW_NON_GCC (__jitter_unsigned_type,  \
                                                  __jitter_signed_type,    \
                                                  (a), (b), (bit_no))))))
#else // The GCC builtin is not available in this configuration.
# define JITTER_WOULD_PLUS_OVERFLOW(__jitter_unsigned_type,      \
                                    __jitter_signed_type,        \
                                    a, b, bit_no)                \
    JITTER_WOULD_PLUS_OVERFLOW_NON_GCC (__jitter_unsigned_type,  \
                                        __jitter_signed_type,    \
                                        (a), (b), (bit_no))
#endif // #ifdef JITTER_HAVE_GCC_OVERFLOW_CHECKING

/* A halper macro in the implementation of JITTER_WOULD_PLUS_OVERFLOW for the
   generic non-builtin case.  This does not evaluate to a Boolean, but to a
   signed word: the sum operation overflows iff the result of the expansion is
   negative.
   This is intended for use in a conditional fast branch checking the sign. */
#define JITTER_WOULD_PLUS_OVERFLOW_SIGNED_WORD(__jitter_unsigned_type,  \
                                               __jitter_signed_type,    \
                                               a, b, bit_no)            \
  /* The formula comes from Hacker's Delight, §2. */                    \
  (~ ((__jitter_unsigned_type) (a)                                      \
      ^ (__jitter_unsigned_type) (b))                                   \
   & (((__jitter_unsigned_type) (a))                                    \
      ^ (((__jitter_unsigned_type) (a)                                  \
          + (__jitter_unsigned_type) (b)))))

/* The generic non-builtin implementation of JITTER_WOULD_PLUS_OVERFLOW, with
   the same API.  This is not intended for the user to call directly. */
#define JITTER_WOULD_PLUS_OVERFLOW_NON_GCC(__jitter_unsigned_type,   \
                                           __jitter_signed_type,     \
                                           a, b, bit_no)             \
  (JITTER_WOULD_PLUS_OVERFLOW_SIGNED_WORD (__jitter_unsigned_type,   \
                                           __jitter_signed_type,     \
                                           (a), (b), (bit_no))       \
   & JITTER_SIGN_BIT_FOR_BITS(bit_no))

/* The GCC implementation of JITTER_WOULD_PLUS_OVERFLOW.  This is not intended
   for the user to call directly, and in fact is only correct when the given
   integer type matches the result width.  Again, not for the user. */
#define JITTER_WOULD_PLUS_OVERFLOW_GCC(__jitter_signed_type, a, b)  \
  (__builtin_add_overflow_p ((a), (b), (__jitter_signed_type) -1))

/* Like JITTER_WOULD_PLUS_OVERFLOW, but checking overflow for subtraction; the
   sign-bit idea is the same. */
#ifdef JITTER_HAVE_GCC_OVERFLOW_CHECKING
# define JITTER_WOULD_MINUS_OVERFLOW(__jitter_unsigned_type,                \
                                     __jitter_signed_type,                  \
                                     a, b, bit_no)                          \
  (((bit_no) == 64)                                                         \
   ? JITTER_WOULD_MINUS_OVERFLOW_GCC (int64_t, (a), (b))                    \
   : (((bit_no) == 32)                                                      \
      ? JITTER_WOULD_MINUS_OVERFLOW_GCC (int32_t, (a), (b))                 \
      : (((bit_no) == 16)                                                   \
         ? JITTER_WOULD_MINUS_OVERFLOW_GCC (int16_t, (a), (b))              \
         : (((bit_no) == 8)                                                 \
            ? JITTER_WOULD_MINUS_OVERFLOW_GCC (int8_t, (a), (b))            \
            : JITTER_WOULD_MINUS_OVERFLOW_NON_GCC (__jitter_unsigned_type,  \
                                                   __jitter_signed_type,    \
                                                   (a), (b), (bit_no))))))
#else // No GCC builtin available.
# define JITTER_WOULD_MINUS_OVERFLOW(__jitter_unsigned_type,      \
                                     __jitter_signed_type,        \
                                     a, b, bit_no)                \
    JITTER_WOULD_MINUS_OVERFLOW_NON_GCC (__jitter_unsigned_type,  \
                                         __jitter_signed_type,    \
                                         (a), (b), (bit_no))
#endif // #ifdef JITTER_HAVE_GCC_OVERFLOW_CHECKING
#define JITTER_WOULD_MINUS_OVERFLOW_SIGNED_WORD(__jitter_unsigned_type,  \
                                                __jitter_signed_type,    \
                                                a, b, bit_no)            \
  /* This formula comes from Hacker's Delight §2, again. */              \
  ((((__jitter_unsigned_type) (a) - (__jitter_unsigned_type) (b))        \
    ^ (__jitter_unsigned_type) (a))                                      \
   & (((__jitter_unsigned_type) (a) - (__jitter_unsigned_type) (b))      \
      ^ ~ (__jitter_unsigned_type) (b)))
#define JITTER_WOULD_MINUS_OVERFLOW_NON_GCC(__jitter_unsigned_type,   \
                                            __jitter_signed_type,     \
                                            a, b, bit_no)             \
  (JITTER_WOULD_MINUS_OVERFLOW_SIGNED_WORD (__jitter_unsigned_type,   \
                                            __jitter_signed_type,     \
                                            (a), (b), (bit_no))       \
   & JITTER_SIGN_BIT_FOR_BITS(bit_no))
#define JITTER_WOULD_MINUS_OVERFLOW_GCC(__jitter_signed_type, a, b)  \
  (__builtin_sub_overflow_p ((a), (b), (__jitter_signed_type) -1))

/* Like JITTER_WOULD_PLUS_OVERFLOW and JITTER_WOULD_MINUS_OVERFLOW, but checking
   overflow for multiplication; unfortunately the fallback non-builtin case for
   multiplication is less pretty and the idea of the sign bit does not apply.
   Apart from this the logic is the same as for sum and subtraction. */
#ifdef JITTER_HAVE_GCC_OVERFLOW_CHECKING
# define JITTER_WOULD_TIMES_OVERFLOW(__jitter_unsigned_type,                \
                                     __jitter_signed_type,                  \
                                     a, b, bit_no)                          \
  (((bit_no) == 64)                                                         \
   ? JITTER_WOULD_TIMES_OVERFLOW_GCC (int64_t, (a), (b))                    \
   : (((bit_no) == 32)                                                      \
      ? JITTER_WOULD_TIMES_OVERFLOW_GCC (int32_t, (a), (b))                 \
      : (((bit_no) == 16)                                                   \
         ? JITTER_WOULD_TIMES_OVERFLOW_GCC (int16_t, (a), (b))              \
         : (((bit_no) == 8)                                                 \
            ? JITTER_WOULD_TIMES_OVERFLOW_GCC (int8_t, (a), (b))            \
            : JITTER_WOULD_TIMES_OVERFLOW_NON_GCC (__jitter_unsigned_type,  \
                                                   __jitter_signed_type,    \
                                                   (a), (b), (bit_no))))))
#else // No GCC builtin available.
# define JITTER_WOULD_TIMES_OVERFLOW(__jitter_unsigned_type,     \
                                     __jitter_signed_type,       \
                                     a, b, bit_no)               \
    JITTER_WOULD_TIMES_OVERFLOW_NON_GCC (__jitter_unsigned_type,  \
                                         __jitter_signed_type,    \
                                         (a), (b), (bit_no))
#endif // #ifdef JITTER_HAVE_GCC_OVERFLOW_CHECKING
#define JITTER_WOULD_TIMES_OVERFLOW_NON_GCC(__jitter_unsigned_type,   \
                                            __jitter_signed_type,     \
                                            a, b, bit_no)             \
  /* This formula comes from Hacker's Delight §2, once more.  Notice  \
     that, differently from the cases for sum and subtraction, this   \
     works by case analysis and the result is a Boolean.  This will   \
     certainly be inefficient. */                                     \
  (((a) > 0)                                                          \
   ? (((b) > 0)                                                       \
      ? ((a) > ((__jitter_signed_type)                                \
                JITTER_MOST_POSITIVE_SIGNED_IN_BITS (bit_no)          \
                / (b)))                                               \
      : ((b) < ((__jitter_signed_type)                                \
                JITTER_MOST_NEGATIVE_SIGNED_IN_BITS (bit_no)          \
                / (a))))                                              \
   : (((b) > 0)                                                       \
      ? ((a) < ((__jitter_signed_type)                                \
                JITTER_MOST_NEGATIVE_SIGNED_IN_BITS (bit_no)          \
                / (b)))                                               \
      : ((a) != 0                                                     \
         && (b) < ((__jitter_signed_type)                             \
                   JITTER_MOST_POSITIVE_SIGNED_IN_BITS (bit_no)       \
                   / (a)))))
#define JITTER_WOULD_TIMES_OVERFLOW_GCC(__jitter_signed_type, a, b)  \
  (__builtin_mul_overflow_p ((a), (b), (__jitter_signed_type) -1))

/* Like the previous operations, checking overflow for division.  GCC has no
   builtin for this case. */
#define JITTER_WOULD_DIVIDED_OVERFLOW(__jitter_unsigned_type,  \
                                      __jitter_signed_type,    \
                                      a, b, bit_no)            \
  ((b) == 0                                                    \
    || ((a) == ((__jitter_signed_type)                         \
                JITTER_MOST_NEGATIVE_SIGNED_IN_BITS (bit_no))  \
       && (b) == -1))

/* Like the previous operations, checking overflow for remainder. */
#define JITTER_WOULD_REMAINDER_OVERFLOW(__jitter_unsigned_type,  \
                                        __jitter_signed_type,    \
                                        a, b, bit_no)            \
  /* Remainder will overflow in the same cases as division. */   \
  JITTER_WOULD_DIVIDED_OVERFLOW (__jitter_unsigned_type,  \
                                 __jitter_signed_type,    \
                                 (a), (b), (bit_no))




/* Sign function, as a macro.
 * ************************************************************************** */

/* Given an expression a, expand to a signed integer expression holding the sign
   of the result of the evaluation of a: one of -1, 0, +1.
   The argument may be evaluated more than once.  If the argument is a constant
   expression then the expansion is constant as well.
   This is also correct for floating-point arguments. */
#define JITTER_SIGN(n)                                                \
  /* The idea is from Hacker's Delight §2-8, which also presents an   \
     alternative equivalent to, in my notation,                       \
       (!! ((n) >= 0) - !! ((n) <= 0))                                \
     .  In C, ever since K&R, (! expression) is guaranteed to always  \
     yield either 0 or 1; therefore (!! expression) is a portable     \
     way of normalizing a Boolean. */                                 \
  (!! ((n) > 0) - !! ((n) < 0))




/* Boolean operations.
 * ************************************************************************** */

/* I might want to move this section to some other header. */

/* Expand to an expression evaluating to the logical (not bitwise) xor of the
   result of the evaluation of the two given Boolean expressions.  This is
   useful for comparing possibly non-normalized Booleans for inequality,
   obtaining a non-false value when the two arguments are (logically)
   different. */
#define JITTER_XOR(a, b)                                               \
  /* Implementation note: this normalization using ! is correct and    \
     portable, as (! exp) is guaranteed to evaluate to either 0 or 1.  \
     This has been the case forever, since even before the time of     \
     ANSI C. */                                                        \
  (! (a) != ! (b))

/* Similar to JITTER_XOR, but here the expansion evaluates to the logical
   negation of the logical xor of the results.  This is a convenient way of
   comparing two possibly non-normalized Booleans for Boolean equality. */
#define JITTER_NXOR(a, b)                              \
  /* See the implementation comment in JITTER_XOR. */  \
  (! (a) == ! (b))


#endif // #ifndef JITTER_ARITHMETIC_H_
