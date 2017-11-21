/* Jittery Lisp: s-expression header.

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


#ifndef JITTERLISP_SEXPRESSION_H_
#define JITTERLISP_SEXPRESSION_H_

#include <stdio.h>
#include <jitter/jitter.h>


/* About this file.
 * ************************************************************************** */

/* This header provides macro definitions for encoding and decoding
   s-expressions, which is to say for converting from a C object to the tagged
   Lisp representation of the same object, and vice-versa.

   Allocation and memory handling is *not* covered here: see
   jitterlisp-allocator.h . */




/* Multiple-evaluation warning.
 * ************************************************************************** */

/* Some of the macro defined in this header may evaluate their arguments
   multiple times.  Even if this situation currently applies to a small number
   of cases such details may easily change in the future and should not be
   relied upon.

   The user should not pass C expressions with side effects as arguments to the
   macros defined here. */




/* S-expression representation.
 * ************************************************************************** */

/* A JitterLisp object is, by convention, represented as a C unsigned integer.

   This type has the same width as a hardware machine word (jitter_int and
   jitter_uint are defined to be exactly as wide as a C pointer), so that they
   fit in hardware registers.

   An object whose entire value or state is held in the jitterlisp_object,
   without referring to memory, is called "unboxed".

   A "boxed" object contains a pointer to memory, where more information is
   stored in some form suitable to the object type. */
typedef jitter_uint jitterlisp_object;




/* S-expression representation: tags.
 * ************************************************************************** */

/* I reserve the least significant JITTERLISP_TAG_BIT_NO bits in every Lisp
   object, be it boxed or unboxed for its "tag", which contains type information
   about the object.

   An object tag contains some type information about the object, but not
   necessarily all of it.  Other relevant bits may be in the payload (see the
   section below about subtags) or, for boxed objects, in a header in memory. */
#define JITTERLISP_TAG_BIT_NO     3

/* The remaining JITTERLISP_NON_TAG_BIT_NO bits of every object (all its bits
   except for the tag) are by definition its "non-tag".  How the tag is encoded
   depends on the object type: with some tag values another small number of bits
   may be reserved in the non-tag part, to form a "sub-tag".  The bits on the
   left of the sub-tag (or directly on the left of the tag when the sub-tag has
   size zero) are the object "payload".

   According to the object type a payload may be an integer (unsigned or in
   two's complement), shifted left in the encoded jitterlisp_object to make
   place for tag and subtag, or an aligned pointer, not shifted as its least
   significant bits would always be zero without tag and subtag because of
   alignment. */
#define JITTERLISP_NON_TAG_BIT_NO (JITTER_BITS_PER_WORD - JITTERLISP_TAG_BIT_NO)

/* How many different tags we can represent with JITTERLISP_TAG_BIT_NO bits,
   also including any unused configuration.  This is useful for building
   arrays indexed by tag. */
#define JITTERLISP_TAG_NO         (((jitter_uint) 1) << JITTERLISP_TAG_BIT_NO)

/* A bitmask matching the tag part of the word. */
#define JITTERLISP_TAG_BIT_MASK   (((jitter_uint) (JITTERLISP_TAG_NO)) - 1)




/* S-expression representation: bitwise utility.
 * ************************************************************************** */

/* The words taken as arguments by the macros in this section and evaluated to
   by their expansions are all unsigned, of size sizeof (jitter_uint). */


/* Expand to an r-value evaluating to the most significant bit of the given
   word, not shifted. */
#define JITTERLISP_MOST_SIGNIFICANT_BIT(_jitterlisp_word)  \
  (((jitter_uint) (_jitterlisp_word))                      \
   & ((jitter_uint) 1) << (JITTER_BITS_PER_WORD - 1))

/* Expand to an r-value evaluating to a bitmask matching the given number of
   least significant bits in a word. */
#define JITTERLISP_BIT_MASK(_jitterlisp_bit_no)      \
  ((((jitter_uint) 1) << (_jitterlisp_bit_no)) - 1)

/* Expand to an r-value evaluating to a bit mask with the given number of
   consecutive 1 bits, the first one starting at the most significant position
   in a word. */
#define JITTERLISP_HIGH_BIT_MASK(_jitterlisp_bit_no)  \
  (JITTERLISP_BIT_MASK(_jitterlisp_bit_no)            \
   << (JITTER_BITS_PER_WORD - (_jitterlisp_bit_no)))

/* Expand to an r-value evaluating to the given word with the given number
   of its least significant bits set to zero.  No side effects.
   Rationale: this has the advantage of working independently from the
   original tag but might not be the most efficient solution when the original
   configuration is known; in particular these operations is more difficult for
   GCC to optimize away by combining them with arithmetic, including address
   arithmetic. */
#define JITTERLISP_WITH_BITS_MASKED_OFF(_jitterlisp_word,    \
                                        _jitterlisp_bit_no)  \
  (((jitter_uint) (_jitterlisp_word))                        \
   & ~ JITTERLISP_BIT_MASK(_jitterlisp_bit_no))

/* Expand to an r-value evaluating to the given word with the given number of
   its least significant bits set to the given bits.  No side effects.  This
   does not check that the given new bits fit in the given number of bits.  This
   works by first masking off bits and then applying a bitwise or on the result.
   Rationale: see JITTERLISP_WITH_BITS_MASKED_OFF . */
#define JITTERLISP_WITH_BITS_MASKED_ON(_jitterlisp_word,                  \
                                       _jitterlisp_new_bits,              \
                                       _jitterlisp_bit_no)                \
  (JITTERLISP_WITH_BITS_MASKED_OFF(_jitterlisp_word, _jitterlisp_bit_no)  \
   | ((jitter_uint) (_jitterlisp_new_bits)))

/* Expand to an r-value evaluating to the given word arithmetically
   right-shifted by _jitterlisp_bit_no .

   There are basically two separate implementations of this, one relying on >>
   sign-extending on signed operands, like GCC does, and another generic but
   slow solution.  Which implementation is used depends on a constant expression
   checking how >> behaves at compile time.  It might be desirable to move this
   logic to configure.

   Notice that the seemingly obvious alternative of doing a signed division by
   a power of two does not always compute the correct result with a negative
   operand in two's complement: the rounding direction for signed division is
   not what we need here. */
#define JITTERLISP_WITH_BITS_ASHIFTED_OFF(_jitterlisp_word,          \
                                          _jitterlisp_bit_no)        \
  (JITTERLISP_RIGHT_SHIFT_SIGN_EXTENDS                               \
   ? JITTERLISP_WITH_BITS_ASHIFTED_OFF_GCC(_jitterlisp_word,         \
                                           _jitterlisp_bit_no)       \
   : JITTERLISP_WITH_BITS_ASHIFTED_OFF_GENERIC(_jitterlisp_word,     \
                                               _jitterlisp_bit_no))

/* Expand to a constant expression, nonzero iff >> sign-extends (at least on an
   argument of size jitter_int , which is what we care about here).
   This is used in the implementation of JITTERLISP_WITH_BITS_ASHIFTED_OFF . */
#define JITTERLISP_RIGHT_SHIFT_SIGN_EXTENDS                             \
  /* We rely on one simple test.  Some ridiculous C compiler might */   \
  /* in theory behave in different ways according to the arguments, */  \
  /* but I don't feel pedantic enough to care about this. */            \
  ((((jitter_int) -56) >> 3)                                            \
   == ((jitter_int) -7))

/* One of the two implementations for JITTERLISP_WITH_BITS_ASHIFTED_OFF .  This
   definition is more efficient than the alternative but relies on >>
   sign-extending on signed operands like GCC does; the C standards doesn't
   define a behavior in this case. */
#define JITTERLISP_WITH_BITS_ASHIFTED_OFF_GCC(_jitterlisp_word,    \
                                              _jitterlisp_bit_no)  \
  ((jitter_uint)                                                   \
   (((jitter_int) (_jitterlisp_word))                              \
    >> (_jitterlisp_bit_no)))

/* One of the two implementations for JITTERLISP_WITH_BITS_ASHIFTED_OFF .  This
   solution is not pretty and probably very inefficient, but doesn't rely on
   arithmetic right shifts in its implementation. */
#define JITTERLISP_WITH_BITS_ASHIFTED_OFF_GENERIC(_jitterlisp_word,    \
                                                  _jitterlisp_bit_no)  \
  (JITTERLISP_MOST_SIGNIFICANT_BIT(_jitterlisp_word)                   \
   ? /* The word is negative: do a logic shift, then or a low bit */   \
     /* mask to the result to set the least significant bits to 1. */  \
     ((((jitter_uint) (_jitterlisp_word))                              \
       >> (_jitterlisp_bit_no))                                        \
      | JITTERLISP_HIGH_BIT_MASK(_jitterlisp_bit_no))                  \
   : /* The word is non-negative: just do a logic shift. */            \
     (((jitter_uint) (_jitterlisp_word))                               \
      >> (_jitterlisp_bit_no)))

/* Expand to an r-value evaluating to the given word modified by logically
   right-shifting the value by the given number of bits.  No side effects. */
#define JITTERLISP_WITH_BITS_LSHIFTED_OFF(_jitterlisp_word,     \
                                          _jitterlisp_bit_no)   \
  (((jitter_uint) (_jitterlisp_word)) >> (_jitterlisp_bit_no))

/* Expand to an r-value evaluating to the given word modified by left-shifting
   by the given number of bits and or-ing the given new bits to the result.  No
   side effects.  No check is made to ensure that the new bits actually fit in
   the least significant bit area which was filled with zeroes by shifting. */
#define JITTERLISP_WITH_BITS_SHIFTED_ON(_jitterlisp_word,            \
                                        _jitterlisp_new_bits,        \
                                        _jitterlisp_new_bit_no)      \
  ((((jitter_uint) (_jitterlisp_word)) << (_jitterlisp_new_bit_no))  \
   | (_jitterlisp_new_bits))

/* Expand to an r-value evaluating to the given word with the given new bits
   added.
   Rationale: by adding or removing known tag bits this way it is easy for GCC
   to compile a memory accesse from a tagged base with a known offset into a
   single base-plus-constant-offset load or store: the tag bits get combined
   with the memory offset into a compile-time constant.
   The same trick works for sums or subtractions among unshifted tagged fixnums,
   where some intermediate tagging and untagging operations may be avoided by
   merging them with others. */
#define JITTERLISP_WITH_BITS_ADDED(_jitterlisp_word,  \
                                   _jitterlisp_bits)  \
  (((jitter_uint) (_jitterlisp_word))                 \
   + ((jitter_uint) (_jitterlisp_bits)))

/* Expand to an r-value evaluating to the given word with the given new bits
   subtracted.
   Rationale: see the comment for JITTERLISP_WITH_BITS_ADDED . */
#define JITTERLISP_WITH_BITS_SUBTRACTED(_jitterlisp_word,  \
                                        _jitterlisp_bits)  \
  (((jitter_uint) (_jitterlisp_word))                      \
   - ((jitter_uint) (_jitterlisp_bits)))

/* Expand to an r-value evaluating to the given number of the least significant
   bits in the given word. */
#define JITTERLISP_GET_BITS(_jitterlisp_word,    \
                            _jitterlisp_bit_no)  \
  (((jitter_uint) (_jitterlisp_word))            \
   & JITTERLISP_BIT_MASK(_jitterlisp_bit_no))

/* Expand to an r-value evaluating to non-false iff the given word has the
   given number of its least significant bits equal to the given value. */
#define JITTERLISP_HAS_BITS(_jitterlisp_word,    \
                            _jitterlisp_bits,    \
                            _jitterlisp_bit_no)  \
  (JITTERLISP_GET_BITS(_jitterlisp_word,         \
                       _jitterlisp_bit_no)       \
   == ((jitter_uint) (_jitterlisp_bits)))




/* S-expression representation: encoding, decoding and tag checking.
 * ************************************************************************** */

/* An C object of the appropriate type can be "encoded" into a JitterLisp
   object, by storing it, or some pointer to it or to equivalent information in
   memory, combined with a tag.  "Decoding" is the opposite process converting a
   JitterLisp object to a C object, or a pointer to it.

   Encoding and decoding are non-destructive operations: they expand to
   expressions evaluating to values, and do not modify the result of their
   operand evaluation. */

/* Expand to an r-value evaluating to the tag of the given JitterLisp tagged
   object as an unsigned value. */
#define JITTERLISP_GET_TAG(_jitterlisp_tagged_object)                    \
  JITTERLISP_GET_BITS(_jitterlisp_tagged_object, JITTERLISP_TAG_BIT_NO)

/* Expand to an r-value evaluating to non-false iff the given object has the
   given tag. */
#define JITTERLISP_HAS_TAG(_jitterlisp_tagged_object, _jitterlisp_tag)  \
  JITTERLISP_HAS_BITS(_jitterlisp_tagged_object,                        \
                      _jitterlisp_tag,                                  \
                      JITTERLISP_TAG_BIT_NO)

/* Expand to an r-value evaluating to the given object representation modified
   by seeting all the tag bits to zero.  The object payload may or may not need
   to be shifted, according to its type.  No side effects.
   Rationale: see JITTERLISP_WITH_BITS_MASKED_OFF . */
#define JITTERLISP_WITH_TAG_MASKED_OFF(_jitterlisp_tagged_object)  \
  JITTERLISP_WITH_BITS_MASKED_OFF(_jitterlisp_tagged_object,       \
                                  JITTERLISP_TAG_BIT_NO)

/* Expand to an r-value evaluating to the given object representation modified
   by seeting all the tag bits to zero via a subtraction of the tag; this assumes
   that the encoded object has the provided tag, which is *not* checked.
   The object payload may or may not need to be shifted, according to its type.
   No side effects.

   Rationale: when loading or storing thru tagged pointers, particularly if the
   offset is a compile-time constant, the tag to be subtracted can be combined
   with the offset; this saves one bitwise and instruction. */
#define JITTERLISP_WITH_TAG_SUBTRACTED(_jitterlisp_tagged_object,  \
                                       _jitterlisp_tag)            \
  JITTERLISP_WITH_BITS_SUBTRACTED(_jitterlisp_tagged_object,       \
                                  _jitterlisp_tag)                 \

/* Expand to an r-value evaluating to the given object representation modified
   by arithmetically shifting the value right, eliminating the tag bits.  No
   side effects. */
#define JITTERLISP_WITH_TAG_ASHIFTED_OFF(_jitterlisp_tagged_object)  \
  JITTERLISP_WITH_BITS_ASHIFTED_OFF(_jitterlisp_tagged_object,       \
                                    JITTERLISP_TAG_BIT_NO)

/* Expand to an r-value evaluating to the given object representation modified
   by logically shifting the value right, eliminating the tag bits.  No
   side effects. */
#define JITTERLISP_WITH_TAG_LSHIFTED_OFF(_jitterlisp_tagged_object)  \
  JITTERLISP_WITH_BITS_LSHIFTED_OFF(_jitterlisp_tagged_object,       \
                                    JITTERLISP_TAG_BIT_NO)

/* Expand to an r-value evaluating to the Lisp representation of the given
   object, which must have a type castable to jitter_uint, on which the given
   tag is attached by left-shiting and or-ing. */
#define JITTERLISP_WITH_TAG_SHIFTED_ON(_jitterlisp_untagged_object,  \
                                       _jitterlisp_tag)              \
  JITTERLISP_WITH_BITS_SHIFTED_ON(_jitterlisp_untagged_object,       \
                                  _jitterlisp_tag,                   \
                                  JITTERLISP_TAG_BIT_NO)

/* Expand to an r-value evaluating to the Lisp representation of the given
   object, which must have a type castable to jitter_uint, on which the given
   tag is attached by simply adding it -- this assumes that the rightmost
   JITTERLISP_TAG_BIT_NO bits of the untagged representation are zero, and
   does *not* check that it's true. */
#define JITTERLISP_WITH_TAG_ADDED(_jitterlisp_untagged_object,  \
                                  _jitterlisp_tag)              \
  JITTERLISP_WITH_BITS_ADDED(_jitterlisp_untagged_object,       \
                             _jitterlisp_tag)                   \

/* Expand to an r-value evaluating to the Lisp representation of the given
   object, which must have a type castable to jitter_uint, on which the given
   tag is attached by masking-shiting and or-ing -- which is to say, by
   overwriting the rightmost JITTERLISP_TAG_BIT_NO bits of the untagged
   representation but without losing any bit of the left.

   Rationale: see the comment before JITTERLISP_WITH_TAG_MASKED_OFF. */
#define JITTERLISP_WITH_TAG_MASKED_ON(_jitterlisp_untagged_object,  \
                                      _jitterlisp_tag)              \
  JITTERLISP_WITH_BITS_MASKED_ON(_jitterlisp_untagged_object,       \
                                 _jitterlisp_tag,                   \
                                 JITTERLISP_TAG_BIT_NO)




/* S-expression representation: subtags.
 * ************************************************************************** */

/* Some objects  [FIXME: explain what subtags are.  One tag may be used with
   subtags of different length] */

/* Style/mnemonic convention: these macros have arguments always following
   this order:
   - object;
   - tag;
   - subtag;
   - subtag-bit-no.
   Macros not needing all of these still keep this ordering on the arguments
   they actually have. */

// FIXME: comment.
#define JITTERLISP_NON_TAG_BIT_NO                 \
  (JITTER_BITS_PER_WORD - JITTERLISP_TAG_BIT_NO)

// FIXME: comment.
#define JITTERLISP_EXTENDED_TAG_BIT_NO(_jitterlisp_subtag_bit_no)  \
  (JITTERLISP_TAG_BIT_NO + (_jitterlisp_subtag_bit_no))

// FIXME: comment.
#define JITTERLISP_PAYLOAD_BIT_NO(_jitterlisp_subtag_bit_no)     \
  (JITTERLISP_NON_TAG_BIT_NO                                     \
   - JITTERLISP_EXTENDED_TAG_BIT_NO(_jitterlisp_subtag_bit_no))

// FIXME: comment.
#define JITTERLISP_EXTENDED_TAG_BIT_MASK(_jitterlisp_subtag_bit_no)  \
  ((((jitter_uint) 1)                                                \
    << JITTERLISP_EXTENDED_TAG_BIT_NO(_jitterlisp_subtag_bit_no))    \
   - 1)

// FIXME: comment
#define JITTERLISP_EXTENDED_TAG(_jitterlisp_tag,            \
                                _jitterlisp_subtag,         \
                                _jitterlisp_subtag_bit_no)  \
  (((_jitterlisp_subtag) << (JITTERLISP_TAG_BIT_NO))        \
   | (_jitterlisp_tag))


// FIXME: comment
#define JITTERLISP_HAS_EXTENDED_TAG(_jitterlisp_tagged_object,              \
                                    _jitterlisp_tag,                        \
                                    _jitterlisp_subtag,                     \
                                    _jitterlisp_subtag_bit_no)              \
  (((_jitterlisp_tagged_object)                                             \
    & ((1u << (JITTERLISP_TAG_BIT_NO + (_jitterlisp_subtag_bit_no))) - 1))  \
   == (JITTERLISP_EXTENDED_TAG((_jitterlisp_tag),                           \
                               (_jitterlisp_subtag),                        \
                               (_jitterlisp_subtag_bit_no))))

/* FIXME: We have macros to access the tag of an object.  Add other macros to
   access an object subtag, extended tag, non-tag and payload. */

/* FIXME: Add macros to encode an object with tag and subtag, both shifting and
   masking.  Factor: add new macros to do encoding and decoding with a _given_
   number of bits.  Use the new macros to reimplement the tag macros above,
   and subtag macros here. */




/* S-expression representation: conventions.
 * ************************************************************************** */

/* Some computations on s-expression are more efficient with specific values of
   some tag or subtag.  When the definitions below need to make such assumptions
   they always do it within CPP conditionals checking for the actual value.

   Tag values and subtag values and widths must be kept easy to change in the
   future, even conditionally to accommodate for different hardware. */

/* The user is not supposed to directly access tags and subtags from ordinary
   code, as the specific details about tags and subtags may change in future
   versions.  Instead the user should call the macros for type checking,
   encoding, decoding, and operating on s-expressions. */


/* For every tagged type foo the following macros are defined:
   - the tag for foos, named JITTERLISP_FOO_TAG;
   - the untagged C type for foos, named JITTERLISP_FOO_UNTAGGED_TYPE;
   - the macro JITTERLISP_FOO_ENCODE(untagged_exp), expanding to an r-value
     evaluating to the tagged representation of the result of the evaluation
     of untagged_exp as a foo;
   - the macro JITTERLISP_FOO_DECODE(tagged_exp), expanding to an r-value
     evaluating to the untagged representation of the result of the evaluation
     of tagged_exp, as a JITTERLISP_FOO_UNTAGGED_TYPE.
   - for every predefined opration optr whose result and/or operands are foos, a
     macro JITTERLISP_FOO_OPTR(tagged_operand0, tagged_operand1, ...) with the
     appropriate number of tagged operands, evaluating to one tagged result; the
     operation internally performs untagging and tagging as required, but does
     *no type checking*.

   For ever JitterLisp type bar, not necessararily having its own tag, the
   following macros are defined:
   - JITTERLISP_IS_BAR(tagged_exp), expanding to an r-value which evaluates
     to a C boolean, non-false iff the tagged expression has type bar. */




/* S-expression representation: fixnums.
 * ************************************************************************** */

/* Fixnums are encoded as (JITTER_BITS_PER_WORD - JITTERLISP_TAG_BIT_NO)-bit
   two's complement integers, left-shifted by JITTERLISP_TAG_BIT_NO bits, with
   the tag in their rightmost JITTERLISP_TAG_BIT_NO bits.

   Fixnums are always signed.

   There are no subtags for fixnums. */

/* The tag for fixnums. */
#define JITTERLISP_FIXNUM_TAG            0b000

/* The C type for untagged fixnums.  Notice that fixnums are always signed. */
#define JITTERLISP_FIXNUM_UNTAGGED_TYPE  jitter_int

/* Expand to an r-value evaluating to a C (untagged) boolean which is non-false
   iff the given tagged object evaluates to a fixnum. */
#define JITTERLISP_IS_FIXNUM(_jitterlisp_tagged_object)  \
  (JITTERLISP_HAS_TAG((_jitterlisp_tagged_object),       \
                      JITTERLISP_FIXNUM_TAG))

/* Expand to an r-value evaluating to the encoded representation of the given
   untagged integer expression as a Lisp fixnum. */
#define JITTERLISP_FIXNUM_ENCODE(_jitterlisp_untagged_fixnum)     \
  (JITTERLISP_WITH_TAG_SHIFTED_ON((_jitterlisp_untagged_fixnum),  \
                                  JITTERLISP_FIXNUM_TAG))

/* Expand to an r-value evaluating to the untagged jitter_int content of the
   given tagged fixnum.  No type check is performed. */
#define JITTERLISP_FIXNUM_DECODE(_jitterlisp_tagged_fixnum)      \
  (JITTERLISP_WITH_TAG_ASHIFTED_OFF(_jitterlisp_tagged_fixnum))

/* Expand to an r-value expression evaluating to the tagged fixnum operation
   result having the given tagged fixnum operands, and the given infix C
   operation as the operator (the C operator working on untagged operands). */
#define JITTERLISP_FIXNUMS_TO_FIXNUM_BINARY(_jitterlisp_infix,            \
                                            _jitterlisp_tagged_fixnum_a,  \
                                            _jitterlisp_tagged_fixnum_b)  \
  (JITTERLISP_FIXNUM_ENCODE(                                              \
     (JITTERLISP_FIXNUM_DECODE(_jitterlisp_tagged_fixnum_a))              \
     _jitterlisp_infix                                                    \
     (JITTERLISP_FIXNUM_DECODE(_jitterlisp_tagged_fixnum_b))))

/* Right now fixnum-to-fixnum operations behave "correctly" on overflow and
   underflow, where correctly means that the result is always tagged correctly
   -- but the result decoded value will be whatever the C operators yielded.
   I'll have to do something more complex, and almost certainly less efficient,
   after introducing bignums. */

/* The plus and minus operations can be defined in a more efficient way than the
   others with respect to tagging, even more if the fixnum tag is zero. */
#if JITTERLISP_FIXNUM_TAG == 0
# define JITTERLISP_FIXNUM_PLUS_OR_MINUS(_jitterlisp_infix,            \
                                         _jitterlisp_tagged_fixnum_a,  \
                                         _jitterlisp_tagged_fixnum_b)  \
    (JITTERLISP_WITH_TAG_MASKED_ON(((_jitterlisp_tagged_fixnum_a)      \
                                    _jitterlisp_infix                  \
                                    (_jitterlisp_tagged_fixnum_b)),    \
                                   JITTERLISP_FIXNUM_TAG))
#else // JITTERLISP_FIXNUM_TAG != 0
# define JITTERLISP_FIXNUM_PLUS_OR_MINUS(_jitterlisp_infix,            \
                                         _jitterlisp_tagged_fixnum_a,  \
                                         _jitterlisp_tagged_fixnum_b)  \
    /* Notice that the infix operation is on unsigned operands. */     \
    (JITTERLISP_WITH_TAG_MASKED_ON((_jitterlisp_tagged_fixnum_a)       \
                                   _jitterlisp_infix                   \
                                   (JITTERLISP_WITH_TAG_SUBTRACTED(    \
                                      _jitterlisp_tagged_fixnum_b,     \
                                      JITTERLISP_FIXNUM_TAG)),         \
                                   JITTERLISP_FIXNUM_TAG))
#endif // #if JITTERLISP_FIXNUM_TAG == 0

/* Oprations on fixnums. */
#define JITTERLISP_FIXNUM_PLUS(_jitterlisp_tagged_fixnum_a,     \
                               _jitterlisp_tagged_fixnum_b)     \
  JITTERLISP_FIXNUM_PLUS_OR_MINUS(+,                            \
                                  _jitterlisp_tagged_fixnum_a,  \
                                  _jitterlisp_tagged_fixnum_b)
#define JITTERLISP_FIXNUM_MINUS(_jitterlisp_tagged_fixnum_a,    \
                                _jitterlisp_tagged_fixnum_b)    \
  JITTERLISP_FIXNUM_PLUS_OR_MINUS(-,                            \
                                  _jitterlisp_tagged_fixnum_a,  \
                                  _jitterlisp_tagged_fixnum_b)
#define JITTERLISP_FIXNUM_TIMES(_jitterlisp_tagged_fixnum_a,        \
                                _jitterlisp_tagged_fixnum_b)        \
  JITTERLISP_FIXNUMS_TO_FIXNUM_BINARY(*,                            \
                                      _jitterlisp_tagged_fixnum_a,  \
                                      _jitterlisp_tagged_fixnum_b)
#define JITTERLISP_FIXNUM_DIVIDED(_jitterlisp_tagged_fixnum_a,      \
                                  _jitterlisp_tagged_fixnum_b)      \
  JITTERLISP_FIXNUMS_TO_FIXNUM_BINARY(/,                            \
                                      _jitterlisp_tagged_fixnum_a,  \
                                      _jitterlisp_tagged_fixnum_b)
#define JITTERLISP_FIXNUM_REMAINDER(_jitterlisp_tagged_fixnum_a,    \
                                    _jitterlisp_tagged_fixnum_b)    \
  JITTERLISP_FIXNUMS_TO_FIXNUM_BINARY(%,                            \
                                      _jitterlisp_tagged_fixnum_a,  \
                                      _jitterlisp_tagged_fixnum_b)
#define JITTERLISP_FIXNUM_UNARY_MINUS(_jitterlisp_tagged_fixnum_a)  \
  JITTERLISP_FIXNUM_MINUS(JITTERLISP_FIXNUM_ENCODE(0),              \
                          _jitterlisp_tagged_fixnum_a)

/* In the case of comparison operators on fixnums a solution in the spirit of
   the more efficient solution for plus and minus above works with *any* tag.
   Notice that the operands must be compared as *signed*. */
#define JITTERLISP_FIXNUM_COMPARISON(_jitterlisp_infix,            \
                                     _jitterlisp_tagged_fixnum_a,  \
                                     _jitterlisp_tagged_fixnum_b)  \
  (JITTERLISP_BOOLEAN_ENCODE(((jitter_int)                         \
                              (_jitterlisp_tagged_fixnum_a))       \
                             _jitterlisp_infix                     \
                             ((jitter_int)                         \
                              (_jitterlisp_tagged_fixnum_b))))

/* Boolean operations on fixnum operands. */
#define JITTERLISP_FIXNUM_EQUAL(_jitterlisp_tagged_fixnum_a,  \
                                _jitterlisp_tagged_fixnum_b)  \
  JITTERLISP_FIXNUM_COMPARISON(==,                            \
                                _jitterlisp_tagged_fixnum_a,  \
                                _jitterlisp_tagged_fixnum_b)
#define JITTERLISP_FIXNUM_NOTEQUAL(_jitterlisp_tagged_fixnum_a,  \
                                _jitterlisp_tagged_fixnum_b)     \
  JITTERLISP_FIXNUM_COMPARISON(!=,                               \
                                _jitterlisp_tagged_fixnum_a,     \
                                _jitterlisp_tagged_fixnum_b)
#define JITTERLISP_FIXNUM_LESS(_jitterlisp_tagged_fixnum_a,  \
                               _jitterlisp_tagged_fixnum_b)  \
  JITTERLISP_FIXNUM_COMPARISON(<,                            \
                               _jitterlisp_tagged_fixnum_a,  \
                               _jitterlisp_tagged_fixnum_b)
#define JITTERLISP_FIXNUM_NOTLESS(_jitterlisp_tagged_fixnum_a,  \
                                  _jitterlisp_tagged_fixnum_b)  \
  JITTERLISP_FIXNUM_COMPARISON(>=,                              \
                               _jitterlisp_tagged_fixnum_a,     \
                               _jitterlisp_tagged_fixnum_b)
#define JITTERLISP_FIXNUM_GREATER(_jitterlisp_tagged_fixnum_a,  \
                                  _jitterlisp_tagged_fixnum_b)  \
  JITTERLISP_FIXNUM_LESS(_jitterlisp_tagged_fixnum_b,           \
                         _jitterlisp_tagged_fixnum_a)
#define JITTERLISP_FIXNUM_NOTGREATER(_jitterlisp_tagged_fixnum_a,  \
                                     _jitterlisp_tagged_fixnum_b)  \
  JITTERLISP_FIXNUM_NOTLESS(_jitterlisp_tagged_fixnum_b,           \
                            _jitterlisp_tagged_fixnum_a)




/* S-expression representation: unique and character values.
 * ************************************************************************** */

/* This tag is used for JitterLisp objects of which only one instance exists
   such as (), #t and #f, and also for character objects, who are about one
   million in Unicode and therefore can be easily represented unboxed along
   with every unique object.

   I prefer to assume a wide fixed-width encoding for characters in memory, such
   as UCF-4; however using single UTF-8 bytes as characters works as well; each
   one will be considered an object of its own.

   This is still quite wasteful in terms of the actual bit configuration used
   in the space of the possible configurations.  Some other type might fit
   here in the fugure, with just one more subtag bit to discriminate, which
   wouldn't be a problem for representing unique and character objects. */
#define JITTERLISP_UNIQUE_OR_CHARACTER_TAG  0b001 // FIXME: I've not really thought about what value is best here

// FIXME: generalize this concept into a comment section above.
/* Characters are encoded left-shifted by (JITTERLISP_TAG_BIT_NO
   + JITTERLISP_UNIQUE_OR_CHARACTER_SUBTAG_BIT_NO) bits, of course with a tag in
   the JITTERLISP_TAG_BIT_NO least significant bits.

   The subtag distinguishes characters from unique objects. */
#define JITTERLISP_UNIQUE_OR_CHARACTER_SUBTAG_BIT_NO  1
#define JITTERLISP_UNIQUE_SUBTAG                      0b1
#define JITTERLISP_CHARACTER_SUBTAG                   0b0

/* Expand to an r-value evaluating to a C (untagged) boolean which is non-false
   iff the given tagged object evaluates to a unique-or-character object. */
#define JITTERLISP_IS_UNIQUE_OR_CHARACTER(_jitterlisp_tagged_object) \
  (JITTERLISP_HAS_TAG(o, JITTERLISP_UNIQUE_OR_CHARACTER_TAG))

/* Expand to an r-value evaluating to a C (untagged) boolean which is non-false
   iff the given tagged object evaluates to a unique object. */
#define JITTERLISP_IS_UNIQUE(_jitterlisp_tagged_object)   \
  (JITTERLISP_HAS_EXTENDED_TAG(                           \
      (_jitterlisp_tagged_object),                        \
      JITTERLISP_UNIQUE_OR_CHARACTER_TAG,                 \
      JITTERLISP_UNIQUE_SUBTAG,                           \
      JITTERLISP_UNIQUE_OR_CHARACTER_SUBTAG_BIT_NO))

/* Like JITTERLISP_IS_UNIQUE , for characters. */
#define JITTERLISP_IS_CHARACTER(_jitterlisp_tagged_object)  \
  (JITTERLISP_HAS_EXTENDED_TAG(                             \
      (_jitterlisp_tagged_object),                          \
      JITTERLISP_UNIQUE_OR_CHARACTER_TAG,                   \
      JITTERLISP_CHARACTER_SUBTAG,                          \
      JITTERLISP_UNIQUE_OR_CHARACTER_SUBTAG_BIT_NO))

// FIXME: ugly.  Factor.
#define JITTERLISP_CHARACTER_ENCODE(_jitterlisp_untagged_character)  \
  (JITTERLISP_WITH_TAG_SHIFTED_ON(                                   \
     ((((jitter_int)(_jitterlisp_untagged_character))                \
       << JITTERLISP_UNIQUE_OR_CHARACTER_SUBTAG_BIT_NO)              \
      | JITTERLISP_CHARACTER_SUBTAG),                                \
     JITTERLISP_UNIQUE_OR_CHARACTER_TAG))

// FIXME: ugly.  Factor.
#define JITTERLISP_UNIQUE_ENCODE(_jitterlisp_unique_index)  \
  (JITTERLISP_WITH_TAG_SHIFTED_ON(                          \
      ((((jitter_uint)(_jitterlisp_unique_index))           \
        << JITTERLISP_UNIQUE_OR_CHARACTER_SUBTAG_BIT_NO)    \
       | JITTERLISP_UNIQUE_SUBTAG),                         \
      JITTERLISP_UNIQUE_OR_CHARACTER_TAG))

// FIXME: rewrite, factor.
#define JITTERLISP_UNIQUE_OR_CHARACTER_DECODE(_jitterlisp_tagged_object)  \
  ((jitter_int)                                                           \
   (((jitter_uint) (_jitterlisp_tagged_object))                           \
    >> (JITTERLISP_TAG_BIT_NO                                             \
        + JITTERLISP_UNIQUE_OR_CHARACTER_SUBTAG_BIT_NO)))

#define JITTERLISP_UNIQUE_DECODE(_jitterlisp_tagged_object)         \
  JITTERLISP_UNIQUE_OR_CHARACTER_DECODE(_jitterlisp_tagged_object)
#define JITTERLISP_CHARACTER_DECODE(_jitterlisp_tagged_object)      \
  JITTERLISP_UNIQUE_OR_CHARACTER_DECODE(_jitterlisp_tagged_object)

/* How many unique objects there are.  This of course must agree with the
   definitions below. */
#define JITTERLISP_UNIQUE_OBJECT_NO  5

/* Define every unique object.  The names in the definition of
   jitterlisp_unique_object_names must follow this order. */
#define JITTERLISP_FALSE       JITTERLISP_UNIQUE_ENCODE(0)
#define JITTERLISP_TRUE        JITTERLISP_UNIQUE_ENCODE(1)
#define JITTERLISP_EMPTY_LIST  JITTERLISP_UNIQUE_ENCODE(2)
#define JITTERLISP_EOF         JITTERLISP_UNIQUE_ENCODE(3)
#define JITTERLISP_NOTHING     JITTERLISP_UNIQUE_ENCODE(4)

/* Unique object predicates.  Since unique objects are unboxed (and unique)
   these are simple comparisons by identity.  The expansion evaluates to a
   C (untagged) boolean r-value. */
#define JITTERLISP_IS_EMPTY_LIST(_jitterlisp_tagged_object)  \
  ((_jitterlisp_tagged_object) == JITTERLISP_EMPTY_LIST)
#define JITTERLISP_IS_TRUE(_jitterlisp_tagged_object)  \
  ((_jitterlisp_tagged_object) == JITTERLISP_TRUE)
#define JITTERLISP_IS_FALSE(_jitterlisp_tagged_object)  \
  ((_jitterlisp_tagged_object) == JITTERLISP_FALSE)
#define JITTERLISP_IS_EOF(_jitterlisp_tagged_object)  \
  ((_jitterlisp_tagged_object) == JITTERLISP_EOF)
#define JITTERLISP_IS_NOTHING(_jitterlisp_tagged_object)  \
  ((_jitterlisp_tagged_object) == JITTERLISP_NOTHING)

/* Every unique object has a unique printable name.  The array is meant to be
   indexed by decoded unique values. */
extern const char * const
jitterlisp_unique_object_names [];




/* S-expression representation: Booleans.
 * ************************************************************************** */

/* The Boolean constants JITTERLISP_FALSE and JITTERLISP_TRUE are unique values
   like any other, already defined above.  Booleans are not a separate "type"
   in the sense of this file, but it may be convenient for the user to see them
   that way.  The following convenience macros provide the illusion. */

/* Booleans are not a "type", from the point of view of tagging.  But it's very
   easy to check whether a tagged object is a boolean or not.  The expansion
   evaluates to a C (untagged) boolean r-value. */
#define JITTERLISP_IS_BOOLEAN(_jitterlisp_tagged_object)  \
  (JITTERLISP_IS_TRUE(_jitterlisp_tagged_object)          \
   || JITTERLISP_IS_FALSE(_jitterlisp_tagged_object))

/* Expand to an r-value evaluating to a tagged boolean value, true iff the given
   argument evalutes to non-false.

   This is difficult to make always efficient without requiring C's booleans to
   be canonical, but should not be very critical.  The conditional expression
   should always be easy for GCC to optimize away when the encoded value is the
   result of a C comparison accessible to the compiler rather than coming, for
   example, from a function argument. */
#define JITTERLISP_BOOLEAN_ENCODE(_jitterlisp_untagged_bool)          \
  ((_jitterlisp_untagged_bool) ? JITTERLISP_TRUE : JITTERLISP_FALSE)

/* Expand to an r-value evaluating to a C (untagged) boolean value, false
   iff the argument evaluates to the false boolean tagged value. */
#define JITTERLISP_BOOLEAN_DECODE(_jitterlisp_tagged_bool)  \
  ((_jitterlisp_tagged_bool) != JITTERLISP_FALSE)




/* S-expression representation: symbols.
 * ************************************************************************** */

// FIXME: comment.

#define JITTERLISP_SYMBOL_TAG  0b010 // FIXME: I've not really thought about what value is the b

/* Symbols are handled in a special way allocation-wise: interned symbols are
   allocated with malloc, and live until the memory subsystem is finalized.
   Uninterned symbols, on the other hand, live on the garbage-collected heap.

   Both symbol types are encoded pointers to the following struct. */
struct jitterlisp_symbol
{
  /* The symbol name as a malloc-allocated string, or NULL if the symbol is not
     interned. */
  char *name_or_NULL;
};

#define JITTERLISP_IS_SYMBOL(_jitterlisp_tagged_object)  \
  (JITTERLISP_HAS_TAG((_jitterlisp_tagged_object),       \
                      JITTERLISP_SYMBOL_TAG))

#define JITTERLISP_SYMBOL_ENCODE(_jitterlisp_untagged_symbol)  \
  (JITTERLISP_WITH_TAG_ADDED(_jitterlisp_untagged_symbol,      \
                             JITTERLISP_SYMBOL_TAG))

#define JITTERLISP_SYMBOL_DECODE(_jitterlisp_tagged_symbol)      \
  ((struct jitterlisp_symbol *)                                  \
   (JITTERLISP_WITH_TAG_SUBTRACTED((_jitterlisp_tagged_symbol),  \
                                   JITTERLISP_SYMBOL_TAG)))




/* S-expression representation: conses.
 * ************************************************************************** */

// FIXME: comment.

#define JITTERLISP_CONS_TAG  0b011 // FIXME: I've not really thought about what value is the best

struct jitterlisp_cons
{
  jitterlisp_object car;
  jitterlisp_object cdr;
};

#define JITTERLISP_IS_CONS(_jitterlisp_tagged_object)  \
  (JITTERLISP_HAS_TAG((_jitterlisp_tagged_object),     \
                      JITTERLISP_CONS_TAG))

#define JITTERLISP_CONS_ENCODE(_jitterlisp_untagged_cons)  \
  (JITTERLISP_WITH_TAG_ADDED(_jitterlisp_untagged_cons,    \
                             JITTERLISP_CONS_TAG))

#define JITTERLISP_CONS_DECODE(_jitterlisp_tagged_cons)        \
  ((struct jitterlisp_cons *)                                  \
   (JITTERLISP_WITH_TAG_SUBTRACTED((_jitterlisp_tagged_cons),  \
                                   JITTERLISP_CONS_TAG)))




/* Not for the user: s-expression initialization and finalization.
 * ************************************************************************** */

/* These functions are called by jitterlisp_initialize and jitterlisp_finalize
   as needed.  They are not for the user to call directly. */

/* Initialize the s-expression subsystem.  This must be called before using any
   other function declared here. */
void
jitterlisp_sexpression_initialize (void);

/* Finalize the s-expression subsystem and free resources.  After this is called
   no other function declared here may be used again, until the subsystem is
   re-initialized with jitterlisp_sexpression_initialize. */
void
jitterlisp_sexpression_finalize (void);

#endif // #ifndef JITTERLISP_SEXPRESSION_H_
