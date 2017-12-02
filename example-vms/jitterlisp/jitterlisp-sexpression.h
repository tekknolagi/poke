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

/* We need the jitter_int and jitter_uint types. */
#include <jitter/jitter.h>




/* About this file.
 * ************************************************************************** */

/* This header provides macro definitions for encoding and decoding
   s-expressions, which is to say for converting from a C object to the tagged
   Lisp representation of the same object, and vice-versa.

   Allocation and memory handling is *not* covered here: see
   jitterlisp-allocator.h .  Operations on Lisp objects are not defined here
   either: see jitterlisp-operations.h . */




/* Correctness and performance caveats.
 * ************************************************************************** */

/* Some of the macro defined in this header may evaluate their arguments
   multiple times.  Even if this situation currently applies to a small number
   of cases such details may easily change in the future and should not be
   relied upon.

   The user should not pass C expressions with side effects as arguments to the
   macros defined here.

   Ptag and stag configurations, ptag lengths and stag lengths should be
   compile-time constants.  Everything should work even if those values were not
   known at compile time but there would be a considerable performance impact.

   Like Jitter-generated code, this code is designed to be *always* compiled
   with optimization; in particular some macros may expand to C conditional
   expressions with compile-time constants as conditions, meant to reduce to
   just one of the two branches at compile time. */




/* S-expression representation: bitwise expression utility.
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

/* Expand to an r-value evaluating to the given word with the given number of
   its least significant bits set to zero.  No side effects.  Rationale: this
   has the advantage of working independently from the original {p,s,}tag but
   might not be the most efficient solution when the original configuration is
   known; in particular these operations is more difficult for GCC to optimize
   away by combining them with arithmetic, including address arithmetic. */
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
   solution is not pretty and probably very inefficient, with a conditional
   essentially impossible to optimize away and even difficult to compile to
   non-branching code; but at least this doesn't rely on >> performing
   arithmetic right shifts on signed operands. */
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




/* S-expression representation: tags, ptags, stags.
 * ************************************************************************** */

/* I reserve at least the least significant JITTERLISP_PTAG_BIT_NO bits in
   every Lisp object, be it boxed or unboxed, for its "tag", which contains
   type information about the object.

   The tag is staged into two parts:
   - the primary tag or "ptag", always of exactly JITTERLISP_PTAG_BIT_NO bits,
     in the least significant part of the word;
   - the secondary tag or "stag", immediately on the left of the primary tag,
     of variable size.

   The stag can have different sizes according to the ptag, and the stag size
   may even differ for different cases belonging to the same ptag.
   The concatenation of stag and ptag is the full "tag" of an object.  Stags are
   chosen so as to make full tags unambiguous.

   Example:
     We could have a 2-bit ptag, and for its configuration 0b10 we could have
     three possible stags: 0b1 (1-bit), 0b00 (2-bit), 0b10 (2-bit).  By checking
     the least significant four bits on a word we can always distinguish the
     case: 0b?110, 0b0010, 0b1010; the first case has one more bit available to
     the payload compared to the other two.

   An object tag contains some type information about the object, but not
   necessarily all of it.  Other relevant bits, for some boxed objects, may be
   in a header in memory. */
#define JITTERLISP_PTAG_BIT_NO     3

/* The remaining JITTERLISP_NON_PTAG_BIT_NO bits of every object (all its bits
   except for the ptag) are by definition its "non-ptag".  How the full tag is
   encoded depends on the object type: with some ptag values another small
   number of bits may be reserved for the stag.  The bits on the left of the
   stag (or directly on the left of the tag when the stag has size zero) are
   the object "payload".

   According to the object type a payload may be an integer (unsigned or in
   two's complement), shifted left in the encoded jitterlisp_object to make
   place for stag and ptag, or an aligned pointer, not shifted as its least
   significant bits would always be zero without stag and ptag because of
   alignment. */
#define JITTERLISP_NON_PTAG_BIT_NO  \
  (JITTER_BITS_PER_WORD - JITTERLISP_PTAG_BIT_NO)

/* How many different ptags we can represent with JITTERLISP_PTAG_BIT_NO bits,
   also including any unused configuration.  This is useful for building
   arrays indexed by a ptag. */
#define JITTERLISP_PTAG_NO  \
  (((jitter_uint) 1) << JITTERLISP_PTAG_BIT_NO)

/* A bitmask matching the ptag part of the word. */
#define JITTERLISP_PTAG_BIT_MASK  \
  (((jitter_uint) (JITTERLISP_PTAG_NO)) - 1)




/* S-expression representation: ptag checking.
 * ************************************************************************** */

/* The operations below only access the ptag part of an object tag.  They are
   useful for implementing more complex tagging, untagging and checking
   operations below. */

/* Expand to an r-value evaluating to the ptag of the given JitterLisp tagged
   object as an unsigned value. */
#define JITTERLISP_GET_PTAG(_jitterlisp_tagged_object)                    \
  JITTERLISP_GET_BITS(_jitterlisp_tagged_object, JITTERLISP_PTAG_BIT_NO)

/* Expand to an r-value evaluating to non-false iff the given object has the
   given ptag. */
#define JITTERLISP_HAS_PTAG(_jitterlisp_tagged_object, _jitterlisp_ptag)  \
  JITTERLISP_HAS_BITS(_jitterlisp_tagged_object,                          \
                      _jitterlisp_ptag,                                   \
                      JITTERLISP_PTAG_BIT_NO)




/* S-expression representation: tag definitions.
 * ************************************************************************** */

/* How many bits in a word do not belong to the ptag.  This number of bits
   includes both the stag, if any, and the payload.  */
#define JITTERLISP_NON_PTAG_BIT_NO                 \
  (JITTER_BITS_PER_WORD - JITTERLISP_PTAG_BIT_NO)

/* How many bits in a word are taken by the full tag, given the stag size.
   Just a sum of course, but this macro makes the intent of some code more
   explicit. */
#define JITTERLISP_TAG_BIT_NO(_jitterlisp_stag_bit_no)  \
  (JITTERLISP_PTAG_BIT_NO + (_jitterlisp_stag_bit_no))

/* How many bits in a word are used for the payload, given the stag size. */
#define JITTERLISP_PAYLOAD_BIT_NO(_jitterlisp_stag_bit_no)  \
  (JITTERLISP_NON_PTAG_BIT_NO                               \
   - JITTERLISP_TAG_BIT_NO(_jitterlisp_stag_bit_no))

/* A bit mask (on the least significant part of a word) matching the full
   tag, given the stag size. */
#define JITTERLISP_TAG_BIT_MASK(_jitterlisp_stag_bit_no)  \
  ((((jitter_uint) 1)                                     \
    << JITTERLISP_TAG_BIT_NO(_jitterlisp_stag_bit_no))    \
   - 1)

/* Expand to a r-value evaluating to a bit configuration matching objects
   with the given ptag and stag, using the given stag size.  This is meant
   to be checked against the least significant part of a word. */
#define JITTERLISP_TAG(_jitterlisp_ptag,             \
                       _jitterlisp_stag,             \
                       _jitterlisp_stag_bit_no)      \
  (((_jitterlisp_stag) << (JITTERLISP_PTAG_BIT_NO))  \
   | (_jitterlisp_ptag))




/* S-expression representation: tag checking, tagging and untagging.
 * ************************************************************************** */

/* An C object of the appropriate type can be "encoded" into a JitterLisp
   object by representing it, or some pointer to it or to equivalent
   information in memory, combined with a tag.  "Decoding" is the opposite
   process converting a JitterLisp object to a C object, or a pointer to it.

   Encoding and decoding are non-destructive operations: they expand to
   expressions evaluating to values, and do not modify the result of their
   operand evaluation.  Memory allocation is a separate operation from encoding
   and decoding; memory operations are defined in jitterlisp-allocator.h ,
   not here. */

/* Style/mnemonic convention: these macros have arguments always following
   this order:
   - object;
   - ptag;
   - stag;
   - stag-bit-no.
   The general-purpose macros for tagging and untagging objects, meant to be
   used for defining type-specific tagging and untagging, take all of the
   arguments above, in the order above, even if some unneeded arguments may
   never be evaluated.  This makes the code easier to modify. */

/* Expand to a r-value evaluating to a boolean, non-false iff the given word has
   the full tag obtained by the given ptag and stag, using the given stag
   size. */
#define JITTERLISP_HAS_TAG(_jitterlisp_tagged_object,    \
                           _jitterlisp_ptag,             \
                           _jitterlisp_stag,             \
                           _jitterlisp_stag_bit_no)      \
  (((_jitterlisp_tagged_object)                          \
    & JITTERLISP_TAG_BIT_MASK(_jitterlisp_stag_bit_no))  \
   == (JITTERLISP_TAG((_jitterlisp_ptag),                \
                      (_jitterlisp_stag),                \
                      (_jitterlisp_stag_bit_no))))

/* Expand to an r-value evaluating to the Lisp representation of the given
   object, which must have a type castable to jitter_uint, on which the given
   full tag is attached by left-shiting and or-ing. */
#define JITTERLISP_WITH_TAG_SHIFTED_ON(_jitterlisp_untagged_object,  \
                                       _jitterlisp_ptag,             \
                                       _jitterlisp_stag,             \
                                       _jitterlisp_stag_bit_no)      \
  JITTERLISP_WITH_BITS_SHIFTED_ON(                                   \
     _jitterlisp_untagged_object,                                    \
     JITTERLISP_TAG((_jitterlisp_ptag),                              \
                    (_jitterlisp_stag),                              \
                    (_jitterlisp_stag_bit_no)),                      \
     JITTERLISP_TAG_BIT_NO(_jitterlisp_stag_bit_no))

/* Expand to an r-value evaluating to the given object representation modified
   by arithmetically shifting the value right, eliminating the full tag bits.
   No side effects. */
#define JITTERLISP_WITH_TAG_ASHIFTED_OFF(_jitterlisp_tagged_object,  \
                                         _jitterlisp_ptag,           \
                                         _jitterlisp_stag,           \
                                         _jitterlisp_stag_bit_no)    \
  JITTERLISP_WITH_BITS_ASHIFTED_OFF(                                 \
     _jitterlisp_tagged_object,                                      \
     JITTERLISP_TAG_BIT_NO(_jitterlisp_stag_bit_no))

/* Expand to an r-value evaluating to the given object representation modified
   by logically shifting the value right, eliminating the full tag bits.  No
   side effects. */
#define JITTERLISP_WITH_TAG_LSHIFTED_OFF(_jitterlisp_tagged_object,  \
                                         _jitterlisp_ptag,           \
                                         _jitterlisp_stag,           \
                                         _jitterlisp_stag_bit_no)    \
  JITTERLISP_WITH_BITS_LSHIFTED_OFF(                                 \
     _jitterlisp_tagged_object,                                      \
     JITTERLISP_TAG_BIT_NO(_jitterlisp_stag_bit_no))

/* Expand to an r-value evaluating to the Lisp representation of the given
   object, which must have a type castable to jitter_uint, on which the given
   full tag is attached by masking-shiting and or-ing -- which is to say,
   by overwriting the rightmost JITTERLISP_TAG_BIT_NO bits of the
   untagged representation but without losing any bit of the left.

   Rationale: see the comment before JITTERLISP_WITH_TAG_MASKED_OFF. */
#define JITTERLISP_WITH_TAG_MASKED_ON(_jitterlisp_untagged_object,  \
                                      _jitterlisp_ptag,             \
                                      _jitterlisp_stag,             \
                                      _jitterlisp_stag_bit_no)      \
  JITTERLISP_WITH_BITS_MASKED_ON(                                   \
     _jitterlisp_untagged_object,                                   \
     _jitterlisp_ptag,                                              \
     JITTERLISP_TAG_BIT_NO(_jitterlisp_stag_bit_no))

/* Expand to an r-value evaluating to the given object representation modified
   by seeting all the full tag bits to zero.  The object payload may or may not
   need to be shifted according to its type, but that operation is not performed
   by the expansion of this macro.  No side effects.
   Rationale: see JITTERLISP_WITH_BITS_MASKED_OFF . */
#define JITTERLISP_WITH_TAG_MASKED_OFF(_jitterlisp_untagged_object,  \
                                       _jitterlisp_ptag,             \
                                       _jitterlisp_stag,             \
                                       _jitterlisp_stag_bit_no)      \
  JITTERLISP_WITH_BITS_MASKED_OFF(                                   \
     _jitterlisp_tagged_object,                                      \
     JITTERLISP_TAG_BIT_NO(_jitterlisp_stag_bit_no))

/* Expand to an r-value evaluating to the Lisp representation of the given
   object, which must have a type castable to jitter_uint, on which the given
   full tag is attached by simply adding it -- this assumes that the rightmost
   JITTERLISP_TAG_BIT_NO bits of the untagged representation are zero, and does
   *not* check that it's true. */
#define JITTERLISP_WITH_TAG_ADDED(_jitterlisp_untagged_object,         \
                                  _jitterlisp_ptag,                    \
                                  _jitterlisp_stag,                    \
                                  _jitterlisp_stag_bit_no)             \
  JITTERLISP_WITH_BITS_ADDED(_jitterlisp_untagged_object,              \
                             JITTERLISP_TAG(_jitterlisp_ptag,          \
                                            _jitterlisp_stag,          \
                                            _jitterlisp_stag_bit_no))

/* Expand to an r-value evaluating to the given object representation modified
   by seeting all the full tag bits to zero via a subtraction of the entire full
   tag; this assumes that the encoded object has the provided full tag, which is
   *not* checked.
   The object payload may or may not need to be shifted, according to its type;
   this macro expansion does not do that.  No side effects.

   Rationale: when loading or storing thru tagged pointers, particularly if the
   offset is a compile-time constant, the full tag to be subtracted can be
   combined with the offset; this saves one bitwise and instruction. */
#define JITTERLISP_WITH_TAG_SUBTRACTED(_jitterlisp_tagged_object,           \
                                       _jitterlisp_ptag,                    \
                                       _jitterlisp_stag,                    \
                                       _jitterlisp_stag_bit_no)             \
  JITTERLISP_WITH_BITS_SUBTRACTED(_jitterlisp_tagged_object,                \
                                  JITTERLISP_TAG(_jitterlisp_ptag,          \
                                                 _jitterlisp_stag,          \
                                                 _jitterlisp_stag_bit_no))




/* S-expression representation: conventions.
 * ************************************************************************** */

/* Some computations on s-expression are more efficient with specific values of
   ptag or stag.  When the definitions below need to make such assumptions they
   always do it within CPP conditionals checking for the actual value.

   Ptag and stag values and widths must be kept easy to change in the future,
   even conditionally to accommodate for different hardware. */

/* The user is not supposed to directly access tags and stags from ordinary
   code, as the specific details about tags and stags may change in future
   versions.  Instead the user should call the macros for type checking,
   encoding and decoding s-expressions.

   Operations on tagged objects are not defined here: see
   jitterlisp-operations.h . */


/* For every tagged type foo the following macros are defined:
   - the ptag for foos, named JITTERLISP_FOO_PTAG;
   - the untagged C type for foos, named JITTERLISP_FOO_UNTAGGED_TYPE;
   - the macro JITTERLISP_FOO_ENCODE(untagged_exp), expanding to an r-value
     evaluating to the tagged representation of the result of the evaluation
     of untagged_exp as a foo;
   - the macro JITTERLISP_FOO_DECODE(tagged_exp), expanding to an r-value
     evaluating to the untagged representation of the result of the evaluation
     of tagged_exp, as a JITTERLISP_FOO_UNTAGGED_TYPE.

   For ever JitterLisp type bar, not necessararily having its own tag, the
   following macros are defined:
   - JITTERLISP_IS_BAR(tagged_exp), expanding to an r-value which evaluates
     to a C boolean, non-false iff the tagged expression has type bar. */




/* S-expression representation: dummy "anything" type.
 * ************************************************************************** */

/* This dummy tag check succeeds with any object.  It is convenient to have for
   machine-generated code containing a tag-checking macro, which sometimes need
   no actual check. */
#define JITTERLISP_IS_ANYTHING(_jitterlisp_tagged_object)  \
  true




/* S-expression representation: fixnums.
 * ************************************************************************** */

/* Fixnums are encoded as (JITTER_BITS_PER_WORD - JITTERLISP_PTAG_BIT_NO)-bit
   two's complement integers, left-shifted by JITTERLISP_PTAG_BIT_NO bits, with
   the tag in their rightmost JITTERLISP_PTAG_BIT_NO bits.

   Fixnums are always signed. */

/* The tag for fixnums.  Notice that a zero full tag allows for more efficient
   sum and subtraction operations, and this is exploited in the operation
   definitions. */
#define JITTERLISP_FIXNUM_PTAG           0b000
#define JITTERLISP_FIXNUM_STAG_BIT_NO    0
#define JITTERLISP_FIXNUM_STAG           0b0

/* The C type for untagged fixnums.  Notice that fixnums are always signed. */
#define JITTERLISP_FIXNUM_UNTAGGED_TYPE  jitter_int

/* Expand to an r-value evaluating to a C (untagged) boolean which is non-false
   iff the given tagged object evaluates to a fixnum. */
#define JITTERLISP_IS_FIXNUM(_jitterlisp_tagged_object)  \
  JITTERLISP_HAS_TAG((_jitterlisp_tagged_object),        \
                     JITTERLISP_FIXNUM_PTAG,             \
                     JITTERLISP_FIXNUM_STAG,             \
                     JITTERLISP_FIXNUM_STAG_BIT_NO)

/* Expand to an r-value evaluating to the encoded representation of the given
   untagged integer expression as a Lisp fixnum. */
#define JITTERLISP_FIXNUM_ENCODE(_jitterlisp_untagged_fixnum)    \
  JITTERLISP_WITH_TAG_SHIFTED_ON((_jitterlisp_untagged_fixnum),  \
                                 JITTERLISP_FIXNUM_PTAG,         \
                                 JITTERLISP_FIXNUM_STAG,         \
                                 JITTERLISP_FIXNUM_STAG_BIT_NO)

/* Expand to an r-value evaluating to the untagged jitter_int content of the
   given tagged fixnum.  No type check is performed. */
#define JITTERLISP_FIXNUM_DECODE(_jitterlisp_tagged_fixnum)        \
  JITTERLISP_WITH_TAG_ASHIFTED_OFF(_jitterlisp_tagged_fixnum,      \
                                   JITTERLISP_FIXNUM_PTAG,         \
                                   JITTERLISP_FIXNUM_STAG,         \
                                   JITTERLISP_FIXNUM_STAG_BIT_NO)




/* S-expression representation: unique and character values.
 * ************************************************************************** */

/* This ptag is used for JitterLisp objects of which only one instance exists
   such as (), #t and #f, and also for character objects, who are about one
   million in Unicode and therefore can be easily represented unboxed along
   with every unique object.

   I prefer to assume a wide fixed-width encoding for characters in memory, such
   as UCF-4; however using single UTF-8 bytes as characters works as well; each
   one will be considered an object of its own.

   This is still quite wasteful in terms of the actual bit configuration used
   in the space of the possible configurations.  Some other type might fit
   here in the future, with just one more stag bit to discriminate, which
   wouldn't be a problem for representing unique and character objects. */
#define JITTERLISP_UNIQUE_OR_CHARACTER_PTAG         0b001

/* How to distinguish unique values from characters. */
#define JITTERLISP_UNIQUE_STAG_BIT_NO               1
#define JITTERLISP_UNIQUE_STAG                      0b1
#define JITTERLISP_CHARACTER_STAG_BIT_NO            1
#define JITTERLISP_CHARACTER_STAG                   0b0

/* Expand to an r-value evaluating to a C (untagged) boolean which is non-false
   iff the given tagged object evaluates to a unique object. */
#define JITTERLISP_IS_UNIQUE(_jitterlisp_tagged_object)    \
  JITTERLISP_HAS_TAG((_jitterlisp_tagged_object),          \
                     JITTERLISP_UNIQUE_OR_CHARACTER_PTAG,  \
                     JITTERLISP_UNIQUE_STAG,               \
                     JITTERLISP_UNIQUE_STAG_BIT_NO)

/* Like JITTERLISP_IS_UNIQUE , for characters. */
#define JITTERLISP_IS_CHARACTER(_jitterlisp_tagged_object)  \
  JITTERLISP_HAS_TAG((_jitterlisp_tagged_object),           \
                     JITTERLISP_UNIQUE_OR_CHARACTER_PTAG,   \
                     JITTERLISP_CHARACTER_STAG,             \
                     JITTERLISP_CHARACTER_STAG_BIT_NO)

/* Encode operation for a character. */
#define JITTERLISP_CHARACTER_ENCODE(_jitterlisp_untagged_character)    \
  JITTERLISP_WITH_TAG_SHIFTED_ON((_jitterlisp_untagged_character),     \
                                 JITTERLISP_UNIQUE_OR_CHARACTER_PTAG,  \
                                 JITTERLISP_CHARACTER_STAG,            \
                                 JITTERLISP_CHARACTER_STAG_BIT_NO)

/* Encode operation for a unique-object index. */
#define JITTERLISP_UNIQUE_ENCODE(_jitterlisp_unique_index)             \
  JITTERLISP_WITH_TAG_SHIFTED_ON((_jitterlisp_unique_index),           \
                                 JITTERLISP_UNIQUE_OR_CHARACTER_PTAG,  \
                                 JITTERLISP_UNIQUE_STAG,               \
                                 JITTERLISP_UNIQUE_STAG_BIT_NO)

/* Decode operation for a character. */
#define JITTERLISP_UNIQUE_DECODE(_jitterlisp_tagged_object)              \
  JITTERLISP_WITH_TAG_LSHIFTED_OFF((_jitterlisp_tagged_object),          \
                                   JITTERLISP_UNIQUE_OR_CHARACTER_PTAG,  \
                                   JITTERLISP_UNIQUE_STAG,               \
                                   JITTERLISP_UNIQUE_STAG_BIT_NO)

/* Decode operation for a unique-object index. */
#define JITTERLISP_CHARACTER_DECODE(_jitterlisp_tagged_object)           \
  JITTERLISP_WITH_TAG_LSHIFTED_OFF((_jitterlisp_tagged_object),          \
                                   JITTERLISP_UNIQUE_OR_CHARACTER_PTAG,  \
                                   JITTERLISP_CHARACTER_STAG,            \
                                   JITTERLISP_CHARACTER_STAG_BIT_NO)

/* Every unique object has a unique printable name.  The array is meant to be
   indexed by decoded unique values and is defined to have exactly
   JITTERLISP_UNIQUE_OBJECT_NO elements. */
extern const char * const
jitterlisp_unique_object_names [];




/* S-expression representation: specific unique objects.
 * ************************************************************************** */

/* Unique objects are simple unboxed objects, each represented as a shifted
   index.  They are efficient to compare to, but by themselves hold no mutable
   state and no attributes. */

/* How many unique objects there are.  This of course must agree with the
   definitions below. */
#define JITTERLISP_UNIQUE_OBJECT_NO  6

/* Define every unique object.  The names in the definition of
   jitterlisp_unique_object_names must follow this order. */

/* The #f object.  In JitterLisp this is distinct from () and the symbol named
   nil, which is a symbol like any other. */
#define JITTERLISP_FALSE       JITTERLISP_UNIQUE_ENCODE(0)

/* The #t object. */
#define JITTERLISP_TRUE        JITTERLISP_UNIQUE_ENCODE(1)

/* The () object.  In JitterLisp this is distinct from #f and the symbol named
   nil, which is a symbol like any other. */
#define JITTERLISP_EMPTY_LIST  JITTERLISP_UNIQUE_ENCODE(2)

/* The end-of-file or end-of-input object.  This object has no reader syntax. */
#define JITTERLISP_EOF         JITTERLISP_UNIQUE_ENCODE(3)

/* A unique object conventionally used as the result of forms not evaluating to
   any useful result.  No reader syntax. */
#define JITTERLISP_NOTHING     JITTERLISP_UNIQUE_ENCODE(4)

/* A unique object used to represent the global value of globally unbound
   symbols.  This is used in the internal symbol representation but should never
   be the result of an evaluation.  No reader syntax, of course. */
#define JITTERLISP_UNDEFINED   JITTERLISP_UNIQUE_ENCODE(5)

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
#define JITTERLISP_IS_UNDEFINED(_jitterlisp_tagged_object)  \
  ((_jitterlisp_tagged_object) == JITTERLISP_UNDEFINED)




/* S-expression representation: Booleans.
 * ************************************************************************** */

/* The Boolean constants JITTERLISP_FALSE and JITTERLISP_TRUE are unique values
   like any other, already defined above.  Booleans are not a separate "type"
   in the sense of tags or even extended tags, but it is convenient for the user
   to see them that way.  The following convenience macros provide the
   illusion. */

/* Expand to an r-value (untagged) boolean evaluating to non-false iff the
   given tagged object is #t or #f . */
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
   iff the argument evaluates to the false boolean tagged value.  This is
   the preferred way of checking a JitterLisp object for falsity. */
#define JITTERLISP_BOOLEAN_DECODE(_jitterlisp_tagged_bool)  \
  ((_jitterlisp_tagged_bool) != JITTERLISP_FALSE)




/* S-expression representation: symbols.
 * ************************************************************************** */

/* Symbols are handled in a special way allocation-wise: interned symbols are
   allocated with malloc, and live until the memory subsystem is finalized.
   Uninterned symbols, on the other hand, live on the garbage-collected heap.

   Both symbol types are encoded pointers to a struct jitterlisp_symbol . */
#define JITTERLISP_SYMBOL_PTAG         0b010

#define JITTERLISP_SYMBOL_STAG_BIT_NO  0
#define JITTERLISP_SYMBOL_STAG         0b0

/* A symbol datum. */
struct jitterlisp_symbol
{
  /* The symbol name as a malloc-allocated string, or NULL if the symbol is not
     interned. */
  char *name_or_NULL;

  /* The value bound to the symbol in the global environment, or
     JITTERLISP_UNDEFINED if there is no global binding.

     FIXME: this makes each symbol a GC root which will require some careful
     testing in the case of interned symbols, as they are malloc-allocated. */
  jitterlisp_object global_value;
};

/* Symbol tag checking, encoding and decoding. */
#define JITTERLISP_IS_SYMBOL(_jitterlisp_tagged_object)  \
  JITTERLISP_HAS_TAG((_jitterlisp_tagged_object),        \
                     JITTERLISP_SYMBOL_PTAG,             \
                     JITTERLISP_SYMBOL_STAG,             \
                     JITTERLISP_SYMBOL_STAG_BIT_NO)
#define JITTERLISP_SYMBOL_ENCODE(_jitterlisp_untagged_symbol)  \
  JITTERLISP_WITH_TAG_ADDED(_jitterlisp_untagged_symbol,       \
                            JITTERLISP_SYMBOL_PTAG,            \
                            JITTERLISP_SYMBOL_STAG,            \
                            JITTERLISP_SYMBOL_STAG_BIT_NO)
#define JITTERLISP_SYMBOL_DECODE(_jitterlisp_tagged_symbol)          \
  ((struct jitterlisp_symbol *)                                      \
   (JITTERLISP_WITH_TAG_SUBTRACTED((_jitterlisp_tagged_symbol),      \
                                   JITTERLISP_SYMBOL_PTAG,           \
                                   JITTERLISP_SYMBOL_STAG,           \
                                   JITTERLISP_SYMBOL_STAG_BIT_NO)))




/* S-expression representation: conses.
 * ************************************************************************** */

/* Conses are represented boxed, with no header.  Accessing the car and cdr
   fields in memory is a frequent operation so it's particularly important to
   decode tagged conses with subtractions, which are often optimizable. */

#define JITTERLISP_CONS_PTAG           0b011

#define JITTERLISP_CONS_STAG_BIT_NO    0
#define JITTERLISP_CONS_STAG           0b0

/* A cons datum. */
struct jitterlisp_cons
{
  /* The first cons field. */
  jitterlisp_object car;

  /* The second cons field. */
  jitterlisp_object cdr;
};

/* Cons tag checking, encoding and decoding. */
#define JITTERLISP_IS_CONS(_jitterlisp_tagged_object)  \
  JITTERLISP_HAS_TAG((_jitterlisp_tagged_object),      \
                     JITTERLISP_CONS_PTAG,             \
                     JITTERLISP_CONS_STAG,             \
                     JITTERLISP_CONS_STAG_BIT_NO)
#define JITTERLISP_CONS_ENCODE(_jitterlisp_untagged_cons)  \
  JITTERLISP_WITH_TAG_ADDED(_jitterlisp_untagged_cons,     \
                            JITTERLISP_CONS_PTAG,          \
                            JITTERLISP_CONS_STAG,          \
                            JITTERLISP_CONS_STAG_BIT_NO)
#define JITTERLISP_CONS_DECODE(_jitterlisp_tagged_cons)            \
  ((struct jitterlisp_cons *)                                      \
   (JITTERLISP_WITH_TAG_SUBTRACTED((_jitterlisp_tagged_cons),      \
                                   JITTERLISP_CONS_PTAG,           \
                                   JITTERLISP_CONS_STAG,           \
                                   JITTERLISP_CONS_STAG_BIT_NO)))



/* S-expression representation: closures.
 * ************************************************************************** */

/* Closures are represented boxed, with no header. */

#define JITTERLISP_CLOSURE_PTAG           0b100

#define JITTERLISP_CLOSURE_STAG_BIT_NO    0
#define JITTERLISP_CLOSURE_STAG           0b0

/* A closure datum.  The exact list of fields will probably change. */
struct jitterlisp_closure
{
  /* The non-global environment, as an a-list. */
  jitterlisp_object environment;

  /* The function formal arguments as a list of symbols. */
  jitterlisp_object formals;

  /* The function body as a form list. */
  jitterlisp_object body;
};

/* Closure tag checking, encoding and decoding. */
#define JITTERLISP_IS_CLOSURE(_jitterlisp_tagged_object)  \
  JITTERLISP_HAS_TAG((_jitterlisp_tagged_object),         \
                     JITTERLISP_CLOSURE_PTAG,             \
                     JITTERLISP_CLOSURE_STAG,             \
                     JITTERLISP_CLOSURE_STAG_BIT_NO)
#define JITTERLISP_CLOSURE_ENCODE(_jitterlisp_untagged_closure)  \
  JITTERLISP_WITH_TAG_ADDED(_jitterlisp_untagged_closure,        \
                            JITTERLISP_CLOSURE_PTAG,             \
                            JITTERLISP_CLOSURE_STAG,             \
                            JITTERLISP_CLOSURE_STAG_BIT_NO)
#define JITTERLISP_CLOSURE_DECODE(_jitterlisp_tagged_closure)         \
  ((struct jitterlisp_closure *)                                      \
   (JITTERLISP_WITH_TAG_SUBTRACTED((_jitterlisp_tagged_closure),      \
                                   JITTERLISP_CLOSURE_PTAG,           \
                                   JITTERLISP_CLOSURE_STAG,           \
                                   JITTERLISP_CLOSURE_STAG_BIT_NO)))




/* S-expression representation: vectors.
 * ************************************************************************** */

/* Vectors are represented boxed as a header pointing to the actual vector
   elements as a separate heap buffer, not directly accessible by the user. */

#define JITTERLISP_VECTOR_PTAG           0b101

#define JITTERLISP_VECTOR_STAG_BIT_NO    0
#define JITTERLISP_VECTOR_STAG           0b0

/* A vector header. */
struct jitterlisp_vector
{
  /* How many elements there are. */
  jitter_uint element_no;

  /* A pointer to the first element. */
  jitterlisp_object *elements;
};

/* Vector tag checking, encoding and decoding. */
#define JITTERLISP_IS_VECTOR(_jitterlisp_tagged_object)  \
  JITTERLISP_HAS_TAG((_jitterlisp_tagged_object),        \
                     JITTERLISP_VECTOR_PTAG,             \
                     JITTERLISP_VECTOR_STAG,             \
                     JITTERLISP_VECTOR_STAG_BIT_NO)
#define JITTERLISP_VECTOR_ENCODE(_jitterlisp_untagged_vector)  \
  JITTERLISP_WITH_TAG_ADDED(_jitterlisp_untagged_vector,       \
                            JITTERLISP_VECTOR_PTAG,            \
                            JITTERLISP_VECTOR_STAG,            \
                            JITTERLISP_VECTOR_STAG_BIT_NO)
#define JITTERLISP_VECTOR_DECODE(_jitterlisp_tagged_vector)          \
  ((struct jitterlisp_vector *)                                      \
   (JITTERLISP_WITH_TAG_SUBTRACTED((_jitterlisp_tagged_vector),      \
                                   JITTERLISP_VECTOR_PTAG,           \
                                   JITTERLISP_VECTOR_STAG,           \
                                   JITTERLISP_VECTOR_STAG_BIT_NO)))




/* Globally named objects.
 * ************************************************************************** */

/* A few s-expressions, particularly some interned symbols, are important enough
   for performance reasons to be bound to global C variables.  It would be nice
   to make them constant, but this is not possible since most of them require
   heap-allocation.

   This requires them to be GC roots, which will need some work if I switch to a
   moving GC. */
extern jitterlisp_object jitterlisp_object_begin;
extern jitterlisp_object jitterlisp_object_define;
extern jitterlisp_object jitterlisp_object_if;
extern jitterlisp_object jitterlisp_object_lambda;
extern jitterlisp_object jitterlisp_object_let;
extern jitterlisp_object jitterlisp_object_let_star;
extern jitterlisp_object jitterlisp_object_quote;
extern jitterlisp_object jitterlisp_object_set_bang;
extern jitterlisp_object jitterlisp_object_while;




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
