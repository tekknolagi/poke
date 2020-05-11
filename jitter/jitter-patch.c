/* VM library: native code patching, machine-independent part.

   Copyright (C) 2017, 2019 Luca Saiu
   Updated in 2020 by Luca Saiu
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


/* Ignore the rest of this file if we have no host assembly support.
 * ************************************************************************** */

#include <jitter/jitter.h>

/* If we are not using assembly support ignore the rest of this file. */
#ifdef JITTER_ENABLE_ASSEMBLY




/* Include headers.
 * ************************************************************************** */

#include <jitter/jitter-patch.h>
#include <string.h>
#include <limits.h>

#include <jitter/machine/jitter-machine.h>




/* Machine-independent convenience functions.
 * ************************************************************************** */

enum jitter_snippet_to_patch
jitter_snippet_for_loading (const char *immediate_pointer,
                            unsigned int residual_index,
                            const char *loading_code_to_write)
{
  if (residual_index < JITTER_RESIDUAL_REGISTER_NO)
    return jitter_snippet_for_loading_register (immediate_pointer,
                                                residual_index,
                                                loading_code_to_write);
  else
    return jitter_snippet_for_loading_memory (immediate_pointer,
                                              residual_index
                                              - JITTER_RESIDUAL_REGISTER_NO,
                                              loading_code_to_write);
}

void
jitter_patch_load_immediate (char *native_code,
                             unsigned int residual_index,
                             const char *immediate_pointer,
                             enum jitter_snippet_to_patch snippet)
{
  size_t native_code_size = jitter_snippet_size (snippet);
  if (residual_index < JITTER_RESIDUAL_REGISTER_NO)
    jitter_patch_load_immediate_to_register (native_code, native_code_size,
                                             immediate_pointer, snippet);
  else
    jitter_patch_load_immediate_to_memory (native_code, native_code_size,
                                           residual_index
                                           - JITTER_RESIDUAL_REGISTER_NO,
                                           immediate_pointer,
                                           snippet);
}

void
jitter_copy_snippet (char *to_native_code, enum jitter_snippet_to_patch snippet)
{
  size_t snippet_length = jitter_snippet_size (snippet);
  const char *from_native_code = jitter_snippet_code (snippet);
  memcpy (to_native_code, from_native_code, snippet_length);
}




/* Machine-independent convenience functions.
 * ************************************************************************** */

const char*
jitter_snippet_code (enum jitter_snippet_to_patch snippet)
{
  return jitter_native_snippet_pointers [snippet];
}

size_t
jitter_snippet_size (enum jitter_snippet_to_patch snippet)
{
  return jitter_native_snippet_sizes [snippet];
}

const char*
jitter_snippet_name (enum jitter_snippet_to_patch snippet)
{
  return jitter_native_snippet_names [snippet];
}




/* Machine-independent utility functions for choosing snippets.
 * ************************************************************************** */

bool
jitter_is_negative (int64_t word)
{
  return word < 0;
}

bool
jitter_fits_in_bits_zero_extended (uint64_t word, unsigned bit_no)
{
  /* If word can be represented at all (and it can, as we received it) then it
     fits in a 64-bit word. */
  if (bit_no >= 64)
    return true;

  /* Now we can safely stop worrying about overflow.  In particular,
     this mask will have at least the most significant bit unset. */
  uint64_t limit = ((uint64_t) 1U) << bit_no;
  return word < limit;
}

bool
jitter_fits_in_bits_sign_extended (uint64_t original, unsigned bit_no)
{
  /* This is a very direct way of checking, but there is a simpler alternative:
     in two's complement with n bits I can represent every integer in
     [-(2^(n-1)), 2^(n-1)-1].  Shall I just do a range check instead of this?  I
     guess that the current solution might have the advantage of giving the
     right answer on weird non-two's-complement machines, even if the entire
     logic of patching might break on them.  I cannot really tell without
     testing. */

  /* No value fits in zero bits. */
  if (bit_no == 0)
    return false;

  /* If the given bit number is at least as large as 64 then there is no
     problem: any argument we may have received can be represented in its
     same number of bits. */
  if (bit_no >= 64)
    return true;

  /* From now on we can assume bit_no is strictly greater than zero and strictly
     less than 64.  What remains is the interesting case. */

  /* Truncate the word to bit_no bits. */
  uint64_t bitmask = (((uint64_t) 1U) << bit_no) - 1;
  uint64_t truncated = original & bitmask;

  /* Sign-extend the truncted word back to a full word, quite literally, by
     copying the sign bit from the original argument back over every bit we
     cleared by truncation. */
  uint64_t sign_extended = truncated;
  unsigned sign_bit_index = bit_no - 1;
  bool sign_bit = original & (((uint64_t) 1U) << sign_bit_index);
  unsigned i;
  for (i = sign_bit_index + 1; i < 64; i ++)
    if (sign_bit)
      sign_extended |= ((uint64_t) 1U) << i;

  /* Check whether the sign_extended truncated word is the same as the
     original. */
  return sign_extended == original;
}

#endif // #ifdef JITTER_ENABLE_ASSEMBLY
