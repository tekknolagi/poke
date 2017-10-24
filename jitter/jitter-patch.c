/* VM library: native code patching, machine-independent part.

   Copyright (C) 2017 Luca Saiu
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

#include <jitter/jitter-config.h>

/* If there is no assembly support ignore the rest of this file. */
#ifdef JITTER_HAS_ASSEMBLY




/* Include headers.
 * ************************************************************************** */

#include <jitter/jitter.h>
#include <jitter/jitter-patch.h>
#include <string.h>
#include <limits.h>

#include <jitter/machine/jitter-machine.h>




/* Machine-independent convenience functions.
 * ************************************************************************** */

enum jitter_routine_to_patch
jitter_routine_for_loading (const char *immediate_pointer,
                            unsigned int residual_index,
                            const char *loading_code_to_write)
{
  if (residual_index < JITTER_RESIDUAL_REGISTER_NO)
    return jitter_routine_for_loading_register (immediate_pointer,
                                                residual_index,
                                                loading_code_to_write);
  else
    return jitter_routine_for_loading_memory (immediate_pointer,
                                              residual_index
                                              - JITTER_RESIDUAL_REGISTER_NO,
                                              loading_code_to_write);
}

void
jitter_patch_load_immediate (char *native_code,
                             unsigned int residual_index,
                             const char *immediate_pointer,
                             enum jitter_routine_to_patch routine)
{
  size_t native_code_size = jitter_routine_size (routine);
  if (residual_index < JITTER_RESIDUAL_REGISTER_NO)
    jitter_patch_load_immediate_to_register (native_code, native_code_size,
                                             immediate_pointer, routine);
  else
    jitter_patch_load_immediate_to_memory (native_code, native_code_size,
                                           residual_index
                                           - JITTER_RESIDUAL_REGISTER_NO,
                                           immediate_pointer,
                                           routine);
}

void
jitter_copy_routine (char *to_native_code, enum jitter_routine_to_patch routine)
{
  size_t routine_length = jitter_routine_size (routine);
  const char *from_native_code = jitter_routine_code (routine);
  memcpy (to_native_code, from_native_code, routine_length);
}




/* Machine-independent convenience functions.
 * ************************************************************************** */

const char*
jitter_routine_code (enum jitter_routine_to_patch routine)
{
  return jitter_native_routine_pointers [routine];
}

size_t
jitter_routine_size (enum jitter_routine_to_patch routine)
{
  return jitter_native_routine_sizes [routine];
}

const char*
jitter_routine_name (enum jitter_routine_to_patch routine)
{
  return jitter_native_routine_names [routine];
}




/* Machine-independent utility functions for choosing routines.
 * ************************************************************************** */

bool
jitter_is_negative (int64_t word)
{
  return word < 0;
}

bool
jitter_fits_in_bits_zero_extended (uint64_t word, unsigned bit_no)
{
  /* If word can be represented at all then it fits in a word. */
  if (bit_no >= SIZEOF_VOID_P * CHAR_BIT)
    return true;

  /* Now we can safely stop worrying about overflow.  In particular,
     this mask will have at least the most significant bit unset. */
  uint64_t limit = ((uint64_t) 1U) << bit_no;
  return word < limit;
}

bool
jitter_fits_in_bits_sign_extended (uint64_t original, unsigned bit_no)
{
  /* FIXME: in two's complement with n bits I can represent every integer in
     [-(2^(n-1)), 2^(n-1)-1].  Can I just do a range check instead of this? I
     suspect not if I want this to work in general, without making hypotheses
     over the machine word size.  I should think more about this. */

  /* No value fits in zero bits. */
  if (bit_no == 0)
    return false;

  /* If the given bit number is at least as large as the number of bits in a
     word then there is no problem. */
  if (bit_no >= JITTER_BITS_PER_WORD)
    return true;

  /* From now on we can assume bit_no is greater than zero and less than the
     number of bits in a word. */

  /* Truncate the word to bit_no bits. */
  uint64_t bitmask = (((uint64_t) 1U) << bit_no) - 1;
  uint64_t truncated = original & bitmask;

  /* Sign-extend the truncted word back to a full word. */
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

#endif // #ifdef JITTER_HAS_ASSEMBLY
