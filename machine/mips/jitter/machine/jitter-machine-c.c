/* VM library: native code patching for MIPS .

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


#include <stdint.h>
#include <assert.h>
#include <string.h>

#include <jitter/jitter-fatal.h>

#include <jitter/jitter-patch.h>
#include <jitter/jitter-machine-common.h>

#include "jitter-machine.h"


void
jitter_invalidate_icache (char *from, size_t byte_no)
{
  /* This doesn't need to do anything on MIPS.  The GCC builtin
     __builtin___clear_cache seems sufficient, on the machines I've tested. */
}

enum jitter_snippet_to_patch
jitter_snippet_for_loading_register (const char *immediate_pointer,
                                     unsigned int residual_register_index,
                                     const char *loading_code_to_write)
{
  int32_t immediate = * (int32_t*) immediate_pointer;
  if (JITTER_FITS_IN_BITS_ZERO_EXTENDED ((uint32_t) immediate, 16))
    switch (residual_register_index)
      {
      case 0:  return jitter_snippet_load_zero_extended_16bit_to_register_0;
      case 1:  return jitter_snippet_load_zero_extended_16bit_to_register_1;
      case 2:  return jitter_snippet_load_zero_extended_16bit_to_register_2;
      case 3:  return jitter_snippet_load_zero_extended_16bit_to_register_3;
      case 4:  return jitter_snippet_load_zero_extended_16bit_to_register_4;
      case 5:  return jitter_snippet_load_zero_extended_16bit_to_register_5;
      default: jitter_fatal ("impossible");
      }
  else if (JITTER_FITS_IN_BITS_SIGN_EXTENDED (immediate, 16))
    switch (residual_register_index)
      {
      case 0:  return jitter_snippet_load_sign_extended_16bit_to_register_0;
      case 1:  return jitter_snippet_load_sign_extended_16bit_to_register_1;
      case 2:  return jitter_snippet_load_sign_extended_16bit_to_register_2;
      case 3:  return jitter_snippet_load_sign_extended_16bit_to_register_3;
      case 4:  return jitter_snippet_load_sign_extended_16bit_to_register_4;
      case 5:  return jitter_snippet_load_sign_extended_16bit_to_register_5;
      default: jitter_fatal ("impossible");
      }
  else
    switch (residual_register_index)
      {
      case 0:  return jitter_snippet_load_32bit_to_register_0;
      case 1:  return jitter_snippet_load_32bit_to_register_1;
      case 2:  return jitter_snippet_load_32bit_to_register_2;
      case 3:  return jitter_snippet_load_32bit_to_register_3;
      case 4:  return jitter_snippet_load_32bit_to_register_4;
      case 5:  return jitter_snippet_load_32bit_to_register_5;
      default: jitter_fatal ("impossible");
      }
}

enum jitter_snippet_to_patch
jitter_snippet_for_loading_memory (const char *immediate_pointer,
                                   unsigned int index,
                                   const char *loading_code_to_write)
{
  jitter_fatal ("MIPS residual memory: not implemented yet");
}

void
jitter_patch_load_immediate_to_register (char *native_code,
                                         size_t native_code_size,
                                         const char *immediate_pointer,
                                         enum jitter_snippet_to_patch snippet)
{
  uint32_t u = * (uint32_t*) immediate_pointer;
  uint16_t low = u & ((1 << 16) - 1);
  uint16_t high = u >> 16;
  /* See the comments in machine-assembly.S about offsets. */
  switch (snippet)
    {
    case jitter_snippet_load_zero_extended_16bit_to_register_0:
    case jitter_snippet_load_zero_extended_16bit_to_register_1:
    case jitter_snippet_load_zero_extended_16bit_to_register_2:
    case jitter_snippet_load_zero_extended_16bit_to_register_3:
    case jitter_snippet_load_zero_extended_16bit_to_register_4:
    case jitter_snippet_load_zero_extended_16bit_to_register_5:
    case jitter_snippet_load_sign_extended_16bit_to_register_0:
    case jitter_snippet_load_sign_extended_16bit_to_register_1:
    case jitter_snippet_load_sign_extended_16bit_to_register_2:
    case jitter_snippet_load_sign_extended_16bit_to_register_3:
    case jitter_snippet_load_sign_extended_16bit_to_register_4:
    case jitter_snippet_load_sign_extended_16bit_to_register_5:
    case jitter_snippet_load_zero_extended_16bit_to_memory:
    case jitter_snippet_load_sign_extended_16bit_to_memory:
      /* Each of these snippets is implemented by a single 32-bit instruction
         with a literal in the rightmost 16 bits; they can all be patched in
         the same way. */
#ifdef JITTER_WORDS_BIGENDIAN
      memcpy (native_code + 2, &low, 2);
#else
      memcpy (native_code + 0, &low, 2);
#endif // #ifdef JITTER_WORDS_BIGENDIAN
      break;
    case jitter_snippet_load_32bit_to_register_0:
    case jitter_snippet_load_32bit_to_register_1:
    case jitter_snippet_load_32bit_to_register_2:
    case jitter_snippet_load_32bit_to_register_3:
    case jitter_snippet_load_32bit_to_register_4:
    case jitter_snippet_load_32bit_to_register_5:
    case jitter_snippet_load_32bit_to_memory:
      /* Not much more difficult.  Here we have two 32-bit instructions to
         patch, each with a 16-bit literal in the end.  The high part comes
         first. */
#ifdef JITTER_WORDS_BIGENDIAN
      memcpy (native_code + 2, &high, 2);
      memcpy (native_code + 6, &low, 2);
#else
      memcpy (native_code + 0, &high, 2);
      memcpy (native_code + 4, &low, 2);
#endif // #ifdef JITTER_WORDS_BIGENDIAN
      break;

    default:
      jitter_fatal ("impossible");
    }
}

void
jitter_patch_load_immediate_to_memory (char *native_code,
                                       size_t native_code_size,
                                       unsigned int memory_index,
                                       const char *immediate_pointer,
                                       enum jitter_snippet_to_patch snippet)
{
  jitter_fatal ("MIPS residual memory: not implemented yet");
}

#ifdef JITTER_HAVE_PATCH_IN
enum jitter_snippet_to_patch
jitter_snippet_for_patch_in (const struct jitter_patch_in_descriptor *dp)
{
  jitter_uint patch_in_case = dp->patch_in_case;
  switch (patch_in_case)
    {
    case JITTER_PATCH_IN_CASE_FAST_BRANCH_UNCONDITIONAL:
      return jitter_snippet_jump_unconditional_28bit_pseudo_direct;

    case JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_ANY:
      return jitter_snippet_branch_conditional_18bit_offset;

    case JITTER_PATCH_IN_CASE_FAST_BRANCH_BRANCH_AND_LINK:
      return jitter_snippet_jump_and_link_28bit_pseudo_direct;

    default:
      jitter_fatal ("jitter_snippet_for_patch_in: unsupported patch-in case");
    }
}
#endif // #ifdef JITTER_HAVE_PATCH_IN

/* I keep this conditional to be able to test with patch-in disabled, by just
   commenting one line in the header. */
#ifdef JITTER_HAVE_PATCH_IN
void
jitter_patch_patch_in (char *native_code,
                       const char *immediate_pointer,
                       const struct jitter_patch_in_descriptor *descriptor,
                       enum jitter_snippet_to_patch snippet)
{
  switch (snippet)
    {
    case jitter_snippet_branch_conditional_18bit_offset:
      {
        char *jump_target = * (char**) immediate_pointer;
        char *jump_address = native_code;
        char *delay_slot_address = jump_address + 4;

        /* Check that the jump origin and target addresses are 32-bit
           aligned. */
        uint32_t jump_target_u = (uint32_t) (jitter_uint) jump_target;
        uint32_t jump_address_u = (uint32_t) (jitter_uint) jump_address;
        if (jump_address_u % 4 != 0)
          jitter_fatal ("conditional branch instruction not 32-bit aligned");
        if (jump_target_u % 4 != 0)
          jitter_fatal ("conditional branch target not 32-bit aligned");

        /* Compute the shifted branch offset (the two lowest bits, always zero,
           are not represented), and fail if doesn't fit in the instruction. */
        int64_t jump_offset = jump_target - delay_slot_address;
        int64_t jump_offset_shifted_64bit = jump_offset >> 2;
        if (! JITTER_FITS_IN_BITS_SIGN_EXTENDED (jump_offset_shifted_64bit, 16))
          jitter_fatal ("conditional branch target too far");
        uint16_t jump_offset_shifted = jump_offset_shifted_64bit;

        /* Compute the patched instruction, by replacing the shifted offset into
           the old instruction. */
        uint32_t old_instruction = * (uint32_t*) native_code;
        uint32_t instruction
          = (old_instruction & ~ 0xffff) | jump_offset_shifted;

        /* Write the patched instruction in place of the original instruction. */
        memcpy (native_code, & instruction, 4);
        break;
      }

    case jitter_snippet_jump_unconditional_28bit_pseudo_direct:
    case jitter_snippet_jump_and_link_28bit_pseudo_direct:
      {
        char *jump_target = * (char**) immediate_pointer;
        char *jump_address = native_code;

        uint32_t jump_target_u = (uint32_t) (jitter_uint) jump_target;
        uint32_t jump_address_u = (uint32_t) (jitter_uint) jump_address;

        /* Check that the jump origin and target addresses are 32-bit
           aligned. */
        if (jump_address_u % 4 != 0)
          jitter_fatal ("j instruction not 32-bit aligned");
        if (jump_target_u % 4 != 0)
          jitter_fatal ("j instruction target not 32-bit aligned");

        /* Do a logical right shift of the two address, so as to ignore the two
           least significatn zero bits. */
        jump_target_u >>= 2;
        jump_address_u >>= 2;

#define JITTER_INSTRUCTION_INDEX(address_as_uint32) \
  (address_as_uint32 & ((1u << 26) - 1u))
#define JITTER_REGION(address_as_uint32) \
  (address_as_uint32 & ~ ((1u << 26) - 1u))

        /* Check that the six most significant bits of the jump targets are the
           same as the six most significant target of the jumping
           instruction. */
        if (JITTER_REGION(jump_address_u) != JITTER_REGION(jump_target_u))
          jitter_fatal ("can't jump out of the current 256MB region");

        /* Encode the instruction. */
        uint32_t unscaled_opcode
          = ((snippet == jitter_snippet_jump_unconditional_28bit_pseudo_direct)
             ? 2u
             : 3u);
        uint32_t instruction
          = (  (unscaled_opcode << 26)                    /* opcode. */
             | JITTER_INSTRUCTION_INDEX(jump_target_u));  /* instruction index. */

#undef JITTER_INSTRUCTION_INDEX
#undef JITTER_REGION

        /* Write the instruction to memory. */
        memcpy (native_code, & instruction, 4);
        //* (uint32_t *) jump_address = instruction; // FIXME: would 32-bit alignment suffice for MIPS64 as well?
        break;
      }
    default:
      jitter_fatal ("jitter_patch_patch_in: unsupported snippet %li",
                    (long) snippet);
    }
}
#endif // #ifdef JITTER_HAVE_PATCH_IN
