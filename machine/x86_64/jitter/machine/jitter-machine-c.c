/* VM library: native code patching for x86_64 .

   Copyright (C) 2017, 2019 Luca Saiu
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


//#include <config.h>

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
  /* This doesn't need to do anything on x86_64. */
}

enum jitter_routine_to_patch
jitter_routine_for_loading_register (const char *immediate_pointer,
                                     unsigned int residual_register_index,
                                     const char *loading_code_to_write)
{
  int64_t immediate = * (int64_t*) immediate_pointer;
  if (immediate == 0)
    switch (residual_register_index)
      {
      case 0:  return jitter_routine_load_0_to_64bit_residual_register_0;
      case 1:  return jitter_routine_load_0_to_64bit_residual_register_1;
      case 2:  return jitter_routine_load_0_to_64bit_residual_register_2;
      case 3:  return jitter_routine_load_0_to_64bit_residual_register_3;
      default: jitter_fatal ("impossible A");
      }
  if (immediate == -1)
    switch (residual_register_index)
      {
      case 0:  return jitter_routine_load_minus_1_to_64bit_residual_register_0;
      case 1:  return jitter_routine_load_minus_1_to_64bit_residual_register_1;
      case 2:  return jitter_routine_load_minus_1_to_64bit_residual_register_2;
      case 3:  return jitter_routine_load_minus_1_to_64bit_residual_register_3;
      default: jitter_fatal ("impossible B");
      }
  else if (jitter_fits_in_bits_zero_extended (immediate, 32))
    switch (residual_register_index)
      {
      case 0:  return jitter_routine_set_32bit_residual_register_0;
      case 1:  return jitter_routine_set_32bit_residual_register_1;
      case 2:  return jitter_routine_set_32bit_residual_register_2;
      case 3:  return jitter_routine_set_32bit_residual_register_3;
      default: jitter_fatal ("impossible C");
      }
  else if (jitter_fits_in_bits_sign_extended (immediate, 32))
    switch (residual_register_index)
      {
      case 0:  return jitter_routine_set_64bit_sign_extended_residual_register_0;
      case 1:  return jitter_routine_set_64bit_sign_extended_residual_register_1;
      case 2:  return jitter_routine_set_64bit_sign_extended_residual_register_2;
      case 3:  return jitter_routine_set_64bit_sign_extended_residual_register_3;
      default: jitter_fatal ("impossible C");
      }
  else
    switch (residual_register_index)
      {
      case 0:  return jitter_routine_set_64bit_residual_register_0;
      case 1:  return jitter_routine_set_64bit_residual_register_1;
      case 2:  return jitter_routine_set_64bit_residual_register_2;
      case 3:  return jitter_routine_set_64bit_residual_register_3;
      default: jitter_fatal ("impossible D");
      }
}

enum jitter_routine_to_patch
jitter_routine_for_loading_memory (const char *immediate_pointer,
                                   unsigned int index,
                                   const char *loading_code_to_write)
{
  if (index >= 16)
    jitter_fatal ("you have an awful lot of memory residuals: not implemented yet");

  /* FIXME: support the other cases as well.  This is good as a fallback case,
     as an inefficient but quite general routine.  The only case it doesn't
     cover is index >= 16 . */
  return jitter_routine_set_64bit_residual_memory_small_offset;
}

void
jitter_patch_load_immediate_to_register (char *native_code,
                                         size_t native_code_size,
                                         const char *immediate_pointer,
                                         enum jitter_routine_to_patch routine)
{
  /* See the comments in machine-assembly.S about offsets. */
  switch (routine)
    {
    case jitter_routine_load_0_to_64bit_residual_register_0:
    case jitter_routine_load_0_to_64bit_residual_register_1:
    case jitter_routine_load_0_to_64bit_residual_register_2:
    case jitter_routine_load_0_to_64bit_residual_register_3:
    case jitter_routine_load_minus_1_to_64bit_residual_register_0:
    case jitter_routine_load_minus_1_to_64bit_residual_register_1:
    case jitter_routine_load_minus_1_to_64bit_residual_register_2:
    case jitter_routine_load_minus_1_to_64bit_residual_register_3:
      /* Do nothing: these routines do not need patching. */
      break;

    case jitter_routine_set_32bit_residual_register_0:
    case jitter_routine_set_32bit_residual_register_1:
    case jitter_routine_set_32bit_residual_register_2:
    case jitter_routine_set_32bit_residual_register_3:
    case jitter_routine_set_64bit_sign_extended_residual_register_0:
    case jitter_routine_set_64bit_sign_extended_residual_register_1:
    case jitter_routine_set_64bit_sign_extended_residual_register_2:
    case jitter_routine_set_64bit_sign_extended_residual_register_3:
      /* Since x86_64 is little-endian I can truncate a 64-bit value to its
         lower 32 bits by just taking its *first* four bytes.
         The immediate is 32-bit, coming at the end of the instruction, for
         both the movl and the movq case. */
      memcpy (native_code + native_code_size - 4, immediate_pointer, 4);
      break;

    case jitter_routine_set_64bit_residual_register_0:
    case jitter_routine_set_64bit_residual_register_1:
    case jitter_routine_set_64bit_residual_register_2:
    case jitter_routine_set_64bit_residual_register_3:
      memcpy (native_code + native_code_size - 8, immediate_pointer, 8);
      break;

    default:
      jitter_unimplemented ("jitter_patch_load_immediate_to_register");
    }
}

void
jitter_patch_load_immediate_to_memory (char *native_code,
                                       size_t native_code_size,
                                       unsigned int memory_index,
                                       const char *immediate_pointer,
                                       enum jitter_routine_to_patch routine)
{
  switch (routine)
    {
    case jitter_routine_set_64bit_residual_memory_small_offset:
      {
        char *first_zero = memchr (native_code, 0x0, native_code_size);
        /* Just to be sure that I'm patching the first instruction at the right
           place, since its encoding is complicated, check:
           - that I actually found a 0x0 byte;
           - that the 8 bytes I'm about to overwrite are actually all 0x0;
           - and that the byte right after those (where the next instruction
             begins) is not. */
        uint64_t zeroes = * (uint64_t*) first_zero;
        assert (first_zero != NULL);
        assert (zeroes == 0L);
        assert (first_zero [8] != 0);

        /* Patch the literal in. */
        memcpy (first_zero, immediate_pointer, 8);

        /* Patch the memory offset in. */
        unsigned char offset = memory_index * 8;
        memcpy (native_code + native_code_size - 1, &offset, 1);
        break;
      }
    default:
      jitter_unimplemented ("jitter_patch_load_immediate_to_memory");
    }
}

/* I keep this conditional to be able to test with patch-in disabled, by just
   commenting one line in the header. */
#ifdef JITTER_HAVE_PATCH_IN
enum jitter_routine_to_patch
jitter_routine_for_patch_in (const struct jitter_patch_in_descriptor *dp)
{
  jitter_uint patch_in_case = dp->patch_in_case;
  switch (patch_in_case)
    {
    case JITTER_PATCH_IN_CASE_FAST_BRANCH_UNCONDITIONAL:
      return jitter_routine_jump_unconditional_32bit_offset;

    case JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_ZERO:
      return jitter_routine_jump_on_zero_32bit_offset;
    case JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NONZERO:
      return jitter_routine_jump_on_nonzero_32bit_offset;
    case JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NEGATIVE:
      return jitter_routine_jump_on_sign_32bit_offset;
    case JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NONNEGATIVE:
      return jitter_routine_jump_on_nonsign_32bit_offset;
    case JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_EQUAL:
      return jitter_routine_jump_on_equal_32bit_offset;
    case JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NOTEQUAL:
      return jitter_routine_jump_on_notequal_32bit_offset;
    case JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_LESS_SIGNED:
      return jitter_routine_jump_on_less_32bit_offset;
    case JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_LESS_UNSIGNED:
      return jitter_routine_jump_on_below_32bit_offset;
    case JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NOTGREATER_SIGNED:
      return jitter_routine_jump_on_notgreater_32bit_offset;
    case JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NOTGREATER_UNSIGNED:
      return jitter_routine_jump_on_notabove_32bit_offset;
    case JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_GREATER_SIGNED:
      return jitter_routine_jump_on_greater_32bit_offset;
    case JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_GREATER_UNSIGNED:
      return jitter_routine_jump_on_above_32bit_offset;
    case JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NOTLESS_SIGNED:
      return jitter_routine_jump_on_notless_32bit_offset;
    case JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NOTLESS_UNSIGNED:
      return jitter_routine_jump_on_notbelow_32bit_offset;

    case JITTER_PATCH_IN_CASE_FAST_BRANCH_BRANCH_AND_LINK:
      return jitter_routine_call_32bit_offset;

    default:
      jitter_fatal ("jitter_routine_for_patch_in: unsupported patch-in case %li",
                    (long) patch_in_case);
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
                       enum jitter_routine_to_patch routine)
{
  char *jump_target = * (char**) immediate_pointer;

  switch (routine)
    {
    case jitter_routine_jump_unconditional_32bit_offset:
      {
        /* On x86_64 jump targets are encoded as signed distances from the *end*
           of the jumping instruction--in this case in 32 bits.  This jmp
           instruction takes 5 bytes, of which the last 4 are the offset. */
        int32_t offset = jump_target - (native_code + 5);
        memcpy (native_code + 1, & offset, sizeof (offset));
        break;
      }
    case jitter_routine_jump_on_zero_32bit_offset:
    case jitter_routine_jump_on_nonzero_32bit_offset:
    case jitter_routine_jump_on_sign_32bit_offset:
    case jitter_routine_jump_on_nonsign_32bit_offset:
    case jitter_routine_jump_on_equal_32bit_offset:
    case jitter_routine_jump_on_notequal_32bit_offset:
    case jitter_routine_jump_on_less_32bit_offset:
    case jitter_routine_jump_on_below_32bit_offset:
    case jitter_routine_jump_on_notgreater_32bit_offset:
    case jitter_routine_jump_on_notabove_32bit_offset:
    case jitter_routine_jump_on_greater_32bit_offset:
    case jitter_routine_jump_on_above_32bit_offset:
    case jitter_routine_jump_on_notless_32bit_offset:
    case jitter_routine_jump_on_notbelow_32bit_offset:
      {
        /* On x86_64 jump targets are encoded as signed distances from the *end*
           of the jumping instruction--in this case in 32 bits.  This
           conditional jump instruction takes 6 bytes, of which the last 4 are
           the offset. */
        int32_t offset = jump_target - (native_code + 6);
        memcpy (native_code + 2, & offset, sizeof (offset));
        break;
      }

    case jitter_routine_call_32bit_offset:
      {
        /* On x86_64 jump targets are encoded as signed distances from the *end*
           of the jumping instruction--in this case in 32 bits.  This call
           instruction takes 5 bytes, of which the last 4 are the offset. */
        int32_t offset = jump_target - (native_code + 5);
        memcpy (native_code + 1, & offset, sizeof (offset));
        break;
      }

    default:
      jitter_fatal ("jitter_patch_patch_in: unsupported routine %li",
                    (long) routine);
    }
}
#endif // #ifdef JITTER_HAVE_PATCH_IN
