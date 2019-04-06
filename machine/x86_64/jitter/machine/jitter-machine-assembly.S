/* VM library: assembly support for x86_64 .

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


/* Include the architecture-dependent CPP macro definitions. */
#include <jitter/machine/jitter-machine.h>

/* Include the architecture-independent Gas macro definitions. */
#include <jitter/jitter-machine-common.S>

.text

/* Here come the actual routine definitions, containing the code to be copied
   and patched.  Notice that the order matters, and the calls to jitter_routine
   here must follow the same order as the enum jitter_routine_to_patch cases in
   native.h . */
jitter_arrays .quad

/* Load zero into a register by xor-ing it with itself.  This assembles to
   something like
     31 db   # xor %ebx, %ebx
   , but in this case the code doesn't actually need patching.  I can use
   a 32-bit xorl to load zero to the high half as well. */
jitter_routine load_0_to_64bit_residual_register_0, \
           <xorl JITTER_RESIDUAL_REGISTER_0_32BIT, JITTER_RESIDUAL_REGISTER_0_32BIT>
jitter_routine load_0_to_64bit_residual_register_1, \
           <xorl JITTER_RESIDUAL_REGISTER_1_32BIT, JITTER_RESIDUAL_REGISTER_1_32BIT>
jitter_routine load_0_to_64bit_residual_register_2, \
           <xorl JITTER_RESIDUAL_REGISTER_2_32BIT, JITTER_RESIDUAL_REGISTER_2_32BIT>
jitter_routine load_0_to_64bit_residual_register_3, \
           <xorl JITTER_RESIDUAL_REGISTER_3_32BIT, JITTER_RESIDUAL_REGISTER_3_32BIT>

/* Load -1 into a 64-bit register by or-ing it with a one-byte -1 literal.
   This assembles to something like
     48 83 cb ff   # orq $-1, %rbx
   , but in this case the code doesn't actually need patching. */
jitter_routine load_minus_1_to_64bit_residual_register_0, \
           <orq $-1, JITTER_RESIDUAL_REGISTER_0>
jitter_routine load_minus_1_to_64bit_residual_register_1, \
           <orq $-1, JITTER_RESIDUAL_REGISTER_1>
jitter_routine load_minus_1_to_64bit_residual_register_2, \
           <orq $-1, JITTER_RESIDUAL_REGISTER_2>
jitter_routine load_minus_1_to_64bit_residual_register_3, \
           <orq $-1, JITTER_RESIDUAL_REGISTER_3>

/* Load a 64-bit literal into a register.  These assemble to something like
     48 bb 00 00 00 00 00 00 00 00   # movabsq $0x0, %rbx
   , where the zeroes are the literal constant to patch over.  The literal
   to be patched over is always at the same offset from the end, independently
   from the destination register.
   It goes without saying that these instructions are huge.  Even if on modern
   CPUs they don't take long to execute, according to Agner Fog they can become
   a bottleneck at decoding time, so they are better avoided.  Whenever the
   literal fits in 32 bits, either zero-extended or sign-extended, there is
   a better solution. */
jitter_routine set_64bit_residual_register_0,       \
           <movabsq $0, JITTER_RESIDUAL_REGISTER_0>
jitter_routine set_64bit_residual_register_1,       \
           <movabsq $0, JITTER_RESIDUAL_REGISTER_1>
jitter_routine set_64bit_residual_register_2,       \
           <movabsq $0, JITTER_RESIDUAL_REGISTER_2>
jitter_routine set_64bit_residual_register_3,       \
           <movabsq $0, JITTER_RESIDUAL_REGISTER_3>

/* Load a 64-bit literal into residual argument memory at a fixed offset.
   This assembles to something like:
     49 bd 00 00 00 00 00 00 00 00   # movabsq $0x0, %r13
     4d 89 6c 24 08                  # movq    %r13, 0x8(%r12)
   In the movq instruction the trailing 0x08 is the offset in bytes from
   The Array base, which is supposed to be patched in.  This format
   can be used for offsets up to 0x7f (127) included, therefore allowing
   for 16 + JITTER_RESIDUAL_REGISTER_NO residual arguments (but passing 0 as
   an offset here is inefficient: in that case the routine below should
   be used instead) -- which should be plenty.
   Since the instruction encoding is messy and the total length may change
   with a different choice of registers, it is better to find patch locations
   by looking for the first 0x0 byte, and taking the last byte. */
jitter_routine set_64bit_residual_memory_small_offset,                \
           <movabsq $0, JITTER_SCRATCH_REGISTER>,                     \
           <movq JITTER_SCRATCH_REGISTER, 0x8(JITTER_BASE_REGISTER)>

/* Just like set_64bit_residual_memory_small_offset, with zero offset.
   This assembles to something like:
     49 bd 00 00 00 00 00 00 00 00   # movabsq $0x0, %r13
     4d 89 2c 24                     # movq    %r13, 0x0(%r12)
   This is one byte smaller than set_64bit_residual_memory_small_offset .
   See the comment above about patching. */
jitter_routine set_64bit_residual_memory_zero_offset,               \
           <movabsq $0, JITTER_SCRATCH_REGISTER>,                   \
           <movq JITTER_SCRATCH_REGISTER, 0(JITTER_BASE_REGISTER)>

/* Just like set_64bit_residual_memory_small_offset, but allowing for
   larger offsets.  This assembles to something like:
     49 bd 00 00 00 00 00 00 00 00   # movabsq $0x0, %r13
     4d 89 ac 24 ff 00 00 00         # movq    %r13, 0xff(%r12)
   The last four bytes are the offset from the residual base.
   See the comment above about patching. */
jitter_routine set_64bit_residual_memory_big_offset,                   \
           <movabsq $0, JITTER_SCRATCH_REGISTER>,                      \
           <movq JITTER_SCRATCH_REGISTER, 0xff(JITTER_BASE_REGISTER)>

/* Load a 32-bit literal (or a 64-bit literal which fits zero-extended in
   32 bits -- when writing to the low half of a 64-bit register x86_64 zeroes
   its high half) into a register.
   These assemble to something like
     bb 00 00 00 00   # movl $0x0, %ebx
   , where the zeroes are the literal constant to patch over.  The literal
   to be patched over is always at the same offset *from the end*,
   independently from the destination register; however using the "new"
   registers requires a so-called prefix before the actual opcode, making
   the instruction longer.
   For example:
     41 bd 00 00 00 00   # movl $0x0, %r13d */
jitter_routine set_32bit_residual_register_0,           \
           <movl $0, JITTER_RESIDUAL_REGISTER_0_32BIT>
jitter_routine set_32bit_residual_register_1,           \
           <movl $0, JITTER_RESIDUAL_REGISTER_1_32BIT>
jitter_routine set_32bit_residual_register_2,           \
           <movl $0, JITTER_RESIDUAL_REGISTER_2_32BIT>
jitter_routine set_32bit_residual_register_3,           \
           <movl $0, JITTER_RESIDUAL_REGISTER_3_32BIT>

/* Sign-extend a 32-bit literal into a 64-bit register.
   These assemble to something like
     48 c7 c0 00 00 00 00  # movq $0x0,%rax
   , where the zeroes are the literal constant to patch over.  The literal
   to be patched over is always at the same offset *from the end*; see the
   comment above. */
jitter_routine set_64bit_sign_extended_residual_register_0,  \
           <movq $0, JITTER_RESIDUAL_REGISTER_0>
jitter_routine set_64bit_sign_extended_residual_register_1,  \
           <movq $0, JITTER_RESIDUAL_REGISTER_1>
jitter_routine set_64bit_sign_extended_residual_register_2,  \
           <movq $0, JITTER_RESIDUAL_REGISTER_2>
jitter_routine set_64bit_sign_extended_residual_register_3,  \
           <movq $0, JITTER_RESIDUAL_REGISTER_3>

/* Load a 32-bit literal (zero-extended) into residual argument memory at
   a fixed offset.  This assembles to something like:
     49 c7 44 24 08 00 00 00 00   # movq   $0x0, 0x8(%r12)
   In the movq instruction the 0x08 byte before the 32-bit literal is
   the offset in bytes from The Array base, which is also supposed to
   be patched over.  This format can be used for offsets up to 0x7f (127)
   included, therefore allowing for 16 + JITTER_RESIDUAL_REGISTER_NO residual
   arguments (but passing 0 as an offset here is inefficient: in that case
   the routine below should be used instead) -- which should be plenty.
   The instruction length changes with different choices of registers,
   but computing the offsets from the end is reliable and easy.  No
   assumption should be made about the total code length. */
jitter_routine set_32bit_residual_memory_small_offset,  \
           <movq $0, 0x8(JITTER_BASE_REGISTER)>

/* Just like set_32bit_residual_memory_small_offset, with zero offset.
   This assembles to something like:
     49 c7 04 24 00 00 00 00   # movq $0x0, 0x0(%r12)
   This is one byte smaller than set_32bit_residual_memory_small_offset .
   See the comment above about patching. */
jitter_routine set_32bit_residual_memory_zero_offset,  \
           <movq $0, 0(JITTER_BASE_REGISTER)>

/* Just like set_32bit_residual_memory_small_offset, but allowing for
   larger offsets.  This assembles to something like:
     49 c7 84 24 ff 00 00 00 00 00 00 00   # movq $0x0, 0xff(%r12)
   The last four bytes are the literal to be stored, and the four bytes
   before that are the 32-bit offset from the residual base.
   See the comment above about patching. */
jitter_routine set_32bit_residual_memory_big_offset,  \
           <movq $0, 0xff(JITTER_BASE_REGISTER)>

/* Branch relative (to the end of the jmp instruction) with a 32-bit signed
   offset.  This assembles to something like:
     e9 00 00 00 00   #    jmp L
                      # L:
   An offset of zero, like in the example, would jump to the address
   immediately following the jmp instruction. */
jitter_routine jump_unconditional_32bit_offset,  \
           <jmp.d32 0>

/* Branch conditional relative (to the end of the branch instruction) with
   a 32-bit signed offset.  This assembles to something like:
     0f 8? 00 00 00 00   # j? L
   For example:
     0f 87 00 00 00 00   # ja L
     0f 8f 00 00 00 00   # jg L
   An offset of zero, like in the examples, would jump to the address
   immediately following the conditional branch instruction. */
jitter_routine jump_on_zero_32bit_offset,  \
           <jz.d32 0>
jitter_routine jump_on_nonzero_32bit_offset,  \
           <jnz.d32 0>
jitter_routine jump_on_sign_32bit_offset,  \
           <js.d32 0>
jitter_routine jump_on_nonsign_32bit_offset,  \
           <jns.d32 0>
jitter_routine jump_on_equal_32bit_offset,  \
           <je.d32 0>
jitter_routine jump_on_notequal_32bit_offset,  \
           <jne.d32 0>
jitter_routine jump_on_above_32bit_offset,  \
           <ja.d32 0>
jitter_routine jump_on_notbelow_32bit_offset,  \
           <jae.d32 0>
jitter_routine jump_on_below_32bit_offset,  \
           <jb.d32 0>
jitter_routine jump_on_notabove_32bit_offset,  \
           <jbe.d32 0>
jitter_routine jump_on_less_32bit_offset,  \
           <jl.d32 0>
jitter_routine jump_on_notgreater_32bit_offset,  \
           <jle.d32 0>
jitter_routine jump_on_greater_32bit_offset,  \
           <jg.d32 0>
jitter_routine jump_on_notless_32bit_offset,  \
           <jge.d32 0>

/* Call a procedure whose beginning is given relative (to the end of the call
   instruction) with a 32-bit signed offset.  This assembles to something like:
     e8 78 56 34 12   # callq  0x12345678
*/
jitter_routine call_32bit_offset,    \
           <callq 0x12345678>
