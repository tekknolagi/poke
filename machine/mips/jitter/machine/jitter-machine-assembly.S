/* VM library: assembly support for MIPS .

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

/* Here I use $1/$at explicitly; in this kind of low-level file, where I care
   about the encoding of each assembly instruction, I will certainly not use
   pseudo-instructions expanding to hidden uses of $1. */
.set noat

/* Here come the actual snippet definitions, containing the code to be copied
   and patched.  Notice that the order matters, and the calls to jitter_snippet
   here must follow the same order as the enum jitter_snippet_to_patch cases in
   native.h . */
jitter_arrays .long

/* Load a zero-extended 16-bit literal into a 32-bit register.  This
   assembles to something like
     34 02 ff ff   # ori $2, $0, 0xffff
   where the rightmost 16 bits, here all ones, are the literal. */
jitter_snippet load_zero_extended_16bit_to_register_0, \
           <ori JITTER_RESIDUAL_REGISTER_0, $0, 0>
jitter_snippet load_zero_extended_16bit_to_register_1, \
           <ori JITTER_RESIDUAL_REGISTER_1, $0, 0>
jitter_snippet load_zero_extended_16bit_to_register_2, \
           <ori JITTER_RESIDUAL_REGISTER_2, $0, 0>
jitter_snippet load_zero_extended_16bit_to_register_3, \
           <ori JITTER_RESIDUAL_REGISTER_3, $0, 0>
jitter_snippet load_zero_extended_16bit_to_register_4, \
           <ori JITTER_RESIDUAL_REGISTER_4, $0, 0>
jitter_snippet load_zero_extended_16bit_to_register_5, \
           <ori JITTER_RESIDUAL_REGISTER_5, $0, 0>

/* Load a sign-extended 16-bit literal into a 32-bit register.  This
   assembles to something like
     24 02 00 00   # addiu $2, $0, 0x0000
   where the rightmost 16 bits, here zero, are the literal. */
jitter_snippet load_sign_extended_16bit_to_register_0, \
           <addiu JITTER_RESIDUAL_REGISTER_0, $0, 0>
jitter_snippet load_sign_extended_16bit_to_register_1, \
           <addiu JITTER_RESIDUAL_REGISTER_1, $0, 0>
jitter_snippet load_sign_extended_16bit_to_register_2, \
           <addiu JITTER_RESIDUAL_REGISTER_2, $0, 0>
jitter_snippet load_sign_extended_16bit_to_register_3, \
           <addiu JITTER_RESIDUAL_REGISTER_3, $0, 0>
jitter_snippet load_sign_extended_16bit_to_register_4, \
           <addiu JITTER_RESIDUAL_REGISTER_4, $0, 0>
jitter_snippet load_sign_extended_16bit_to_register_5, \
           <addiu JITTER_RESIDUAL_REGISTER_5, $0, 0>

/* Load a 32-bit literal into a 32-bit register.  This assembles to
   something like
     3c 10 ab cd   # lui $16,      0xabcd
     36 10 12 23   # ori $16, $16, 0x1234
   where the first instruction loads the high half and the second the low
   half.  Both halves are encoded in the rightmost 16 bits of each
   instruction.  The 32-bit literal in this example is 0xabcd1234 . */
jitter_snippet load_32bit_to_register_0,                               \
           <lui JITTER_RESIDUAL_REGISTER_0, 0>,                        \
           <ori JITTER_RESIDUAL_REGISTER_0, JITTER_RESIDUAL_REGISTER_0, 0>
jitter_snippet load_32bit_to_register_1,                               \
           <lui JITTER_RESIDUAL_REGISTER_1, 0>,                        \
           <ori JITTER_RESIDUAL_REGISTER_1, JITTER_RESIDUAL_REGISTER_1, 0>
jitter_snippet load_32bit_to_register_2,                               \
           <lui JITTER_RESIDUAL_REGISTER_2, 0>,                        \
           <ori JITTER_RESIDUAL_REGISTER_2, JITTER_RESIDUAL_REGISTER_2, 0>
jitter_snippet load_32bit_to_register_3,                               \
           <lui JITTER_RESIDUAL_REGISTER_3, 0>,                        \
           <ori JITTER_RESIDUAL_REGISTER_3, JITTER_RESIDUAL_REGISTER_3, 0>
jitter_snippet load_32bit_to_register_4,                               \
           <lui JITTER_RESIDUAL_REGISTER_4, 0>,                        \
           <ori JITTER_RESIDUAL_REGISTER_4, JITTER_RESIDUAL_REGISTER_4, 0>
jitter_snippet load_32bit_to_register_5,                               \
           <lui JITTER_RESIDUAL_REGISTER_5, 0>,                        \
           <ori JITTER_RESIDUAL_REGISTER_5, JITTER_RESIDUAL_REGISTER_5, 0>

/* FIXME: these are not implemented yet. */
jitter_snippet load_zero_extended_16bit_to_memory,  \
           <nop>
jitter_snippet load_sign_extended_16bit_to_memory,  \
           <nop>
jitter_snippet load_32bit_to_memory,  \
           <nop>

/* Jump to an absolute address whose six most significant bits are the same
   as the ones in the address of the jumping instruction, the next 26 are
   specified as an immediate, and the last two are zero.  The opcode takes
   the most significant 6 bits, and its value is 2.
     08000000   # j to the first address in the same region.
     00200825   # or $1, $1, $0
   Here instruction encodings are shown big-endian.
   The second instruction is a nop for the delay slot.  Some old Loongson2F
   version has a bug for which the recommended workaround is using this
   encoding for a nop, rather than the traditional
     00000000   # sll $0, $0, 0x0
   Of course the observable behavior is the same. */
jitter_snippet jump_unconditional_28bit_pseudo_direct,  \
           <.long 0x8000000>,                           \
           <JITTER_MIPS_NOP>

/* Identical to jump_unconditional_28bit_pseudo_direct , except that the
   jumping instruction also stores the return address (the adress of the
   instruction following the delay slot) in $31.  The instruction encoding
   is the same, except that the opcode is 3 instead of 2.
     0c000000   # jal to the first address in the same region.
     00200825   # or $1, $1, $0 */
jitter_snippet jump_and_link_28bit_pseudo_direct,  \
           <.long 0xc000000>,                      \
           <JITTER_MIPS_NOP>

/* This snippet does nothing and takes zero bytes: all the complexity is in the
   inline asm and in the patch-in replacement code, which only replaces the
   branch offset without touching the rest of the instruction.  This solution is
   sensible on MIPS, where conditional branch instructions mention register
   numbers instead of flags, and the register numbers are easy to generate in
   inline asm -- but much less here in assembly, where we don't know about GCC's
   register assignment. */
jitter_snippet branch_conditional_18bit_offset,  \
        <>
