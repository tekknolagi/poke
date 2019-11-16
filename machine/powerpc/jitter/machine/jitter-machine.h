/* VM library: PowerPC definitions, to be included from both C and assembly.

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


#ifndef JITTER_NATIVE_MACHINE_H_ /* One macro suffices for all architectures. */
#define JITTER_NATIVE_MACHINE_H_


/* Assembler syntax.
 * ************************************************************************** */

/* How to introduce comments in assembly on PowerPC. */
#define JITTER_ASM_COMMENT_PREFIX "# "




/* Debugging.
 * ************************************************************************** */

/* Expand to an inline assembly template generating a nop instruction containing
   the given literal as an argument. */
#define _JITTER_ASM_DEBUGGING_NOP(integer_literal_as_string)  \
  "xori %%r0, %%r0, " integer_literal_as_string "\n\t"        \
  "xori %%r0, %%r0, " integer_literal_as_string "\n\t"

/* Expand to a native machine code snippet causing a trap, as a string literal
   in a syntax suitable for extended inline asm. */
#define _JITTER_ASM_CRASH                                                   \
  /* Return from interrupt.  This will cause an exception in user mode, of  \
     a kind not usually seen. */                                            \
  "rfi"




/* Computed goto.
 * ************************************************************************** */

/* Computed goto implemented with inline asm.  See the comments about
   JITTER_ASM_COMPUTED_GOTO_TEMPLATE, and
   JITTER_ASM_COMPUTED_GOTO_INPUT_CONSTRAINT and
   JITTER_ASM_COMPUTED_GOTO_CLOBBERS in jitter/jitter-executor.h . */
#define JITTER_ASM_COMPUTED_GOTO_TEMPLATE  \
  "mtctr %[_jitter_the_target]\n\t"        \
  "bctr"
#define JITTER_ASM_COMPUTED_GOTO_INPUT_CONSTRAINT  \
  "r"
#define JITTER_ASM_COMPUTED_GOTO_CLOBBERS \
  "ctr"




/* Reserved registers.
 * ************************************************************************** */

/* PowerPC configurations should have at least the following registers available
   as callee-save:
     %r14 %r15 %r16 %r17 %r18 %r19 %r20 %r21 %r22
     %r23 %r24 %r25 %r26 %r27 %r28 %r29 .
   This is already excellent, but I suspect a few others might work as well.  For
   example GCC says that %r31 is used as a frame pointer, but stops complaining if
   I use -fomit-frame-pointer -- which I certainly do for VMs.
   I have seen this error message attempting to use %r30 on 32-bit PowerPC
   GNU/Linux:
     error: 30 cannot be used in asm here
   Not using it seems to solve the problem, so I deduce that %r30 is not usable,
   even I read somewhere that it was supposed to be callee-save.  I doubt PowerPC
   has instructions not accepting specific registers as arguments. */

/* Register pointing to The Array base. */
#define JITTER_BASE_REGISTER          %r14

/* How many registers we can use to hold residual arguments. */
#define JITTER_RESIDUAL_REGISTER_NO   14
//#define JITTER_RESIDUAL_REGISTER_NO   0 // this is good for testing memory literals

/* Registers holding residual arguments, with 0-based suffixes.  These have to
   be as many as JITTER_RESIDUAL_REGISTER_NO . */
#define JITTER_RESIDUAL_REGISTER_0    %r15
#define JITTER_RESIDUAL_REGISTER_1    %r16
#define JITTER_RESIDUAL_REGISTER_2    %r17
#define JITTER_RESIDUAL_REGISTER_3    %r18
#define JITTER_RESIDUAL_REGISTER_4    %r19
#define JITTER_RESIDUAL_REGISTER_5    %r20
#define JITTER_RESIDUAL_REGISTER_6    %r21
#define JITTER_RESIDUAL_REGISTER_7    %r22
#define JITTER_RESIDUAL_REGISTER_8    %r23
#define JITTER_RESIDUAL_REGISTER_9    %r24
#define JITTER_RESIDUAL_REGISTER_10   %r25
#define JITTER_RESIDUAL_REGISTER_11   %r26
#define JITTER_RESIDUAL_REGISTER_12   %r27
#define JITTER_RESIDUAL_REGISTER_13   %r28

/* The scratch register. */
#define JITTER_SCRATCH_REGISTER       %r29




/* Patch-ins.
 * ************************************************************************** */

/* Patch-ins are supported on the the PowerPC architecture. */
#define JITTER_MACHINE_SUPPORTS_PATCH_IN 1

/* An instruction entirely made of 0x0 bytes is invalid on the PowerPC, and also
   very easy to recognize at a glance. */
#define JITTER_ASM_PATCH_IN_FILL_BYTE    "0x00"

/* For each patch-in case define its size in bytes, corresponding to the total
   size of the instructions to be patched in in bytes, including possible
   padding nops. */
#define JITTER_PATCH_IN_SIZE_FAST_BRANCH_UNCONDITIONAL                        4
/* #define JITTER_PATCH_IN_SIZE_FAST_BRANCH_CONDITIONAL_ZERO                     ? */
/* #define JITTER_PATCH_IN_SIZE_FAST_BRANCH_CONDITIONAL_NOT_ZERO                 ? */
/* #define JITTER_PATCH_IN_SIZE_FAST_BRANCH_CONDITIONAL_EQUAL                    ? */
/* #define JITTER_PATCH_IN_SIZE_FAST_BRANCH_CONDITIONAL_NOT_EQUAL                ? */
/* #define JITTER_PATCH_IN_SIZE_FAST_BRANCH_CONDITIONAL_LESS_UNSIGNED            ? */
/* #define JITTER_PATCH_IN_SIZE_FAST_BRANCH_CONDITIONAL_LESS_SIGNED              ? */
/* #define JITTER_PATCH_IN_SIZE_FAST_BRANCH_CONDITIONAL_LESS_OR_EQUAL_UNSIGNED   ? */
/* #define JITTER_PATCH_IN_SIZE_FAST_BRANCH_CONDITIONAL_LESS_OR_EQUAL_SIGNED     ? */
/* #define JITTER_PATCH_IN_SIZE_FAST_BRANCH_CONDITIONAL_GREATER_UNSIGNED         ? */
/* #define JITTER_PATCH_IN_SIZE_FAST_BRANCH_CONDITIONAL_GREATER_SIGNED           ? */
/* #define JITTER_PATCH_IN_SIZE_FAST_BRANCH_CONDITIONAL_GREATER_OR_EQUAL_UNSIGNED ? */
/* #define JITTER_PATCH_IN_SIZE_FAST_BRANCH_CONDITIONAL_GREATER_OR_EQUAL_SIGNED  ? */
/* /\* These should be extended in the future with floating-point conditionals. *\/ */




/* C-only page, expanding to nothing if this header is included by assembly.
 * ************************************************************************** */

#ifndef __ASSEMBLER__

/* Notice that the order matters, and these shouldn't be rearranged without also
   changing the order of definitions in machine.S .  We also rely on the first
   case having value 0, since we use enum jitter_snippet_to_patch values as
   array indices. */
enum jitter_snippet_to_patch
  {
    jitter_snippet_load_sign_extended_16bit_to_register_0,
    jitter_snippet_load_sign_extended_16bit_to_register_1,
    jitter_snippet_load_sign_extended_16bit_to_register_2,
    jitter_snippet_load_sign_extended_16bit_to_register_3,
    jitter_snippet_load_sign_extended_16bit_to_register_4,
    jitter_snippet_load_sign_extended_16bit_to_register_5,
    jitter_snippet_load_sign_extended_16bit_to_register_6,
    jitter_snippet_load_sign_extended_16bit_to_register_7,
    jitter_snippet_load_sign_extended_16bit_to_register_8,
    jitter_snippet_load_sign_extended_16bit_to_register_9,
    jitter_snippet_load_sign_extended_16bit_to_register_10,
    jitter_snippet_load_sign_extended_16bit_to_register_11,
    jitter_snippet_load_sign_extended_16bit_to_register_12,
    jitter_snippet_load_sign_extended_16bit_to_register_13,
    jitter_snippet_load_32bit_to_register_0,
    jitter_snippet_load_32bit_to_register_1,
    jitter_snippet_load_32bit_to_register_2,
    jitter_snippet_load_32bit_to_register_3,
    jitter_snippet_load_32bit_to_register_4,
    jitter_snippet_load_32bit_to_register_5,
    jitter_snippet_load_32bit_to_register_6,
    jitter_snippet_load_32bit_to_register_7,
    jitter_snippet_load_32bit_to_register_8,
    jitter_snippet_load_32bit_to_register_9,
    jitter_snippet_load_32bit_to_register_10,
    jitter_snippet_load_32bit_to_register_11,
    jitter_snippet_load_32bit_to_register_12,
    jitter_snippet_load_32bit_to_register_13,
    /* FIXME: the next two snippets are not implemented yet. */
    jitter_snippet_load_sign_extended_16bit_to_memory,
    jitter_snippet_load_32bit_to_memory,

    jitter_snippet_jump_unconditional_26bit_offset_no_link,

    /* The number of snippets. */
    jitter_snippet_no
  };

#endif // #ifndef __ASSEMBLER__

#endif // #ifndef JITTER_NATIVE_MACHINE_H_