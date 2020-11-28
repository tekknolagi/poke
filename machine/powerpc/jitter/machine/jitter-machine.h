/* VM library: PowerPC definitions, to be included from both C and assembly.

   Copyright (C) 2017, 2019, 2020 Luca Saiu
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
#define JITTER_PATCH_IN_SIZE_FAST_BRANCH_UNCONDITIONAL    4
#define JITTER_PATCH_IN_SIZE_FAST_BRANCH_BRANCH_AND_LINK  4
/* There is no need for conditional branch snippets; the inline asm code is
   already correct and needs patching only in the offset.  Conditional branch
   snippets on PowerPC have size zero.  */




/* VM low-level conditional branches.
 * ************************************************************************** */

/* Ignore this page if patch-ins have been disabled (for debugging) or the
   dispatching model does not support them. */
#if defined(JITTER_MACHINE_SUPPORTS_PATCH_IN) && defined(JITTER_DISPATCH_NO_THREADING)

/* This factors the common logic of low-level conditional fast branches.
   opd1_tplt is a string literal template for the second register operand,
   including an initial comma; it can be passed as an empty string when there is
   no second register operand. */
#define _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_UNARY_(cmp_insn, b_insn,  \
                                                         opd0, tgt)         \
  asm goto (JITTER_ASM_DEFECT_DESCRIPTOR                                    \
            /* Emit the patch-in right before a correct conditional branch  \
               instruction, where the only field to patch is the (split)    \
               branch offset.  All the rest, including register numberss,   \
               is correctly generated by the inline asm and needs no        \
               patching. */                                                 \
            cmp_insn " cr7, %[jitter_operand0], 0\n"                        \
            JITTER_ASM_PATCH_IN_PLACEHOLDER                                 \
               (0 /* size in bytes */,                                      \
                JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_ANY /*case*/,  \
                (tgt),                                                      \
                0, 0, 0 /* not used for this case */)                       \
            "1: " b_insn " cr7, 1b\n\t"                                     \
            : /* outputs */                                                 \
            : JITTER_PATCH_IN_INPUTS_FOR_EVERY_CASE,                        \
              [jitter_operand0] "r" (opd0),                                 \
              JITTER_INPUT_VM_INSTRUCTION_BEGINNING /* inputs */            \
            : "cr7" /* clobbers */                                          \
            : jitter_dispatch_label /* goto labels */)
#define _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_BINARY_(                  \
           cmp_insn, b_insn, opd0, opd1, tgt)                               \
  asm goto (JITTER_ASM_DEFECT_DESCRIPTOR                                    \
            /* See the comment above within in                              \
               _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_UNARY_ . */        \
            cmp_insn " cr7, %[jitter_operand0], %[jitter_operand1]\n"       \
            JITTER_ASM_PATCH_IN_PLACEHOLDER                                 \
               (0 /* size in bytes */,                                      \
                JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_ANY /*case*/,  \
                (tgt),                                                      \
                0, 0, 0 /* not used for this case */)                       \
            "1: " b_insn " cr7, 1b\n\t"                                     \
            : /* outputs */                                                 \
            : JITTER_PATCH_IN_INPUTS_FOR_EVERY_CASE,                        \
              [jitter_operand0] "r" (opd0),                                 \
              [jitter_operand1] "r" (opd1),                                 \
              JITTER_INPUT_VM_INSTRUCTION_BEGINNING /* inputs */            \
            : "cr7" /* clobbers */                                          \
            : jitter_dispatch_label /* goto labels */)

/* Low-level fast branches on sign. */
#define _JITTER_LOW_LEVEL_BRANCH_FAST_IF_ZERO_(opd0, tgt)  \
  _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_UNARY_         \
     ("cmpi", "beq", (opd0), (tgt))
#define _JITTER_LOW_LEVEL_BRANCH_FAST_IF_NONZERO_(opd0, tgt)  \
  _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_UNARY_            \
     ("cmpi", "bne", (opd0), (tgt))
#define _JITTER_LOW_LEVEL_BRANCH_FAST_IF_NEGATIVE_(opd0, tgt)  \
  _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_UNARY_             \
     ("cmpi", "blt", (opd0), (tgt))
#define _JITTER_LOW_LEVEL_BRANCH_FAST_IF_NONNEGATIVE_(opd0, tgt)  \
  _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_UNARY_                \
     ("cmpi", "bge", (opd0), (tgt))
#define _JITTER_LOW_LEVEL_BRANCH_FAST_IF_POSITIVE_(opd0, tgt)  \
  _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_UNARY_             \
     ("cmpi", "bgt", (opd0), (tgt))
#define _JITTER_LOW_LEVEL_BRANCH_FAST_IF_NONPOSITIVE_(opd0, tgt)  \
  _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_UNARY_                \
     ("cmpi", "ble", (opd0), (tgt))

/* Low-level fast branches on equality comparisons. */
#define _JITTER_LOW_LEVEL_BRANCH_FAST_IF_EQUAL_(opd0, opd1, tgt)  \
  _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_BINARY_               \
     ("cmp", "beq", (opd0), (opd1), (tgt))
#define _JITTER_LOW_LEVEL_BRANCH_FAST_IF_NOTEQUAL_(opd0, opd1, tgt)  \
  _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_BINARY_               \
     ("cmp", "bne", (opd0), (opd1), (tgt))

/* Low-level fast branches on magnitude comparisons. */
#define _JITTER_LOW_LEVEL_BRANCH_FAST_IF_LESS_SIGNED_(opd0, opd1, tgt)  \
  _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_BINARY_                     \
     ("cmp", "blt", (opd0), (opd1), (tgt))
#define _JITTER_LOW_LEVEL_BRANCH_FAST_IF_GREATER_SIGNED_(opd0, opd1, tgt)  \
  _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_BINARY_                        \
     ("cmp", "bgt", (opd0), (opd1), (tgt))
#define _JITTER_LOW_LEVEL_BRANCH_FAST_IF_NOTLESS_SIGNED_(opd0, opd1, tgt)  \
  _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_BINARY_                        \
     ("cmp", "bge", (opd0), (opd1), (tgt))
#define _JITTER_LOW_LEVEL_BRANCH_FAST_IF_NOTGREATER_SIGNED_(opd0, opd1, tgt)  \
  _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_BINARY_                           \
     ("cmp", "ble", (opd0), (opd1), (tgt))
#define _JITTER_LOW_LEVEL_BRANCH_FAST_IF_LESS_UNSIGNED_(opd0, opd1, tgt)  \
  _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_BINARY_                       \
     ("cmpl", "blt", (opd0), (opd1), (tgt))
#define _JITTER_LOW_LEVEL_BRANCH_FAST_IF_GREATER_UNSIGNED_(opd0, opd1, tgt)  \
  _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_BINARY_                          \
     ("cmpl", "bgt", (opd0), (opd1), (tgt))
#define _JITTER_LOW_LEVEL_BRANCH_FAST_IF_NOTLESS_UNSIGNED_(opd0, opd1, tgt)  \
  _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_BINARY_                          \
     ("cmpl", "bge", (opd0), (opd1), (tgt))
#define _JITTER_LOW_LEVEL_BRANCH_FAST_IF_NOTGREATER_UNSIGNED_(opd0, opd1, tgt)  \
  _JITTER_LOW_LEVEL_BRANCH_FAST_CONDITIONAL_BINARY_                             \
     ("cmpl", "ble", (opd0), (opd1), (tgt))

// FIXME: and, nand, overflow

#endif // #if defined(JITTER_MACHINE_SUPPORTS_PATCH_IN) && defined(JITTER_DISPATCH_NO_THREADING)




/* VM procedures.
 * ************************************************************************** */

/* Procedures are supported on PowerPC ; however we conditionalise the definition
   on JITTER_MACHINE_SUPPORTS_PATCH_IN , for testing convenience. */
#ifdef JITTER_MACHINE_SUPPORTS_PATCH_IN
# define JITTER_MACHINE_SUPPORTS_PROCEDURE  1
#endif // #ifdef JITTER_MACHINE_SUPPORTS_PATCH_IN

#if    defined(JITTER_MACHINE_SUPPORTS_PATCH_IN)   \
    && defined(JITTER_DISPATCH_NO_THREADING)       \
    && defined(JITTER_MACHINE_SUPPORTS_PROCEDURE)

/* Load the link register into the given l-value .  This is executed at the
   very beginning of callee instructions. */
#define _JITTER_PROCEDURE_PROLOG(link_lvalue)                                   \
  do                                                                            \
    {                                                                           \
      const void * jitter_the_return_address;                                   \
      /* This inline asm statement must be volatile because it has no explicit  \
         inputs; the actual input is lr , which is not visible from C.          \
         If this is not volatile GCC can move it somewhere else where it is     \
         exectued only once, with the result saved on the stack.  Of course I   \
         don't want that.  VM instructions begin and and with volatile asm      \
         statements, so this cannot be moved across them. */                    \
      asm volatile ("mflr %[return_address]"                                    \
                    : [return_address] "=r" (jitter_the_return_address)         \
                      /* outputs */);                                           \
      link_lvalue = (const void *) (jitter_the_return_address);                 \
    }                                                                           \
  while (false)

/* Return using native machine language facilities, when the return address in
   in the given rvalue. */
#define JITTER_RETURN(link_rvalue)                                             \
  do                                                                           \
    {                                                                          \
      const void * jitter_the_return_address = (const void*) (link_rvalue);    \
      asm goto (JITTER_ASM_DEFECT_DESCRIPTOR                                   \
                "mtlr %[return_addr]\n\t"                                      \
                "blr\n\t"                                                      \
                : /* outputs. */                                               \
                : [return_addr] "r" (jitter_the_return_address) /* inputs. */  \
                : "lr" /* clobbers. */                                         \
                : jitter_dispatch_label /* gotolabels. */);                    \
      /* The rest of the VM instruction is unreachable. */                     \
      __builtin_unreachable ();                                                \
    }                                                                          \
  while (false)

/* Perform a branch-and-link to the pointed callee instruction using specific
   machine language features rather than generic indirect branches. */
#define JITTER_BRANCH_AND_LINK_INTERNAL(callee_rvalue)                        \
  do                                                                          \
    {                                                                         \
      const void * const jitter_destination =                                 \
        (const void * const) (callee_rvalue);                                 \
      asm goto (JITTER_ASM_DEFECT_DESCRIPTOR                                  \
                "mtctr %[destination]\n\t"                                    \
                "bctrl\n\t"                                                   \
                : /* outputs. */                                              \
                : [destination] "r" (jitter_destination) /* inputs. */        \
                : "ctr", "lr" /* clobbers. */                                 \
                : jitter_dispatch_label /* gotolabels. */);                   \
      /* Skip the rest of the specialised instruction, for compatibility */   \
      /* with more limited dispatches. */                                     \
      JITTER_JUMP_TO_SPECIALIZED_INSTRUCTION_END;                             \
    }                                                                         \
  while (false)

/* Branch to the pointed caleee destination, but set the link register to a
   return address different from the natural one. */
#define JITTER_BRANCH_AND_LINK_WITH(_jitter_callee_rvalue, _jitter_new_link)  \
  do                                                                          \
    {                                                                         \
      const void * const jitter_callee_rvalue =                               \
        (const void * const) (_jitter_callee_rvalue);                         \
      const void * const jitter_new_link =                                    \
        (const void * const) (_jitter_new_link);                              \
      asm goto (JITTER_ASM_DEFECT_DESCRIPTOR                                  \
                JITTER_ASM_COMMENT_UNIQUE("Branch-and-link-with, pretending"  \
                                          "to go to "                         \
                                          "%l[jitter_dispatch_label]")        \
                "mtctr %[jitter_callee_rvalue]\n\t"                           \
                "mtlr %[jitter_new_link]\n\t"                                 \
                "bctr\n\t"                                                    \
                : /* outputs. */                                              \
                : [jitter_callee_rvalue] "r" (jitter_callee_rvalue),          \
                  [jitter_new_link] "r" (jitter_new_link) /* inputs. */       \
                : "ctr", "lr" /* clobbers. */                                 \
                : jitter_dispatch_label /* gotolabels. */);                   \
      /* The rest of the VM instruction is unreachable: this is an            \
         unconditional jump. */                                               \
      __builtin_unreachable ();                                               \
    }                                                                         \
  while (false)

/* Perform a jal with return address in x1 .  Of course we need a patch-in,
   since the destination address is encoded in the jumping instruction. */
#define _JITTER_BRANCH_FAST_AND_LINK_INTERNAL(target_index)                    \
  do                                                                           \
    {                                                                          \
      asm goto (JITTER_ASM_DEFECT_DESCRIPTOR                                   \
                JITTER_ASM_PATCH_IN_PLACEHOLDER(                               \
                   JITTER_PATCH_IN_SIZE_FAST_BRANCH_BRANCH_AND_LINK /*size_in_bytes*/, \
                   JITTER_PATCH_IN_CASE_FAST_BRANCH_BRANCH_AND_LINK /*case*/,  \
                   target_index,                                               \
                   0, 0, 0 /* not used for this case */)                       \
                : /* outputs. */                                               \
                : JITTER_PATCH_IN_INPUTS_FOR_EVERY_CASE,                       \
                  JITTER_INPUT_VM_INSTRUCTION_BEGINNING /* inputs */           \
                : /* clobbers. */                                              \
                : jitter_dispatch_label /* gotolabels. */);                    \
      /* The rest of this specialised instruction is unreachable.  This        \
         implementation is not based on hardware call and return, so there     \
         is no need to generate a hardware jump either. */                     \
      __builtin_unreachable ();                                                \
    }                                                                          \
  while (false)

#endif // #if    defined(JITTER_MACHINE_SUPPORTS_PATCH_IN) ...




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
    jitter_snippet_jump_and_link_26bit_offset,
    jitter_snippet_jump_conditional_16bit_offset,

    /* The number of snippets. */
    jitter_snippet_no
  };

#endif // #ifndef __ASSEMBLER__

#endif // #ifndef JITTER_NATIVE_MACHINE_H_
