/* VM library: x86_64 definitions, to be included from both C and assembly.

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


#ifndef JITTER_NATIVE_MACHINE_H_ /* One macro suffices for all architectures. */
#define JITTER_NATIVE_MACHINE_H_


/* Assembler syntax.
 * ************************************************************************** */

/* How to introduce comments in assembly on x86_64. */
#define JITTER_ASM_COMMENT_PREFIX "# "




/* Optional features.
 * ************************************************************************** */

/* If this macro is defined then implement branch-and-link differently, without
   using callq and retq: instead use %rip-relative address to compute the link
   address with a leaq instruction, load the return address into the scratch
   register, and do a regular jump.
   This ended up being slower than the obvious solution on the machines I
   tested but is useful to keep as an example, possibly to be adapted to
   other architectures */
//#define JITTER_BRANCH_AND_LINK_NO_CALL 1




/* Computed goto.
 * ************************************************************************** */

/* Computed goto implemented with inline asm.  See the comments about
   JITTER_ASM_COMPUTED_GOTO_TEMPLATE, and
   JITTER_ASM_COMPUTED_GOTO_INPUT_CONSTRAINT and
   JITTER_ASM_COMPUTED_GOTO_CLOBBERS in jitter/jitter-interpreter-private.h . */
#define JITTER_ASM_COMPUTED_GOTO_TEMPLATE          "jmpq *%[_jitter_the_target]"
#define JITTER_ASM_COMPUTED_GOTO_INPUT_CONSTRAINT  "rm"




/* Reserved registers.
 * ************************************************************************** */

/* On x86_64 the following registers should not be clobbered by calls, and
   therefore are suitable to reserve:
     %rbx %rbp %r12 %r13 %r14 %r15 .
   I've seen GCC complain ("error: frame pointer required, but reserved")
   in some cases if I reserve %rbp, so that is better avoided. */

/* How many registers we can use to hold residual arguments. */
#define JITTER_RESIDUAL_REGISTER_NO   2
//#define JITTER_RESIDUAL_REGISTER_NO   0 // this is good for testing memory literals

/* Registers holding residual arguments, with 0-based suffixes.  These have to
   be as many as JITTER_RESIDUAL_REGISTER_NO .  On x86_64 I also need to use the low
   32 bits of the registers, which have different names; since I couldn't find
   a way of generating them programmatically from suffixes in all cases using
   either Gas or CPP macros due to the irregularity of the syntax (%eax<->%rax
   vs. %r12<->%r12d), the different definitions need to be kept synchronized by
   hand. */
#define JITTER_RESIDUAL_REGISTER_0          %rbx
#define JITTER_RESIDUAL_REGISTER_0_32BIT    %ebx
#define JITTER_RESIDUAL_REGISTER_1          %r12
#define JITTER_RESIDUAL_REGISTER_1_32BIT    %r12d

/* These are normally used when JITTER_RESIDUAL_REGISTER_NO is 2. */
#define JITTER_RESIDUAL_REGISTER_2          %r15
#define JITTER_RESIDUAL_REGISTER_2_32BIT    %r15d

/* The scratch register.  The same remark above applies. */
#define JITTER_SCRATCH_REGISTER             %r13
#define JITTER_SCRATCH_REGISTER_32BIT       %r13d
/* #define JITTER_SCRATCH_REGISTER             %rbx */
/* #define JITTER_SCRATCH_REGISTER_32BIT       %ebx */

/* Register pointing to a memory buffer holding residual arguments not fitting
   in the registers above.  This is always used as a 64-bit register, so no
   _32BIT version is needed. */
#define JITTER_RESIDUAL_BASE_REGISTER       %r14




/* Patch-ins.
 * ************************************************************************** */

/* Patch-ins are supported on the x86_64 architecture. */
#define JITTER_MACHINE_SUPPORTS_PATCH_IN    1

/* Skip the rest of this page if JITTER_MACHINE_SUPPORTS_PATCH_IN has been
   disabled (presumably for testing), or if patch-ins are not used with the
   current dispatching mode. */
#if    defined(JITTER_MACHINE_SUPPORTS_PATCH_IN) \
    && defined(JITTER_DISPATCH_NO_THREADING)

/* It's nice to have a byte always making up invalid instructions when repeated
   any number of times; this should always be invalid on x86_64 in long mode,
   even if the hexadecimal value is not particularly memorable or easy to
   recognize at a glance. */
#define JITTER_ASM_PATCH_IN_FILL_BYTE       "0xea"

/* For each patch-in case define its size in bytes, corresponding to the total
   size of the instructions to be patched in in bytes, including possible
   padding nops.
   FIXME: no, not really: it's not necessary to do it for every patch-in
   case. */
#define JITTER_PATCH_IN_SIZE_FAST_BRANCH_UNCONDITIONAL   5
#define JITTER_PATCH_IN_SIZE_FAST_BRANCH_CONDITIONAL     6 /* all cases. */
#define JITTER_PATCH_IN_SIZE_FAST_BRANCH_BRANCH_AND_LINK 5

#define JITTER_BRANCH_FAST_CONDITIONAL(asm_instruction_name,                    \
                                       constraints0, constraints1,              \
                                       case, operand0, operand1, target_index)  \
  do                                                                            \
    {                                                                           \
      /* In AT&T syntax operand1 comes before operand0: this is not a mistake. */ \
      asm goto (asm_instruction_name " %[jitter_operand1], %[jitter_operand0]\n\t" \
                JITTER_ASM_PATCH_IN_PLACEHOLDER(                                \
                   JITTER_PATCH_IN_SIZE_FAST_BRANCH_CONDITIONAL /*size_in_bytes*/, \
                   case /*case*/,                                               \
                   target_index,                                                \
                   0, 0, 0 /* not used for this case */)                        \
                : /* outputs */                                                 \
                :   [jitter_operand0] constraints0 (operand0)                   \
                  , [jitter_operand1] constraints1 (operand1)                   \
                  , JITTER_INPUT_VM_INSTRUCTION_BEGINNING /* inputs */          \
                : "cc" /* clobbers */                                           \
                : jitter_jump_anywhere_label /* goto labels */);                \
    }                                                                           \
  while (false)


/* FIXME: this works in all cases but only operand1 may be a literal: this
   limitation may cause suboptimal code generation with a movq $LITERAL, %TEMP
   preceding the comparison instruction.  I should automatically reverse the
   conditional jump condition and swap the two arguments if the literal happens
   to be in the wrong position.  This can be done with __builtin_constant_p .

   Another sensible optimization, even if likely less useful in real code, would
   be checking whether *both* operands are literals, and if so turning the
   conditional branch into an unconditional branch. */

/* FIXME: the cmp instruction sign-extends its immediate, if any, to the size of
   the other operand; since I use the q prefix here the immediate is
   sign-extended to 64 bits.  I haven't found an explicit statement about this
   in the intel documentation, but I suppose that test zero-extends
   immediates. */
#define JITTER_BRANCH_FAST_CMP(case, type, operand0, operand1, target_index) \
  JITTER_BRANCH_FAST_CONDITIONAL("cmpq", "rm", "er", case, \
                                 (type)(operand0), (type)(operand1), target_index)


#define JITTER_BRANCH_FAST_TEST(case, type, operand0, operand1, target_index) \
  JITTER_BRANCH_FAST_CONDITIONAL("testq", "rm", "Zr", case, \
                                 (type)(operand0), (type)(operand1), target_index)

// FIXME: comment.
#define JITTER_BRANCH_FAST_TEST_ONE_OPERAND(case, type, operand0, target_index) \
  JITTER_BRANCH_FAST_TEST(case, type, operand0, operand0, target_index)


#define JITTER_BRANCH_FAST_IF_ZERO(operand0, target_index) \
  JITTER_BRANCH_FAST_TEST_ONE_OPERAND(JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_ZERO, \
                                      jitter_uint, operand0, target_index)
#define JITTER_BRANCH_FAST_IF_NONZERO(operand0, target_index) \
  JITTER_BRANCH_FAST_TEST_ONE_OPERAND(JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NONZERO, \
                                      jitter_uint, operand0, target_index)

#define JITTER_BRANCH_FAST_IF_POSITIVE(operand0, target_index) \
  JITTER_BRANCH_FAST_CMP(JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_GREATER_SIGNED, \
                         jitter_int, operand0, 0, target_index)
#define JITTER_BRANCH_FAST_IF_NONPOSITIVE(operand0, target_index) \
  JITTER_BRANCH_FAST_CMP(JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NOTGREATER_SIGNED, \
                         jitter_int, operand0, 0, target_index)

#define JITTER_BRANCH_FAST_IF_NEGATIVE(operand0, target_index) \
  JITTER_BRANCH_FAST_TEST_ONE_OPERAND(JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NEGATIVE, \
                                      jitter_int, operand0, target_index)
#define JITTER_BRANCH_FAST_IF_NONNEGATIVE(operand0, target_index) \
  JITTER_BRANCH_FAST_TEST_ONE_OPERAND(JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NONNEGATIVE, \
                                      jitter_int, operand0, target_index)

#define JITTER_BRANCH_FAST_IF_EQUAL(operand0, operand1, target_index) \
  /* FIXME: nonimmediate? */ \
  JITTER_BRANCH_FAST_CMP(JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_EQUAL, \
                         jitter_uint, operand0, operand1, target_index)
#define JITTER_BRANCH_FAST_IF_NOTEQUAL(operand0, operand1, target_index) \
  /* FIXME: nonimmediate? */ \
  JITTER_BRANCH_FAST_CMP(JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NOTEQUAL, \
                         jitter_uint, operand0, operand1, target_index)

#define JITTER_BRANCH_FAST_IF_LESS_SIGNED(operand0, operand1, target_index) \
  JITTER_BRANCH_FAST_CMP(JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_LESS_SIGNED, \
                         jitter_int, operand0, operand1, target_index)
#define JITTER_BRANCH_FAST_IF_LESS_UNSIGNED(operand0, operand1, target_index) \
  JITTER_BRANCH_FAST_CMP(JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_LESS_UNSIGNED, \
                         jitter_uint, operand0, operand1, target_index)
#define JITTER_BRANCH_FAST_IF_NOTGREATER_SIGNED(operand0, operand1, target_index) \
  JITTER_BRANCH_FAST_CMP(JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NOTGREATER_SIGNED, \
                         jitter_int, operand0, operand1, target_index)
#define JITTER_BRANCH_FAST_IF_NOTGREATER_UNSIGNED(operand0, operand1, target_index) \
  JITTER_BRANCH_FAST_CMP(JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NOTGREATER_UNSIGNED, \
                         jitter_uint, operand0, operand1, target_index)

#define JITTER_BRANCH_FAST_IF_GREATER_SIGNED(operand0, operand1, target_index) \
  JITTER_BRANCH_FAST_CMP(JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_GREATER_SIGNED, \
                         jitter_int, operand0, operand1, target_index)
#define JITTER_BRANCH_FAST_IF_GREATER_UNSIGNED(operand0, operand1, target_index) \
  JITTER_BRANCH_FAST_CMP(JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_GREATER_UNSIGNED, \
                         jitter_uint, operand0, operand1, target_index)
#define JITTER_BRANCH_FAST_IF_NOTLESS_SIGNED(operand0, operand1, target_index) \
  JITTER_BRANCH_FAST_CMP(JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NOTLESS_SIGNED, \
                         jitter_int, operand0, operand1, target_index)
#define JITTER_BRANCH_FAST_IF_NOTLESS_UNSIGNED(operand0, operand1, target_index) \
  JITTER_BRANCH_FAST_CMP(JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NOTLESS_UNSIGNED, \
                         jitter_uint, operand0, operand1, target_index)
#endif // #if defined(JITTER_MACHINE_SUPPORTS_PATCH_IN) && defined(...




/* VM procedures.
 * ************************************************************************** */

/* Procedures are supported on x86_64 ; but we conditionalize the definition
   on JITTER_MACHINE_SUPPORTS_PATCH_IN , for testing convenience. */
#ifdef JITTER_MACHINE_SUPPORTS_PATCH_IN
# define JITTER_MACHINE_SUPPORTS_PROCEDURE    1
#endif // #ifdef JITTER_MACHINE_SUPPORTS_PATCH_IN

#if    defined(JITTER_MACHINE_SUPPORTS_PATCH_IN)  \
    && defined(JITTER_DISPATCH_NO_THREADING)      \
    && defined(JITTER_MACHINE_SUPPORTS_PROCEDURE)

#ifdef JITTER_BRANCH_AND_LINK_NO_CALL

/* Procedure prolog, the version not relying on callq / retq . */
#define _JITTER_PROCEDURE_PROLOG(link_lvalue)                           \
  do                                                                    \
    {                                                                   \
      asm volatile ("movq %" JITTER_STRINGIFY(JITTER_SCRATCH_REGISTER)  \
                       ", %[the_link_lvalue]"                           \
           : [the_link_lvalue] "=rm" (link_lvalue) /* outputs */        \
           : /* inputs */                                               \
           : /* clobbers */);                                           \
    }                                                                   \
  while (false)

/* Procedure return, the version not relying on callq / retq . */
#define JITTER_RETURN(link_rvalue)                                    \
  do                                                                  \
    {                                                                 \
      asm goto ("jmpq *%[the_link_rvalue]\n\t"                        \
                : /* outputs. */                                      \
                : [the_link_rvalue] "rm" (link_rvalue) /* inputs. */  \
                : /* clobbers. */                                     \
                : jitter_jump_anywhere_label /* gotolabels. */);      \
      /* The rest of the VM instruction is unreachable. */            \
      __builtin_unreachable ();                                       \
    }                                                                 \
  while (false)

/* Branch-and-link, the version not relying on callq / retq . */
#define _JITTER_BRANCH_AND_LINK(callee_rvalue)                               \
  do                                                                         \
    {                                                                        \
      asm goto ("leaq jitter_return_address_%=(%%rip), %"                    \
                   JITTER_STRINGIFY(JITTER_SCRATCH_REGISTER) "\n\t"          \
                "jmpq *%[the_callee_rvalue]\n"                               \
                "jitter_return_address_%=:\n"                                \
                : /* outputs. */                                             \
                : [the_callee_rvalue] "rm" (callee_rvalue) /* inputs. */     \
                : /* clobbers. */                                            \
                : jitter_jump_anywhere_label /* gotolabels. */);             \
      /* Skip the rest of the specialized instruction, for compatibility */  \
      /* with more limited dispatches. */                                    \
      JITTER_JUMP_TO_SPECIALIZED_INSTRUCTION_END;                            \
    }                                                                        \
  while (false)

#ifdef JITTER_MACHINE_SUPPORTS_PATCH_IN
/* Branch-and-link to a fast label, the version not relying on callq / retq . */
#define _JITTER_BRANCH_AND_LINK_FAST(target_index)                              \
  do                                                                            \
    {                                                                           \
      asm goto ("leaq jitter_return_address_%=(%%rip), %"                       \
                   JITTER_STRINGIFY(JITTER_SCRATCH_REGISTER) "\n\t"             \
                JITTER_ASM_PATCH_IN_PLACEHOLDER(                                \
                   JITTER_PATCH_IN_SIZE_FAST_BRANCH_UNCONDITIONAL /*size_in_bytes*/, \
                   JITTER_PATCH_IN_CASE_FAST_BRANCH_UNCONDITIONAL /*case*/,     \
                   target_index,                                                \
                   0, 0, 0 /* not used for this case */)                        \
                "jitter_return_address_%=:\n"                                   \
                : /* outputs. */                                                \
                : JITTER_INPUT_VM_INSTRUCTION_BEGINNING /* inputs */            \
                : /* clobbers. */                                               \
                : jitter_jump_anywhere_label /* gotolabels. */);                \
      /* Skip the rest of the specialized instruction, for compatibility */     \
      /* with more limited dispatches. */                                       \
      JITTER_JUMP_TO_SPECIALIZED_INSTRUCTION_END;                               \
    }                                                                           \
  while (false)
#endif // #ifdef JITTER_MACHINE_SUPPORTS_PATCH_IN

#else // ! defined(JITTER_BRANCH_AND_LINK_NO_CALL)

/* Procedure prolog, the version relying on callq / retq .

   It is okay for %[the_link_lvalue] to be an offsettable memory operand
   relative to %rsp: in this case, as per the intel documentation about the
   "pop" instruction, its effective address is computed relative to the value of
   %rsp *after* a quad has been popped, which is what we want here: the
   temporary effect of altering %rsp by pushing and popping the return address
   is not visible elsewhere.  Anyway, it is *not* okay for %[the_link_lvalue] to
   be anything more complex, whose effective address needs to be computed using
   one or more separate instructions: in that case %rsp would have the wrong
   value.  This is why we introduce jitter_the_link_value (register or
   %rsp-relative) as a temporary.  The idea is computing the effective address
   for link_lvalue *after* the popq instruction.  But if link_lvalue is in a
   register we lose no efficiency: the memory clobber won't affect it, so
   jitter_the_link_value will be kept in the same register, and this entire
   macro will expand to a single "popq %REGISTER" instruction.  As far as I can
   see we should lose no efficiency even if link_lvalue were at an offset from
   %rsp , but in this case I never see the generated cose as popq OFFSET(%rsp),
   which is a pity.  I may have been too conservative here in some place I'm not
   seeing, or maybe this inefficiency is necessary. */
#define _JITTER_PROCEDURE_PROLOG(link_lvalue)                              \
  do                                                                       \
    {                                                                      \
      const void *jitter_the_link_value;                                   \
      asm volatile                                                         \
         ("popq %[the_link_lvalue]"                                        \
          : [the_link_lvalue] "=ro" (jitter_the_link_value) /* outputs */  \
          : /* inputs */                                                   \
          : /* clobbers */);                                               \
      (link_lvalue) = jitter_the_link_value;                               \
    }                                                                      \
  while (false)

/* Procedure return, the version relying on callq / retq . */         \
#define JITTER_RETURN(link_rvalue)                                    \
  do                                                                  \
    {                                                                 \
      asm goto ("pushq %[the_link_rvalue]\n\t"                        \
                "retq"                                                \
                : /* outputs. */                                      \
                : [the_link_rvalue] "rm" (link_rvalue) /* inputs. */  \
                : /* clobbers. */                                     \
                : jitter_jump_anywhere_label /* gotolabels. */);      \
      /* The rest of the VM instruction is unreachable. */            \
      __builtin_unreachable ();                                       \
    }                                                                 \
  while (false)

/* Branch-and-link, the version relying on callq / retq . */
#define _JITTER_BRANCH_AND_LINK(callee_rvalue)                              \
  do                                                                        \
    {                                                                       \
      const void * restrict jitter_call_indirect_target = (callee_rvalue);  \
      asm goto ("callq *%[target]"                                          \
                : /* outputs */                                             \
                : [target] "rm" (jitter_call_indirect_target) /* inputs */  \
                : /* clobbers */                                            \
                : jitter_jump_anywhere_label /* goto labels */);            \
      /* Make the rest of the VM instruction unreachable. */                \
      JITTER_JUMP_TO_SPECIALIZED_INSTRUCTION_END;                           \
    }                                                                       \
  while (false)

#ifdef JITTER_MACHINE_SUPPORTS_PATCH_IN
/* Branch-and-link to a fast label, the version relying on callq / retq . */
#define _JITTER_BRANCH_AND_LINK_FAST(target_index)                              \
  do                                                                            \
    {                                                                           \
      asm goto (JITTER_ASM_PATCH_IN_PLACEHOLDER(                                \
                   JITTER_PATCH_IN_SIZE_FAST_BRANCH_BRANCH_AND_LINK /*size_in_bytes*/, \
                   JITTER_PATCH_IN_CASE_FAST_BRANCH_BRANCH_AND_LINK /*case*/,   \
                   target_index,                                                \
                   0, 0, 0 /* not used for this case */)                        \
                : /* outputs */                                                 \
                : JITTER_INPUT_VM_INSTRUCTION_BEGINNING /* inputs */            \
                : /* clobbers */                                                \
                : jitter_jump_anywhere_label /* goto labels */);                \
      /* Make the rest of the VM instruction unreachable. */                    \
      JITTER_JUMP_TO_SPECIALIZED_INSTRUCTION_END;                               \
    }                                                                           \
  while (false)
#endif // #ifdef JITTER_MACHINE_SUPPORTS_PATCH_IN

#endif // #ifdef JITTER_BRANCH_AND_LINK_NO_CALL

#endif // #if defined(JITTER_MACHINE_SUPPORTS_PATCH_IN) && defined(...




/* C-only page, expanding to nothing if this header is included by assembly.
 * ************************************************************************** */

#ifndef __ASSEMBLER__

/* On x86_64 any 32-bit write to a 64-bit register (but of course not a 32-bit
   store) automatically sets the high 32 bits to zero -- this is a zero-extend,
   not a sign-extend.
   Moreover there is no direct way of loading a literal to a register without
   supplying an immediate smaller than 32 bits.  The choice, therefore, is
   limited to just 32-bit (non-negative and up to 1 << 31 included) or 64-bit
   (otherwise). */
/* FIXME: no, there's another useful case: I can load a 32-bit negative literal
   into a 64-bit register with a movq, and in this case it's sign-extended --
   with the instruction being much shorter than a movabsq:
     48 c7 c3 ff ff ff ff   # movq $-1, %rbx # the 64-bit register value is negative
   Which is different from, say,
     bb ff ff ff ff         # movl $-1, %ebx
   , where the result as a 64-bit integer is positive -- I already cover this.
   What shall I do about this with memory residuals?
   I could even use
     31 db         # xorl %ebx, %ebx
     83 c3 01      # addl $0x1, %ebx
   to load constants in [1, 127] into 64-bit registers.
   This:
     31 db         # xorl %ebx, %ebx
     48 83 c3 ff   # addq $-1, %rbx   # sign-extended
   works for constants in [-128, -1].
   Are these fast?  Is there a way not to have them affect flags?  I doubt it.
   This is good for loading 1, assuming it's fast (not so sure: incq modifies
   some flag, and /mnt/big/handbooks/assembly/intel/optimizing-subroutines-in-assembly-language--fog--1996-2016.pdf §16.2 recommends using add/sub when optimizing for speed, vs. inc/dec when optimizing for size unless we expect no penalty from flag dependencies, which might or might not be my case):
     31 db         # xorl %ebx, %ebx
     48 ff c3      # incq %rbx

   FIXME: See /mnt/big/handbooks/assembly/intel/optimizing-subroutines-in-assembly-language--fog--1996-2016.pdf , §10.2 "Using shorter constants and addresses".
*/

/* Notice that the order matters, and these shouldn't be rearranged without also
   changing the order of definitions in machine.S .  We also rely on the first
   case having value 0, since we use enum jitter_routine_to_patch values as
   array indices. */
enum jitter_routine_to_patch
  {
    jitter_routine_load_0_to_64bit_residual_register_0,
    jitter_routine_load_0_to_64bit_residual_register_1,
    jitter_routine_load_minus_1_to_64bit_residual_register_0,
    jitter_routine_load_minus_1_to_64bit_residual_register_1,
    jitter_routine_set_64bit_residual_register_0,
    jitter_routine_set_64bit_residual_register_1,
    jitter_routine_set_64bit_residual_memory_small_offset,
    jitter_routine_set_64bit_residual_memory_zero_offset,
    jitter_routine_set_64bit_residual_memory_big_offset,
    jitter_routine_set_32bit_residual_register_0,
    jitter_routine_set_32bit_residual_register_1,
    jitter_routine_set_32bit_residual_memory_small_offset,
    jitter_routine_set_32bit_residual_memory_zero_offset,
    jitter_routine_set_32bit_residual_memory_big_offset,
    jitter_routine_jump_unconditional_32bit_offset,
    jitter_routine_jump_on_zero_32bit_offset,
    jitter_routine_jump_on_nonzero_32bit_offset,
    jitter_routine_jump_on_sign_32bit_offset,
    jitter_routine_jump_on_nonsign_32bit_offset,
    jitter_routine_jump_on_equal_32bit_offset,
    jitter_routine_jump_on_notequal_32bit_offset,
    jitter_routine_jump_on_above_32bit_offset,
    jitter_routine_jump_on_notbelow_32bit_offset,
    jitter_routine_jump_on_below_32bit_offset,
    jitter_routine_jump_on_notabove_32bit_offset,
    jitter_routine_jump_on_less_32bit_offset,
    jitter_routine_jump_on_notgreater_32bit_offset,
    jitter_routine_jump_on_greater_32bit_offset,
    jitter_routine_jump_on_notless_32bit_offset,
    jitter_routine_call_32bit_offset,

    /* The number of routines. */
    jitter_routine_no
  };

#endif // #ifndef __ASSEMBLER__

#endif // #ifndef JITTER_NATIVE_MACHINE_H_
