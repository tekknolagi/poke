/* VM library: MIPS definitions, to be included from both C and assembly.

   Copyright (C) 2017, 2018, 2019 Luca Saiu
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

/* How to introduce comments in assembly on MIPS. */
#define JITTER_ASM_COMMENT_PREFIX "# "




/* Debugging.
 * ************************************************************************** */

/* Expand to an inline assembly template generating a nop instruction containing
   the given literal as an argument. */
#define _JITTER_ASM_DEBUGGING_NOP(integer_literal_as_string)  \
  "addi $0, $0, " integer_literal_as_string




/* Computed goto.
 * ************************************************************************** */

/* Computed goto implemented with inline asm.  See the comments about
   JITTER_ASM_COMPUTED_GOTO_TEMPLATE, and
   JITTER_ASM_COMPUTED_GOTO_INPUT_CONSTRAINT and
   JITTER_ASM_COMPUTED_GOTO_CLOBBERS in jitter/jitter-interpreter-private.h .
   In the case of MIPS we can afford to keep .reorder on as per the default
   setting, and *not* to specify a delay slot.  Gas will be able to fill the
   delay slot itself with something useful, when we are lucky. */
#define JITTER_ASM_COMPUTED_GOTO_TEMPLATE          "jr %[_jitter_the_target]"
#define JITTER_ASM_COMPUTED_GOTO_INPUT_CONSTRAINT  "r"




/* Reserved registers.
 * ************************************************************************** */

/* MIPS configurations should have at least the following registers available
   as callee-save:
     $16 $17 $18 $19 $20 $21 $22 $23
   This is already very good, but I suspect a few others might work as well.
   I can almost certainly use $1/$at as scratch within an uninterrupted
   sequence of instructions written by me; I'm not so sure $30/$fp is actually
   used as a frame pointer by GCC, or if I should prevent it; and I've never
   seriously investigated $26-$27/$k0-k1 , but as far as I understand they
   are clobbered by the operating system at unpredictable times. */

/* Register pointing to The Array base. */
#define JITTER_BASE_REGISTER          $16

/* How many registers we can use to hold residual arguments. */
#define JITTER_RESIDUAL_REGISTER_NO   6
//#define JITTER_RESIDUAL_REGISTER_NO   0 // this is good for testing memory literals

/* Registers holding residual arguments, with 0-based suffixes.  These have to
   be as many as JITTER_RESIDUAL_REGISTER_NO . */
#define JITTER_RESIDUAL_REGISTER_0    $17
#define JITTER_RESIDUAL_REGISTER_1    $18
#define JITTER_RESIDUAL_REGISTER_2    $19
#define JITTER_RESIDUAL_REGISTER_3    $20
#define JITTER_RESIDUAL_REGISTER_4    $21
#define JITTER_RESIDUAL_REGISTER_5    $22

/* The scratch register.  The same remark above applies. */
#define JITTER_SCRATCH_REGISTER       $23




/* Patch-ins.
 * ************************************************************************** */

/* Patch-ins are supported on the the MIPS architecture. */
#define JITTER_MACHINE_SUPPORTS_PATCH_IN 1

/* 0xffffffff is not technically an invalid instruction (objdump disassembles it
  as "sd $31, -1($31)"), but it makes programs crash very easily; this makes
  0xff a good choice for the default byte to write on uninitialized instruction
  memory to catch bugs. */
#define JITTER_ASM_PATCH_IN_FILL_BYTE    "0xff"

/* For each patch-in case define its size in bytes, corresponding to the total
   size of the instructions to be patched in in bytes, including possible
   padding nops. */
#define JITTER_PATCH_IN_SIZE_FAST_BRANCH_UNCONDITIONAL                        8

/* MIPS uses a single patch-in case for every conditional branch, and its size
   is zero. */
#define JITTER_PATCH_IN_SIZE_FAST_BRANCH_BRANCH_AND_LINK                      8
/* /\* These should be extended in the future with floating-point conditionals. *\/ */




/* VM conditional branches.
 * ************************************************************************** */

/* Ignore this page if patch-ins have been disabled (for debugging) or the
   dispatching model does not support them. */
#if defined(JITTER_MACHINE_SUPPORTS_PATCH_IN) && defined(JITTER_DISPATCH_NO_THREADING)

/* The same patch-in works for any conditional branch on MIPS, and the
   placeholder has size zero: the idea is emitting the patch-in right before a
   correct conditional branch instruction, where the only field to patch is the
   16-bit branch offset.  All the rest, including registers, is correctly
   generated by inline asm and needs no patching. */
#define JITTER_ASM_MIPS_CONDITIONAL_BRANCH_PATCH_IN(target_index)  \
  JITTER_ASM_PATCH_IN_PLACEHOLDER(                                 \
     0 /* size in bytes */,                                        \
     JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_ANY /*case*/,    \
     target_index,                                                 \
     0, 0, 0 /* not used for this case */)

/* Emit a conditional branch instruction with one register operand. */
#define JITTER_MIPS_BRANCH_FAST_CONDITIONAL_R(instruction_string, operand,  \
                                              target_index)                 \
  do                                                                        \
    {                                                                       \
      asm goto (JITTER_ASM_DEFECT_DESCRIPTOR                                \
                JITTER_ASM_MIPS_CONDITIONAL_BRANCH_PATCH_IN(target_index)   \
                "1:\n\t"                                                    \
                instruction_string " %[jitter_operand], 1b"                 \
                : /* outputs */                                             \
                : JITTER_PATCH_IN_INPUTS_FOR_EVERY_CASE,                    \
                  [jitter_operand] "r" (operand),                           \
                  JITTER_INPUT_VM_INSTRUCTION_BEGINNING /* inputs */        \
                : /* clobbers */                                            \
                : jitter_dispatch_label /* goto labels */);                 \
    }                                                                       \
  while (false)

/* Emit a conditional branch instruction with two register operands. */
#define JITTER_MIPS_BRANCH_FAST_CONDITIONAL_R_R(instruction_string, operand0,  \
                                                operand1, target_index)        \
  do                                                                           \
    {                                                                          \
      asm goto (JITTER_ASM_DEFECT_DESCRIPTOR                                   \
                JITTER_ASM_MIPS_CONDITIONAL_BRANCH_PATCH_IN(target_index)      \
                "1:\n\t"                                                       \
                instruction_string                                             \
                " %[jitter_operand0], %[jitter_operand1], 1b"                  \
                : /* outputs */                                                \
                : JITTER_PATCH_IN_INPUTS_FOR_EVERY_CASE,                       \
                  [jitter_operand0] "r" (operand0),                            \
                  [jitter_operand1] "r" (operand1),                            \
                  JITTER_INPUT_VM_INSTRUCTION_BEGINNING /* inputs */           \
                : /* clobbers */                                               \
                : jitter_dispatch_label /* goto labels */);                    \
    }                                                                          \
  while (false)

/* Emit a conditional branch instruction with two register operands, where the
   first is $0. */
#define JITTER_MIPS_BRANCH_FAST_CONDITIONAL_Z_R(instruction_string, operand1,  \
                                                target_index)                  \
  do                                                                           \
    {                                                                          \
      asm goto (JITTER_ASM_DEFECT_DESCRIPTOR                                   \
                JITTER_ASM_MIPS_CONDITIONAL_BRANCH_PATCH_IN(target_index)      \
                "1:\n\t"                                                       \
                instruction_string " $0, %[jitter_operand1], 1b"               \
                : /* outputs */                                                \
                : JITTER_PATCH_IN_INPUTS_FOR_EVERY_CASE,                       \
                  [jitter_operand1] "r" (operand1),                            \
                  JITTER_INPUT_VM_INSTRUCTION_BEGINNING /* inputs */           \
                : /* clobbers */                                               \
                : jitter_dispatch_label /* goto labels */);                    \
    }                                                                          \
  while (false)

/* Emit a conditional branch instruction with two register operands, where the
   second is $0. */
#define JITTER_MIPS_BRANCH_FAST_CONDITIONAL_R_Z(instruction_string, operand0,  \
                                                target_index)                  \
  do                                                                           \
    {                                                                          \
      asm goto (JITTER_ASM_DEFECT_DESCRIPTOR                                   \
                JITTER_ASM_MIPS_CONDITIONAL_BRANCH_PATCH_IN(target_index)      \
                "1:\n\t"                                                       \
                instruction_string " %[jitter_operand0], $0, 1b"               \
                : /* outputs */                                                \
                : JITTER_PATCH_IN_INPUTS_FOR_EVERY_CASE,                       \
                  [jitter_operand0] "r" (operand0),                            \
                  JITTER_INPUT_VM_INSTRUCTION_BEGINNING /* inputs */           \
                : /* clobbers */                                               \
                : jitter_dispatch_label /* gotolabels */);                     \
    }                                                                          \
  while (false)

/* Emit a conditional branch instruction with one register operand. */
#define JITTER_MIPS_BRANCH_FAST_CONDITIONAL_UNARY(insn_name, opd,           \
                                                  target_index)             \
  do                                                                        \
    {                                                                       \
      JITTER_MIPS_BRANCH_FAST_CONDITIONAL_R(insn_name, opd, target_index);  \
    }                                                                       \
  while (false)

/* Emit a conditional branch instruction with two register operands, using
   JITTER_MIPS_BRANCH_FAST_CONDITIONAL_Z_R or
   JITTER_MIPS_BRANCH_FAST_CONDITIONAL_R_Z where possible, falling back to
   JITTER_MIPS_BRANCH_FAST_CONDITIONAL_R_R when neither argument is known to be
   zero. */
#define JITTER_MIPS_BRANCH_FAST_CONDITIONAL_BINARY(insn_name, type,            \
                                                   opd0, opd1, target_index)   \
  do                                                                           \
    {                                                                          \
      type _jitter_operand_0 = (type) (opd0);                                  \
      type _jitter_operand_1 = (type) (opd1);                                  \
      if (JITTER_IS_KNOWN_CONSTANT_ZERO(_jitter_operand_0))                    \
        JITTER_MIPS_BRANCH_FAST_CONDITIONAL_Z_R(insn_name, _jitter_operand_1,  \
                                                target_index);                 \
      else if (JITTER_IS_KNOWN_CONSTANT_ZERO(_jitter_operand_1))               \
        JITTER_MIPS_BRANCH_FAST_CONDITIONAL_R_Z(insn_name, _jitter_operand_0,  \
                                                target_index);                 \
      else                                                                     \
        JITTER_MIPS_BRANCH_FAST_CONDITIONAL_R_R(insn_name, _jitter_operand_0,  \
                                                _jitter_operand_1,             \
                                                target_index);                 \
    }                                                                          \
  while (false)


/* Unary conditions.  MIPS can express branches conditional on a comparisons
   with zero in one instruction. */
#define _JITTER_BRANCH_FAST_IF_ZERO(opd, target_index)                   \
  JITTER_MIPS_BRANCH_FAST_CONDITIONAL_BINARY("beq", jitter_int, opd, 0,  \
                                             target_index)
#define _JITTER_BRANCH_FAST_IF_NONZERO(opd, target_index)                \
  JITTER_MIPS_BRANCH_FAST_CONDITIONAL_BINARY("bne", jitter_int, opd, 0,  \
                                             target_index)
#define _JITTER_BRANCH_FAST_IF_POSITIVE(opd, target_index)               \
  JITTER_MIPS_BRANCH_FAST_CONDITIONAL_UNARY("bgtz", opd, target_index)
#define _JITTER_BRANCH_FAST_IF_NONPOSITIVE(opd, target_index)            \
  JITTER_MIPS_BRANCH_FAST_CONDITIONAL_UNARY("blez", opd, target_index)
#define _JITTER_BRANCH_FAST_IF_NEGATIVE(opd, target_index)               \
  JITTER_MIPS_BRANCH_FAST_CONDITIONAL_UNARY("bltz", opd, target_index)
#define _JITTER_BRANCH_FAST_IF_NONNEGATIVE(opd, target_index)            \
  JITTER_MIPS_BRANCH_FAST_CONDITIONAL_UNARY("bgez", opd, target_index)

/* Simple binary conditions.  Before the MIPS{32,64}r6 revision MIPS can express
   branches conditional on an "equal" or "different" condition in one
   instruction, but not branches conditional on other binary comparisons, unless
   one of the operands is zero. */
#define _JITTER_BRANCH_FAST_IF_EQUAL(opd0, opd1, target_index)                \
  JITTER_MIPS_BRANCH_FAST_CONDITIONAL_BINARY("beq", jitter_uint, opd0, opd1,  \
                                             target_index)
#define _JITTER_BRANCH_FAST_IF_NOTEQUAL(opd0, opd1, target_index)             \
  JITTER_MIPS_BRANCH_FAST_CONDITIONAL_BINARY("bne", jitter_uint, opd0, opd1,  \
                                             target_index)

/* GCC can easily generate an instruction such as slt , slti , stlu or sltiu to
   set a temporary register to nonzero if a condition is true; we can use C to
   set such a register, then use a fast conditional branch on the register
   checking if it's nonzero.

   MIPS has no sgt instruction, so simply computing a condition which is true
   when we need to jump may require one more instruction to reverse it; if I
   force it to jump on a true condition GCC generates a "xori $FINAL_CONDITION,
   $TEMPORARY_CONDITION, 0x1" instruction, which works; but that is suboptimal.
   The right solution is computing the opposite condition when a xori would be
   needed, and then branching on false instead of true.  The compute_opposite
   parameter determines if the condition is to be reversed. [FIXME: this is what
   I did naïvely: I have to check whether the relevant parameter is known,
   and whether the relevant one is the first or the second depends on the
   condition.]

   This is good but not always optimal: we can do better if we know that one
   of the operands being compared is zero.  The macros below use this one
   when it is not possible to prove that either operand is zero. */
/* FIXME: there is a xori generated in example-vms/uninspired/examples/fibo.vm .
   The instruction in question is "ble %r0, 0x1, $L16", which uses
   _JITTER_BRANCH_FAST_IF_NOTGREATER_SIGNED , itslef defined in terms of
   _JITTER_BRANCH_FAST_IF_NOTLESS_SIGNED .
   Despite being correct the code is suboptimal: there must be a mishandled
   case here.
   I have to sit down when I'm less tired and think hard about the logic in this
   code, but I suppose the problem is which one of the two arguments is known:
   slti and sltiu only take an immediate as their *second* comparand.  If I'm
   not taking this into account then my code is not optimal for every case.  The
   assignment to _jitter_condition below is supposed to generate exactly one
   instruction. either "slt $d, $c1, c2" or "slt $d, $c1, $c2"; I don't think we
   can ignore whether c2 (which, by the way, could be opd0 rather than opd1) is
   known.  Or maybe it isn't possible to do it with just one instruction in
   every case; but I'd be surprised that the MIPS designers didn't include
   sgti/sgtiu instructions then. */
#define _JITTER_BRANCH_FAST_IF_BINARY_GENERAL(infix, type,                  \
                                              opd0, opd1, target_index,     \
                                              compute_opposite)             \
  do                                                                        \
    {                                                                       \
      jitter_uint _jitter_condition;                                        \
      if (compute_opposite)                                                 \
        {                                                                   \
          _jitter_condition = ! ((type)(opd0) infix (type)(opd1));          \
          _JITTER_BRANCH_FAST_IF_ZERO(_jitter_condition, target_index);     \
        }                                                                   \
      else                                                                  \
        {                                                                   \
          _jitter_condition = ((type)(opd0) infix (type)(opd1));            \
          _JITTER_BRANCH_FAST_IF_NONZERO(_jitter_condition, target_index);  \
        }                                                                   \
    }                                                                       \
  while (false)

/* Check if either opd0 or opd1 is known to be zero; in that case use a
   conditional fast branch on the other, which takes just one instruction.
   Otherwise use _JITTER_BRANCH_FAST_IF_BINARY_GENERAL , which takes two
   [FIXME: or three?  See the comment about xori] instructions. */
#define _JITTER_BRANCH_FAST_IF_BINARY(infix, type, opd0, opd1, target_index, \
                                     condition_on_opd1_when_opd0_is_zero,    \
                                     condition_on_opd0_when_opd1_is_zero,    \
                                     compute_opposite)                       \
  do                                                                         \
    {                                                                        \
      type _jitter_operand_0 = (type) (opd0);                                \
      type _jitter_operand_1 = (type) (opd1);                                \
      if (JITTER_IS_KNOWN_CONSTANT_ZERO(_jitter_operand_0))                  \
        JITTER_CONCATENATE_TWO(JITTER_BRANCH_FAST,                           \
                               condition_on_opd1_when_opd0_is_zero)(         \
            _jitter_operand_1, target_index);                                \
      else if (JITTER_IS_KNOWN_CONSTANT_ZERO(_jitter_operand_1))             \
        JITTER_CONCATENATE_TWO(JITTER_BRANCH_FAST,                           \
                               condition_on_opd0_when_opd1_is_zero)(         \
            _jitter_operand_0, target_index);                                \
      else                                                                   \
        _JITTER_BRANCH_FAST_IF_BINARY_GENERAL(infix, type,                   \
                                             _jitter_operand_0,              \
                                             _jitter_operand_1,              \
                                             target_index,                   \
                                             compute_opposite);              \
    }                                                                        \
  while (false)

/* Fast branches conditional on a binary comparison. */
#define _JITTER_BRANCH_FAST_IF_LESS_SIGNED(opd0, opd1, target_index)      \
  _JITTER_BRANCH_FAST_IF_BINARY(<, jitter_int, opd0, opd1, target_index,  \
                               _IF_POSITIVE, /* 0 < opd1 */               \
                               _IF_NEGATIVE  /* opd0 < 0 */,              \
                               false)
#define _JITTER_BRANCH_FAST_IF_LESS_UNSIGNED(opd0, opd1, target_index)     \
  _JITTER_BRANCH_FAST_IF_BINARY(<, jitter_uint, opd0, opd1, target_index,  \
                               _IF_NONZERO,    /* 0 <u opd1 */             \
                               _IF_NEVER_UNARY /* opd0 <u 0 */,            \
                               false)
#define _JITTER_BRANCH_FAST_IF_NOTLESS_SIGNED(opd0, opd1, target_index)    \
  _JITTER_BRANCH_FAST_IF_BINARY(>=, jitter_int, opd0, opd1, target_index,  \
                               _IF_NONPOSITIVE, /* 0 >= opd1 */            \
                               _IF_NONNEGATIVE  /* opd0 >= 0 */,           \
                               true)
#define _JITTER_BRANCH_FAST_IF_NOTLESS_UNSIGNED(opd0, opd1, target_index)   \
  _JITTER_BRANCH_FAST_IF_BINARY(>=, jitter_uint, opd0, opd1, target_index,  \
                               _IF_ZERO,        /* 0 >=u opd1 */            \
                               _IF_ALWAYS_UNARY /* opd0 >=u 0 */,           \
                               true)
#define _JITTER_BRANCH_FAST_IF_GREATER_SIGNED(opd0, opd1, target_index)  \
  _JITTER_BRANCH_FAST_IF_LESS_SIGNED(opd1, opd0, target_index)
#define _JITTER_BRANCH_FAST_IF_GREATER_UNSIGNED(opd0, opd1, target_index)  \
  _JITTER_BRANCH_FAST_IF_LESS_UNSIGNED(opd1, opd0, target_index)
#define _JITTER_BRANCH_FAST_IF_NOTGREATER_SIGNED(opd0, opd1, target_index)  \
  _JITTER_BRANCH_FAST_IF_NOTLESS_SIGNED(opd1, opd0, target_index)
#define _JITTER_BRANCH_FAST_IF_NOTGREATER_UNSIGNED(opd0, opd1, target_index)  \
  _JITTER_BRANCH_FAST_IF_NOTLESS_UNSIGNED(opd1, opd0, target_index)

#endif // #if defined(JITTER_MACHINE_SUPPORTS_PATCH_IN) && defined(JITTER_DISPATCH_NO_THREADING)




/* VM procedures.
 * ************************************************************************** */

/* Procedures are supported on MIPS ; but we conditionalize the definition
   on JITTER_MACHINE_SUPPORTS_PATCH_IN , for testing convenience. */
#ifdef JITTER_MACHINE_SUPPORTS_PATCH_IN
# define JITTER_MACHINE_SUPPORTS_PROCEDURE    1
#endif // #ifdef JITTER_MACHINE_SUPPORTS_PATCH_IN

#if    defined(JITTER_MACHINE_SUPPORTS_PATCH_IN)   \
    && defined(JITTER_DISPATCH_NO_THREADING)       \
    && defined(JITTER_MACHINE_SUPPORTS_PROCEDURE)

/* The return address is in $31 (and I never need to worry about saving its
   initial value before starting, since the interpreter has already called C
   functions at initialization, which means that before the interpreter
   returns it will have to restore its original $31 from the stack anyway);
   just copy $31 to link_lvalue . */
#define _JITTER_PROCEDURE_PROLOG(link_lvalue)                                   \
  do                                                                            \
    {                                                                           \
      register const void * jitter_the_return_address asm ("$31");              \
      /* Let GCC believe we are initializing $31 in the inline asm code; */     \
      /* in reality it's already set. */                                        \
      asm ("# Pretend to set %[return_address], even if it's already set."      \
           : [return_address] "=r" (jitter_the_return_address) /* outputs */);  \
      link_lvalue = (const void *) (jitter_the_return_address);                 \
    }                                                                           \
  while (false)

/* The recommended way of returning from a procedure on MIPS is by jumping via
   register to $31 ; the MIPS32 specification, while remaining abstract, at
   least strongly implies that jumping to $31 may give the correct (pop) hint to
   the CPU branch target predictor, differently from other registers; GCC
   generates code consistent with this hypothesis.  So it's simple: place the
   target address into $31 , and jump to it. */
#define JITTER_RETURN(link_rvalue)                                             \
  do                                                                           \
    {                                                                          \
      /* By using a local register variable we can avoid a register copy */    \
      /* to set $31 . */                                                       \
      register const void * const jitter_the_return_address asm ("$31")        \
        = (const void* const) (link_rvalue);                                   \
      /* Gas seems to do a good job of using the delay slot, which is not */   \
      /* always a nop.  This is why I don't use .set nomacro, noreorder . */   \
      asm goto ("jr %[return_addr]\n\t"                                        \
                /* Putting the defect descriptor before jr prevents GCC from   \
                   using the delay slot intelligently.  It's harmless to have  \
                   it here after the return instruction. */                    \
                JITTER_ASM_DEFECT_DESCRIPTOR                                   \
                : /* outputs. */                                               \
                : [return_addr] "r" (jitter_the_return_address) /* inputs. */  \
                : /* clobbers. */                                              \
                : jitter_dispatch_label /* gotolabels. */);                    \
      /* The rest of the VM instruction is unreachable. */                     \
      __builtin_unreachable ();                                                \
    }                                                                          \
  while (false)

/* Easy: perform a jump-and-link via ragister, and the return address will be
   in $31 . */
#define JITTER_BRANCH_AND_LINK_INTERNAL(callee_rvalue)                        \
  do                                                                          \
    {                                                                         \
      const void * const jitter_destination =                                 \
        (const void * const) (callee_rvalue);                                 \
      /* Gas seems to do a good job of using the delay slot, which is not */  \
      /* always a nop.  This is why I don't use .set nomacro, noreorder . */  \
      asm goto ("jalr %[destination]"                                         \
                /* See the comment in JITTER_RETURN. */                       \
                JITTER_ASM_DEFECT_DESCRIPTOR                                  \
                : /* outputs. */                                              \
                : [destination] "r" (jitter_destination) /* inputs. */        \
                : "$31" /* clobbers. */                                       \
                : jitter_dispatch_label /* gotolabels. */);              \
      /* Skip the rest of the specialized instruction, for compatibility */   \
      /* with more limited dispatches. */                                     \
      JITTER_JUMP_TO_SPECIALIZED_INSTRUCTION_END;                             \
    }                                                                         \
  while (false)

/* Perform an ordinary jump thru register, and load the given return address
   in $31, exploiting the delay slot. */
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
                "\n.set noreorder\n\t"                                        \
                "\n.set nomacro\n\t"                                          \
                "jr %[jitter_callee_rvalue]\n\t"                              \
                "or $31, %[jitter_new_link], $0\n\t"                          \
                "\n.set macro\n\t"                                            \
                "\n.set reorder\n\t"                                          \
                : /* outputs. */                                              \
                : [jitter_callee_rvalue] "r" (jitter_callee_rvalue),          \
                  [jitter_new_link] "r" (jitter_new_link) /* inputs. */       \
                : "$31" /* clobbers. */                                       \
                : jitter_dispatch_label /* gotolabels. */);                   \
      /* The rest of the VM instruction is unreachable: this is an            \
         unconditional jump. */                                               \
      __builtin_unreachable ();                                               \
    }                                                                         \
  while (false)

#ifdef JITTER_MACHINE_SUPPORTS_PATCH_IN
/* Perform a pseudo-direct jump-and-link, and the return address will be in $31
   .  Of course we need a patch-in for the pseudo-direct jump-and-link, since
   the destination address is encoded in the jumping instruction. */
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
                : jitter_dispatch_label /* gotolabels. */);               \
      /* Skip the rest of the specialized instruction, for compatibility */    \
      /* with more limited dispatches. */                                      \
      JITTER_JUMP_TO_SPECIALIZED_INSTRUCTION_END;                              \
    }                                                                          \
  while (false)
#endif // #ifdef JITTER_MACHINE_SUPPORTS_PATCH_IN

#endif // #if    defined(JITTER_MACHINE_SUPPORTS_PATCH_IN) ...



/* C-only page, expanding to nothing if this header is included by assembly.
 * ************************************************************************** */

#ifndef __ASSEMBLER__

/* Notice that the order matters, and these shouldn't be rearranged without also
   changing the order of definitions in machine.S .  We also rely on the first
   case having value 0, since we use enum jitter_routine_to_patch values as
   array indices. */
enum jitter_routine_to_patch
  {
    jitter_routine_load_zero_extended_16bit_to_register_0,
    jitter_routine_load_zero_extended_16bit_to_register_1,
    jitter_routine_load_zero_extended_16bit_to_register_2,
    jitter_routine_load_zero_extended_16bit_to_register_3,
    jitter_routine_load_zero_extended_16bit_to_register_4,
    jitter_routine_load_zero_extended_16bit_to_register_5,
    jitter_routine_load_sign_extended_16bit_to_register_0,
    jitter_routine_load_sign_extended_16bit_to_register_1,
    jitter_routine_load_sign_extended_16bit_to_register_2,
    jitter_routine_load_sign_extended_16bit_to_register_3,
    jitter_routine_load_sign_extended_16bit_to_register_4,
    jitter_routine_load_sign_extended_16bit_to_register_5,
    jitter_routine_load_32bit_to_register_0,
    jitter_routine_load_32bit_to_register_1,
    jitter_routine_load_32bit_to_register_2,
    jitter_routine_load_32bit_to_register_3,
    jitter_routine_load_32bit_to_register_4,
    jitter_routine_load_32bit_to_register_5,
    /* FIXME: the next three are not implemented yet. */
    jitter_routine_load_zero_extended_16bit_to_memory,
    jitter_routine_load_sign_extended_16bit_to_memory,
    jitter_routine_load_32bit_to_memory,

    jitter_routine_jump_unconditional_28bit_pseudo_direct,
    jitter_routine_jump_and_link_28bit_pseudo_direct,

    /* The same routine works for any conditional branch. */
    jitter_routine_branch_conditional_18bit_offset,

    /* The number of routines. */
    jitter_routine_no
  };

#endif // #ifndef __ASSEMBLER__

#endif // #ifndef JITTER_NATIVE_MACHINE_H_
