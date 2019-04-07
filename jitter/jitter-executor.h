/* Jitter: runtime VM-independent header for generated executors.

   Copyright (C) 2016, 2017, 2019 Luca Saiu
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


/* This header is used internally by machine-generated executors.  It
   is not for the user to see.

   For example, some functionality defined here may depend on
   JITTER_VM_PREFIX_LOWER_CASE and JITTER_VM_PREFIX_UPPER_CASE, which are
   defined by machine-generated code in a .c file, without polluting the user
   name space. */

#ifndef JITTER_EXECUTOR_H_
#define JITTER_EXECUTOR_H_

#include <jitter/jitter.h>
#include <jitter/jitter-fast-branch.h>
#include <jitter/jitter-sections.h>


/* Consistency check.
 * ************************************************************************** */

// FIXME: this check is probably wrong.  Instead I should make sure that fast
// branch-and-links are rewritten into a default version using unconditional
// fast branches.

/* /\* Defining patch-in support without procedures leads to subtle annoying */
/*    crashes: in practice it's not possible to check if a fast label argument of */
/*    some instruction is used only as a simple branch target, or for */
/*    branch-and-link as well; when the fast version of a branching macro is */
/*    rewritten into the slow version by the fallback definitions below, the */
/*    rewritten definition must work: either both branch and branch-and-link are */
/*    supported for fast label, or neither. */

/*    Notice that it is not necessary to explicitly support every conditional */
/*    branch variant: if some conditional is not expliticitly defined it is */
/*    rewritten into an ordinary C conditional containing an unconditional fast */
/*    branch.  Having the unconditional version suffices. *\/ */
/* #if    defined(JITTER_DISPATCH_NO_THREADING)      \ */
/*     && defined(JITTER_MACHINE_SUPPORTS_PATCH_IN)  \ */
/*     && ! defined(JITTER_MACHINE_SUPPORTS_PROCEDURE) */
/* # error "you can't define machine-specific procedure support without having patch-ins." */
/* #endif */




/* Unique string generation in (GNU) CPP.
 * ************************************************************************** */

/* Expand to a string literal, different at every expansion.  This is convenient for
   inline assembly comments, to prevent GCC from factoring code which is meant
   to be duplicated -- typically to avoid tail-merging in replicated VM
   instruction code.
   Notice that the unique identifier generated in inline asm code by "%=" is not
   enough to prevent tail-merging: GCC sees two identical string literals containing
   "%=" as, indeed, equal.
   This relies on __COUNTER__ , a GNU C preprocessor extension, but the advanced
   dispatching models requiring this rely on GCC anyway. */
#define JITTER_STRING_LITERAL_UNIQUE  \
  " [" JITTER_STRINGIFY(__COUNTER__) "] "

/* Expand to an integer literal, different at any expansion.  This has similar
   applications to JITTER_STRING_LITERAL_UNIQUE , but is meant for C
   identifiers.  Again, this relies on the GNU C preprocessor. */
#define JITTER_INTEGER_LITERAL_UNIQUE  \
  __COUNTER__




/* Miscellaneous machinery for the executor, possibly to move.
 * ************************************************************************** */

/* Expand to a line-comment prefix for assembly as a string.  "\n# " is a sensible
   default working on most architectures, but this can be overridden in the
   architecture-specific header
   machine/ARCHICTECTURE/jitter/machine/jitter-machine.h .
   Rationale for the newline: on SH, at least with the GNU assembler, comments
   are introduced by "#" at the beginning of a line or by "!" anywhere else; but
   "!" is not supported on other architectures. */
#ifndef JITTER_ASM_COMMENT_PREFIX
# define JITTER_ASM_COMMENT_PREFIX  \
    "\n# "
#endif // #ifndef JITTER_ASM_COMMENT_PREFIX

/* Expand to a string literal suitable to be part inline asm, containing a
   comment-opening text and a terminating "\n\t" sequence. */
#define JITTER_ASM_COMMENT(_jitter_string_literal)         \
  JITTER_ASM_COMMENT_PREFIX _jitter_string_literal "\n\t"

/* Expand to a string literal suitable to be part inline asm, containing a
   comment-opening text, a unique identifier making the generated literal
   different from all others, and a terminating "\n\t" sequence. */
#define JITTER_ASM_COMMENT_UNIQUE(_jitter_string_literal)   \
  JITTER_ASM_COMMENT_PREFIX _jitter_string_literal " "      \
  JITTER_STRING_LITERAL_UNIQUE "\n\t"

/* Expand to an inline asm C statement containing the given comment, and a
   terminating "\n\t" sequence. */
#define JITTER_COMMENT_IN_ASM_(_jitter_string_literal)       \
  asm volatile (JITTER_ASM_COMMENT(_jitter_string_literal))

/* Expand to a string literal containing an asm comment, including containing
   the given text and a unique identifier which will prevent GCC from merging
   different expansions with the same argument. */
#define JITTER_COMMENT_IN_ASM_UNIQUE_(_jitter_string_literal)       \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE(_jitter_string_literal))




/* Debugging features to be used with disassembly.
 * ************************************************************************** */

// FIXME: I should move this section to a different header.

/* "Debugging nops" serve to generate nop instructions containing recognizable
   integer arguments, to be read back by humans disassembling compiled code.
   If there is no architecture-specific code for generating debugging nops, just
   generate nothing in their place. */
#ifndef _JITTER_ASM_DEBUGGING_NOP
# define _JITTER_ASM_DEBUGGING_NOP(integer_literal_as_string)  \
  ""
#endif // #ifndef _JITTER_ASM_DEBUGGING_NOP

/* Debugging nops: user macro, generating a readable asm comment followed (where
   possible) by the nop.
   The argument must be an integer literal in Gas syntax, with no surrounding
   quotes.  The machine-specific macro will emit the appropriate prefix to
   interpret the digit sequence as a hexadecimal constant.
   The expansion of this macro should be used in an *extended* asm statement,
   since '%' characters appearing, for example, as register prefixes, will
   appear escaped as "%%".
   For portability with respect to architectures only supporting small operands,
   the arguments should be non-negative and representable in 7 bits (Rationale:
   one way to implement a "nop" is by adding a short immediate to a register,
   followed by adding it back with the opposite sign; another alternative is
   xoring and a literal constant into the register itself, twice). */
#define JITTER_ASM_DEBUGGING_NOP(integer_literal)                 \
  JITTER_ASM_COMMENT_UNIQUE ("Debugging nop: "                    \
                             JITTER_STRINGIFY (integer_literal))  \
  _JITTER_ASM_DEBUGGING_NOP(JITTER_STRINGIFY (integer_literal))   \
  "\n\t"

/* A machine code snipped causing a trap, written in text form in a syntax
   suitable for extended inline asm.  If no machine-specific definition exists,
   define a stub here. */
#ifndef _JITTER_ASM_CRASH
# define _JITTER_ASM_CRASH                                      \
    JITTER_ASM_COMMENT_PREFIX "unimplemented for this machine"
#endif // #ifndef _JITTER_ASM_DEBUGGING_NOP

/* Expand to a C statement causing a trap.
   This is meant to catch bugs, by delimiting code past the end of VM
   specialized instruction which is not supposed to be replicated.  If such
   code is ever executed it is useful to make the failure well visible.
   It is important that this does not use __builtin_unreachable .  The
   code this is used in is in fact unreachable, but GCC must not be
   informed about the fact, as the code serves to keep the register
   assignment compatible across different program points: the compiler
   must see some impossible control transfers as possible. */
#define JITTER_CRASH_                                        \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE ("Cause a crash")  \
                _JITTER_ASM_CRASH "\n\t"                     \
                : /* outputs */)




/* Macros to let GCC see variables as changed.
 * ************************************************************************** */

// FIXME: use in the definitions below.
#define JITTER_MARK_AS_ASM_OUTPUT_(_jitter_constraint, _jitter_lvalue)  \
  asm (JITTER_ASM_COMMENT_UNIQUE("Pretend to set "                      \
                                 JITTER_STRINGIFY(_jitter_lvalue)       \
                                 " with a "                             \
                                 _jitter_constraint                     \
                                 " constraint in "                      \
                                 "%[the_jitter_lvalue]")                \
       : [the_jitter_lvalue] _jitter_constraint (_jitter_lvalue))

// FIXME: comment
#define JITTER_MARK_RVALUE_AS_READ_BY_ASSEMBLY(variable)               \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE(                             \
                   "Pretend to read the register or memory variable "  \
                   JITTER_STRINGIFY(variable) " in "                   \
                   "%[_variable]")                                     \
                :                                                      \
                : [_variable] "X" (variable))
#define JITTER_MARK_LVALUE_AS_DEFINED_BY_ASSEMBLY(variable)             \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE(                              \
                   "Pretend to write the register or memory variable "  \
                   JITTER_STRINGIFY(variable) " in "                    \
                   "%[_variable]")                                      \
                : [_variable] "=X" (variable))
#define JITTER_MARK_LVALUE_AS_SET_BY_ASSEMBLY(variable)                \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE(                             \
                  "Pretend to read and write the register or memory "  \
                  "variable " JITTER_STRINGIFY(variable) " in "        \
                  "%[_variable]")                                      \
                : [_variable] "+g" (variable))

#define JITTER_MARK_REGISTER_AS_DEFINED_BY_ASSEMBLY(variable)  \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE(                     \
                   "Pretend to write the register variable "   \
                   JITTER_STRINGIFY(variable) " in "           \
                   "%[register_variable]")                     \
                : [register_variable] "=r" (variable))
#define JITTER_MARK_REGISTER_AS_SET_BY_ASSEMBLY(variable)              \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE(                             \
                   "Pretend to read and write the register variable "  \
                                   JITTER_STRINGIFY(variable) " in "   \
                                   "%[register_variable]")             \
                : [register_variable] "+r" (variable))

#define JITTER_MARK_MEMORY_AS_DEFINED_BY_ASSEMBLY(variable)  \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE(                   \
                   "Pretend to write the memory variable "   \
                     JITTER_STRINGIFY(variable))             \
                : "=m" (variable))
#define JITTER_MARK_MEMORY_AS_SET_BY_ASSEMBLY(variable)              \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE(                           \
                   "Pretend to read and write the memory variable "  \
                   JITTER_STRINGIFY(variable))                       \
                : "+m" (variable))
#define JITTER_MARK_ARRAY_ELEMENT_AS_SET_BY_ASSEMBLY(variable, offset)     \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE(                                 \
                   "Pretend to read and write a memory word from "         \
                   JITTER_STRINGIFY(variable)                              \
                   " at offset " JITTER_STRINGIFY(offset))                 \
                : "+m" (* (jitter_int*) ((char*) (variable) + (offset))))




/* Fallback defect descriptors.
 * ************************************************************************** */

/* If defect descriptors are not supported, provide a dummy compatibility macro
   expanding to an empty string.
   FIXME: use defect descriptors, when possible, with minimal threading. */
#ifndef JITTER_ASM_DEFECT_DESCRIPTOR
# define JITTER_ASM_DEFECT_DESCRIPTOR ""
#endif // #ifndef JITTER_ASM_DEFECT_DESCRIPTOR



/* Macros to let GCC see jumps to indeterminate locations.
 * ************************************************************************** */

// FIXME: comment.
#define JITTER_IP_INPUT_CONSTRAINT "r"

// FIXME: comment.
// FIXME: remove.  Fake-jumping to an arbitrary label different from
// jitter_dispatch_label doesn't work well with defect descriptors.
#define JITTER_PRETEND_TO_POSSIBLY_JUMP_TO_(_jitter_label)                  \
  asm goto (JITTER_ASM_COMMENT_UNIQUE("Pretend to possibly jump to "        \
                                      JITTER_STRINGIFY(_jitter_label)       \
                                      " at %l["                             \
                                      JITTER_STRINGIFY(_jitter_label)       \
                                      "] based on "                         \
                                      " jitter_ip"                          \
                                      " at %[jitter_ip]")                   \
            : /* outputs */                                                 \
            : [jitter_ip] JITTER_IP_INPUT_CONSTRAINT (jitter_ip)  \
              /* inputs */                                                  \
            : /* clobbers */                                                \
            : /* jump destinations */ _jitter_label)

#define JITTER_PRETEND_TO_UPDATE_IP_                          \
  JITTER_MARK_AS_ASM_OUTPUT_("+" JITTER_IP_INPUT_CONSTRAINT,  \
                             jitter_ip)

#define JITTER_PRETEND_TO_JUMP_TO_(_jitter_label)      \
  JITTER_PRETEND_TO_POSSIBLY_JUMP_TO_(_jitter_label);  \
  __builtin_unreachable ()

/* Expand to zero assembly instructions, but with inline asm constraints
   affecting GCC's program representation as if the generated code could either
   jump to the content of jitter_anywhere_variable or fall thru.

   Rationale: see the comment about JITTER_PRETEND_TO_JUMP_ANYWHERE below. */
#define JITTER_PRETEND_TO_POSSIBLY_JUMP_ANYWHERE            \
  do                                                        \
    {                                                       \
      asm goto (JITTER_ASM_DEFECT_DESCRIPTOR                \
                JITTER_ASM_COMMENT_UNIQUE( \
                   "# Pretending to possibly jump to " \
                   "%l[jitter_dispatch_label] thru %[jitter_ip]") \
                : \
                : [jitter_ip] "X" (jitter_ip) \
                : \
                : jitter_dispatch_label); \
    }                                                                  \
  while (false)

/* Expand to zero assembly instructions, but with inline asm constraints and
   intrinsics affecting GCC's program representation as if the generated code
   unconditionally jumped to the content of jitter_anywhere_variable .

   Rationale: this code is actually unreachable, but for this program point I
   want GCC to use a register assignment which is coherent with the beginning of
   any other VM instruction, which may actually follow this point during
   replicated code execution.  I would like to simply insert a "goto
   jump_anywhere;" here, but GCC might move the jump before the instruction end
   label [FIXME: can it really happen?  I'm pretty sure I saw that more than
   once, but I should ask some GCC expert for confirmation], which must not
   happen.  So, in order to generate something GCC optimizations can't meddle
   with, I will insert a pretend-jump in inline assembly, with the constraint
   that the control may only flow to the jump_anywhere label; from there control
   could actually jump to any label in this function.  This way I ensure
   compatibility among VM instructions without generating too much junk code, if
   any.  The input operand is required to be in memory and *not* in a register,
   so that GCC doesn't waste a register on this useless thing. */
#define JITTER_PRETEND_TO_JUMP_ANYWHERE          \
  do                                             \
    {                                            \
      JITTER_PRETEND_TO_POSSIBLY_JUMP_ANYWHERE;  \
      __builtin_unreachable ();                  \
    }                                            \
  while (false)




/* Macros expanding to per-instruction labels and variable names.
 * ************************************************************************** */

/* Expand to the label at the beginning of the user code for the specialized
   instruction with the given name, in case of relocatable instructions;
   otherwise expand to the label at the beginning of the relocatable stub
   for non-relocatable instructions. */
#define JITTER_SPECIALIZED_INSTRUCTION_BEGIN_LABEL_OF(name)  \
  JITTER_CONCATENATE_THREE(jitter_specialized_instruction_, name, _beginning_label)

/* Expand to the label at the end of the user code for the specialized
   instruction with the given name. */
#define JITTER_SPECIALIZED_INSTRUCTION_END_LABEL_OF(name)  \
  JITTER_CONCATENATE_THREE(jitter_specialized_instruction_, name, _end_label)

/* Expand to the label at the beginning of the user code for the specialized
   instruction with the given name, in case of non-relocatable instructions. */
#define JITTER_SPECIALIZED_INSTRUCTION_NON_RELOCATABLE_CODE_LABEL_OF(name)  \
  JITTER_CONCATENATE_THREE(jitter_specialized_instruction_, name, _non_relocatable_code_label)

/* Expand to the name of the variable defined as
   JITTER_SPECIALIZED_INSTRUCTION_NON_RELOCATABLE_CODE_LABEL_OF(name) .  The
   variable is always stack-allocated and can be safely read from relocated
   code. */
#define JITTER_SPECIALIZED_INSTRUCTION_NON_RELOCATABLE_CODE_VARIABLE_OF(name)  \
  JITTER_CONCATENATE_THREE(jitter_specialized_instruction_, name, _non_relocatable_code_variable)




/* User macros to access VM state data structures.
 * ************************************************************************** */

/* Expand to the current VM state runtime, as a struct. */
#define JITTER_STATE_RUNTIME  \
  jitter_state_runtime

/* Expand to the current VM state backing, as a struct. */
#define JITTER_STATE_BACKING  \
  (jitter_original_state->jitterlispvm_state_backing)

/* Expand to an l-value referrign the named field in the current VM state
   runtime. */
#define JITTER_STATE_RUNTIME_FIELD(field_name)  \
  (JITTER_STATE_RUNTIME.field_name)

/* Expand to an l-value referrign the named field in the current VM state
   backing. */
#define JITTER_STATE_BACKING_FIELD(field_name)  \
  (JITTER_STATE_BACKING.field_name)




/* Nullary macros expanding to per-instruction labels and variable names.
 * ************************************************************************** */

/* These rely on JITTER_SPECIALIZED_INSTRUCTION_MANGLED_NAME , whose definitions
   is machine-generated to be visible from user instruction code.  Using these
   zero-argument macros is more convenient.

   CPP expansion semantics allows these macros to be used even in the expansion
   of other macros. */

/* Expand to the specialized instruction begin label for the current instruction. */
#define JITTER_SPECIALIZED_INSTRUCTION_BEGIN_LABEL  \
  JITTER_SPECIALIZED_INSTRUCTION_BEGIN_LABEL_OF(    \
    JITTER_SPECIALIZED_INSTRUCTION_MANGLED_NAME)

/* Expand to the stringification of
   JITTER_SPECIALIZED_INSTRUCTION_BEGIN_LABEL . */
#define JITTER_SPECIALIZED_INSTRUCTION_BEGIN_LABEL_AS_STRING   \
  JITTER_STRINGIFY(JITTER_SPECIALIZED_INSTRUCTION_BEGIN_LABEL)

/* Expand to the specialized instruction end label for the current instruction. */
#define JITTER_SPECIALIZED_INSTRUCTION_END_LABEL  \
  JITTER_SPECIALIZED_INSTRUCTION_END_LABEL_OF(    \
    JITTER_SPECIALIZED_INSTRUCTION_MANGLED_NAME)

/* Expand to the label at the beginning of the user code for the current
   instruction, in case of non-relocatable instructions; expand to some
   probably unbound identifier otherwise. */
#define JITTER_SPECIALIZED_INSTRUCTION_NON_RELOCATABLE_CODE_LABEL  \
  JITTER_SPECIALIZED_INSTRUCTION_NON_RELOCATABLE_CODE_LABEL_OF(    \
     JITTER_SPECIALIZED_INSTRUCTION_MANGLED_NAME)

/* Expand to the name of the variable defined as
   JITTER_SPECIALIZED_INSTRUCTION_NON_RELOCATABLE_CODE_LABEL .  The variable is
   always stack-allocated and can be safely read from relocated code.  Expand to
   some probably unbound identifier for relocatable instructions. */
#define JITTER_SPECIALIZED_INSTRUCTION_NON_RELOCATABLE_CODE_VARIABLE  \
  JITTER_SPECIALIZED_INSTRUCTION_NON_RELOCATABLE_CODE_VARIABLE_OF(    \
     JITTER_SPECIALIZED_INSTRUCTION_MANGLED_NAME)




/* VM routine termination.
 * ************************************************************************** */

/* Exit the executor function and return to C. */
#ifdef JITTER_REPLICATE
  /* With replication enabled it's important to avoid tail-merging, which is why
     this is a wrapped computed goto rather than a simple goto... */
# define JITTER_EXIT()                                                         \
    do                                                                         \
      {                                                                        \
        JITTER_COMPUTED_GOTO (jitter_saved_exit_non_replicated_code_pointer);  \
      }                                                                        \
    while (false)
#else
  /* ...But in the case of switch dispatching computed gotos may not be usable
     at all, and with direct-threading there is no correctness problem. */
# define JITTER_EXIT()              \
    do                              \
      {                             \
        goto jitter_exit_vm_label;  \
      }                             \
    while (false)
#endif // #ifdef JITTER_REPLICATE




/* VM instruction prolog and epilog.
 * ************************************************************************** */

/* VM instruction prolog. */
#if   defined(JITTER_DISPATCH_SWITCH)
  /* VM instruction prolog: switch dispatching. */
# define JITTER_INSTRUCTION_PROLOG_(name, mangled_name, residual_arity)  \
  case JITTER_CONCATENATE_THREE(JITTER_VM_PREFIX_LOWER_CASE,             \
                                _specialized_instruction_opcode_,        \
                                mangled_name):
#else
  /* VM instruction prolog: every non-switch dispatches. */
# define JITTER_INSTRUCTION_PROLOG_(name, mangled_name, hotness_attribute)  \
{                                                                           \
  JITTER_SPECIALIZED_INSTRUCTION_BEGIN_LABEL_OF(mangled_name):              \
    __attribute__ ((hotness_attribute));
#endif // defined(JITTER_DISPATCH_SWITCH)

/* How many words are used to encode the current specialized instruction, given
   its residual arity.  According to the dispatching model there may be a word
   for the opcode or the thread, or not.
   This relies on JITTER_SPECIALIZED_INSTRUCTION_RESIDUAL_ARITY being defined,
   which is the case when this macro is used as intended, from specialized
   instruction code within the executor. */
#if   defined(JITTER_DISPATCH_SWITCH)            \
   || defined(JITTER_DISPATCH_DIRECT_THREADING)
# define JITTER_SPECIALIZED_INSTRUCTION_WORD_NO          \
    (1 + JITTER_SPECIALIZED_INSTRUCTION_RESIDUAL_ARITY)
#elif defined(JITTER_DISPATCH_MINIMAL_THREADING)
# define JITTER_SPECIALIZED_INSTRUCTION_WORD_NO    \
    JITTER_SPECIALIZED_INSTRUCTION_RESIDUAL_ARITY
#elif defined(JITTER_DISPATCH_NO_THREADING)
# define JITTER_SPECIALIZED_INSTRUCTION_WORD_NO    \
    JITTER_SPECIALIZED_INSTRUCTION_RESIDUAL_ARITY
#else
# error "unknown dispatching model"
#endif

/* Modify the instruction pointer, if any, to skip the appropriate number of
   residuals for the current instruction plus the opcode or thread, if any.
   This relies on JITTER_SPECIALIZED_INSTRUCTION_RESIDUAL_ARITY being defined,
   which is the case when this macro is used as intended, from specialized
   instruction code within the executor.
   FIXME: shall I use the do..while (false) trick here?  This macro is expanded
   a lot of times, and never from user code. */
// FIXME: add a _ suffix to the name.
// FIXME: do not define for no-threading.
#if   defined(JITTER_DISPATCH_SWITCH)            \
   || defined(JITTER_DISPATCH_DIRECT_THREADING)
# define JITTER_SKIP_RESIDUALS_                                       \
  JITTER_SET_IP(jitter_ip + JITTER_SPECIALIZED_INSTRUCTION_WORD_NO);
#elif defined(JITTER_DISPATCH_MINIMAL_THREADING)
# define JITTER_SKIP_RESIDUALS_                                       \
  JITTER_SET_IP(jitter_ip + JITTER_SPECIALIZED_INSTRUCTION_WORD_NO);
#elif defined(JITTER_DISPATCH_NO_THREADING)
# define JITTER_SKIP_RESIDUALS_  \
  /* do nothing. */
#else
# error "unknown dispatching model"
#endif // #if   defined([dispatching model]...

/* A VM instruction epilog. */
#if   defined(JITTER_DISPATCH_SWITCH)
# define JITTER_INSTRUCTION_EPILOG_(name, mangled_name, residual_arity)  \
    JITTER_SKIP_RESIDUALS_;                                              \
    JITTER_BRANCH_TO_IP();
#elif defined(JITTER_DISPATCH_DIRECT_THREADING)
# define JITTER_INSTRUCTION_EPILOG_(name, mangled_name, residual_arity)  \
      JITTER_SKIP_RESIDUALS_;                                            \
      JITTER_BRANCH_TO_IP();                                             \
    }                                                                    \
   JITTER_SPECIALIZED_INSTRUCTION_END_LABEL_OF(mangled_name):            \
     __builtin_unreachable ();
#elif defined(JITTER_DISPATCH_MINIMAL_THREADING) || defined(JITTER_DISPATCH_NO_THREADING)
# define JITTER_INSTRUCTION_EPILOG_(name, mangled_name, residual_arity)  \
       JITTER_SKIP_RESIDUALS_;                                           \
     }                                                                   \
     /* Mark the end of the specialized instruction with a label. */     \
    JITTER_SPECIALIZED_INSTRUCTION_END_LABEL_OF(mangled_name):           \
     /* What follows is unreachable, but serves to prevent GCC from      \
        reordering code across labels.  The final indirect branch,       \
        which of course would not branch anywhere correct if it were     \
        actually executed, serves to force the compiler to keep the      \
        register assignment compatible between this program point,       \
        at the end of VM instructions, with the register assignment      \
        at the beginning of every VM instruction, or even at their end.  \
        From GCC's point of view, this goto * statement may reach any    \
        label in the function whose address I have taken. */             \
     JITTER_CRASH_;                                                      \
     JITTER_PRETEND_TO_UPDATE_IP_;                                       \
     goto * jitter_ip;
#else
# error "unknown dispatching model"
#endif // #if defined(JITTER_DISPATCH_SWITCH)




/* VM branching utility.
 * ************************************************************************** */

// FIXME: shall I move this page and everything below to some other header?

/* After a VM branch-and-link operation is performed the rest of the specialized
   instruction code is unreachable; in other words the return program point is
   the beginning of the VM instruction following the calling VM instruction, and
   not the machine instruction following the *machine* calling instruction.
   This restriction is artificial on machines where VM calls are implemented by
   native branch-and-link or call instructions, but is needed for compatibility
   with simpler dispatching models where the intuitive semantics is not
   implementable.
   It's important that GCC does not assume that the code past the
   branch-and-link is *automatically* unreachable, as would be the case if we
   simply used __builtin_unreachable.
   This is needed, for example, in the case of a branch-and-link at the end of
   the first branch of a two-way conditional within a VM specialized instruction.
   Without this jump the control after returning would fall back into the second
   branch, which would be incorrect.
   The solution is this macro: instead of marking the code as unreachable let's
   simply jump to the end of the specialized instruction after the call.  In the
   common case where there is no code to skip over GCC will optimize the jump
   away.
   This macro relies on JITTER_SPECIALIZED_INSTRUCTION_MANGLED_NAME having the
   correct definition at the time of the macro call, which is true if the macro
   is used as intended within user code for VM instructions. */
#define JITTER_JUMP_TO_SPECIALIZED_INSTRUCTION_END    \
  do                                                  \
    {                                                 \
      goto JITTER_SPECIALIZED_INSTRUCTION_END_LABEL;  \
    }                                                 \
  while (false)




/* Avoiding GCC tail-merging optimizations with replicated code.
 * ************************************************************************** */

/* The problem is performing the equivalent of goto * while avoiding
   tail-merging, which would be disastrous in replicated code when the shared
   jumping instruction happened to be out of the current VM instruction
   block. */

/* Define a trivial wrapper around GNU C's computed goto.  This will be used for
   direct threading, where tail-merging is not a problem. */
#define JITTER_COMPUTED_GOTO_TRIVIAL(target)  \
  do                                          \
    {                                         \
      goto * (target);                        \
    }                                         \
  while (false)

/* Perform the equivalent of a goto * in assembly, using unique inline asm code,
   different from any other.  This is useful to avoid tail-merging, which would
   be disastrous in replicated code when the shared jumping instruction happened
   to be out of the current VM instruction block.

   The gotolabel declared in the inline asm statement is the "jump anywhere"
   label, actually unreachable at run time, but branching to an undetermined
   label; this is enough for GCC to assume the following to be any VM
   instruction.
   The actual jump target, which is always the beginning of the VM instruction,
   is given as the first inline asm input.  The second input, not really used in
   practice but needed to satisfy GCC, is again the "jump anywhere" label as an
   expression -- without it, I've seen GCC 8 snapshots ICE at the following
   __builtin_unreachable () with "error: flow control insn inside a basic
   block".  Omitting __builtin_unreachable () is a no-no, as it enormously
   simplifies the control-flow graph, and provides more opportunities for
   optimization.

   This relies, of course, on the architecture-specific assembly syntax for
   jumping thru a register or memory, along with the appropriate input operand
   constaraint, to be defined in jitter-machine.h .

   JITTER_ASM_COMPUTED_GOTO_TEMPLATE holds the asm template as a string literal,
   using an input argument named _jitter_the_target .
   JITTER_ASM_COMPUTED_GOTO_INPUT_CONSTRAINT is the input constraint for the
   target expression as a string literal, typically "r" but occasionally "rm"
   on CISC architectrues. */
#if    (! defined(JITTER_ASM_COMPUTED_GOTO_TEMPLATE))          \
    || (! defined(JITTER_ASM_COMPUTED_GOTO_INPUT_CONSTRAINT))
  /* Fail if only one of the two architecture-specific macros for computed
     gotos in assembly is defined.  That should never happen. */
# if    defined(JITTER_ASM_COMPUTED_GOTO_TEMPLATE)          \
     || defined(JITTER_ASM_COMPUTED_GOTO_INPUT_CONSTRAINT)
#   error "only one of JITTER_ASM_COMPUTED_GOTO_TEMPLATE"
#   error "and JITTER_ASM_COMPUTED_GOTO_INPUT_CONSTRAINT is defined"
# endif
#else
  /* A few architectures (notably PowerPC) may also need clobbers to jump via
     register, but that is optional.  If no clobbers were defined, define them
     as empty. */
# if ! defined(JITTER_ASM_COMPUTED_GOTO_CLOBBERS)
#   define JITTER_ASM_COMPUTED_GOTO_CLOBBERS /* nothing */
# endif // ! defined(JITTER_ASM_COMPUTED_GOTO_CLOBBERS)
  /* If we arrived here we have all the information to define
     JITTER_COMPUTED_GOTO_IN_ASM . */
# define JITTER_COMPUTED_GOTO_IN_ASM(target)                                   \
    do                                                                         \
      {                                                                        \
        const void *_jitter_the_target = (const void*) (target);               \
        asm goto (JITTER_ASM_DEFECT_DESCRIPTOR                                 \
                  JITTER_ASM_COMMENT_UNIQUE("goto* in assembly to "            \
                                            JITTER_STRINGIFY(target) " "       \
                                            "at %[_jitter_the_target], "       \
                                            "not actually going to "           \
                                            "jitter_dispatch_label at "        \
                                            "%l[jitter_dispatch_label]")       \
                  JITTER_ASM_COMPUTED_GOTO_TEMPLATE                            \
                  : /* outputs */                                              \
                  : [_jitter_the_target]                                       \
                    JITTER_ASM_COMPUTED_GOTO_INPUT_CONSTRAINT                  \
                       (_jitter_the_target)                                    \
                    /* inputs */                                               \
                  : JITTER_ASM_COMPUTED_GOTO_CLOBBERS /* clobbers */           \
                  : jitter_dispatch_label /* gotolabels */);                   \
        /* This is an unconditional branch: the following statement in the     \
           same block is unreachable. */                                       \
        __builtin_unreachable();                                               \
      }                                                                        \
    while (false)
#endif // #if (! defined(JITTER_ASM_COMPUTED_GOTO_TEMPLATE)) || (! defined ...

/* Define JITTER_COMPUTED_GOTO using one of the macros above. */
#ifdef JITTER_REPLICATE
  /* Replication is enabled: use the assembly version of goto * , or the trivial
     version if there is no assembly version available. */
# ifdef JITTER_COMPUTED_GOTO_IN_ASM
#   define JITTER_COMPUTED_GOTO(target)    \
      JITTER_COMPUTED_GOTO_IN_ASM(target)
#   else
#   define JITTER_COMPUTED_GOTO(target)     \
      JITTER_COMPUTED_GOTO_TRIVIAL(target)
# endif // ifdef JITTER_COMPUTED_GOTO_IN_ASM
#else /* replication is disabled */
  /* Replication is disabled: we don't need the hacks above, so in this case we
     can always use the fallback definition, using a native GCC computed goto;
     this might be more efficient with conditionals. */
# define JITTER_COMPUTED_GOTO(target)      \
    JITTER_COMPUTED_GOTO_TRIVIAL(target)
#endif // #ifdef JITTER_REPLICATE




/* VM branching.
 * ************************************************************************** */

/* Set the VM instruction pointer.  This is only defined for dispatching
   models where an instruction pointer exists. */
#if      defined(JITTER_DISPATCH_SWITCH)             \
      || defined(JITTER_DISPATCH_DIRECT_THREADING)   \
      || defined(JITTER_DISPATCH_MINIMAL_THREADING)
# define JITTER_SET_IP(target_pointer)                           \
    do                                                           \
      {                                                          \
        jitter_ip = (const union jitter_word*)(target_pointer);  \
      }                                                          \
    while (false)
#endif // #if      defined(JITTER_DISPATCH_SWITCH) || ...

/* Jump to the current VM instruction pointer.  This is only defined for
   dispatching models where an instruction pointer is actually used.*/
#if      defined(JITTER_DISPATCH_SWITCH)
# define JITTER_BRANCH_TO_IP()                 \
    do                                         \
      {                                        \
        goto jitter_dispatching_switch_label;  \
      }                                        \
    while (false)
#elif    defined(JITTER_DISPATCH_DIRECT_THREADING)   \
      || defined(JITTER_DISPATCH_MINIMAL_THREADING)
# define JITTER_BRANCH_TO_IP()                    \
    do                                            \
      {                                           \
        JITTER_COMPUTED_GOTO(jitter_ip->thread);  \
      }                                           \
    while (false)
#endif // #if   defined(...

/* Branch to a given VM label, represented as appropriate for the dispatching
   model. */
#ifdef JITTER_DISPATCH_NO_THREADING
#define JITTER_BRANCH(target)        \
  do                                 \
    {                                \
      JITTER_COMPUTED_GOTO(target);  \
    }                                \
  while (false)
#else
#define JITTER_BRANCH(target_pointer)  \
  do                                   \
    {                                  \
      JITTER_SET_IP(target_pointer);   \
      JITTER_BRANCH_TO_IP();           \
      __builtin_unreachable (); /* FIXME: this seems beneficial here, differently from the no-threading case; anyway, the problem could be catched with a defect handler, and this is not a guarantee of correctness. */ \
    }                                  \
  while (false)
#endif // #ifdef JITTER_DISPATCH_NO_THREADING

/* _JITTER_PROCEDURE_PROLOG is only used by generated code in callee
   instructions, and is not intended for the user.  The user is only supposed to
   read JITTER_LINK , which holds the value set by _JITTER_PROCEDURE_PROLOG --
   either as defined here or in a machine-specific definition. */
# define __JITTER_PROCEDURE_PROLOG_COMMON(link_lvalue)                     \
    do                                                                     \
      {                                                                    \
        link_lvalue                                                        \
          = (const union jitter_word *)jitter_state_runtime._jitter_link;  \
      }                                                                    \
    while (false)
#if    defined(JITTER_DISPATCH_SWITCH)             \
    || defined(JITTER_DISPATCH_DIRECT_THREADING)   \
    || defined(JITTER_DISPATCH_MINIMAL_THREADING)
# define _JITTER_PROCEDURE_PROLOG(link_lvalue)  \
    __JITTER_PROCEDURE_PROLOG_COMMON(link_lvalue)
#elif    defined(JITTER_DISPATCH_NO_THREADING)
# ifndef JITTER_MACHINE_SUPPORTS_PROCEDURE
#   define _JITTER_PROCEDURE_PROLOG(link_lvalue)  \
      __JITTER_PROCEDURE_PROLOG_COMMON(link_lvalue)
# endif // ifndef JITTER_MACHINE_SUPPORTS_PROCEDURE
#else
# error "unknown dispatching model"
#endif

/* Branch-and-link to a given VM label, represented in a way appropriate for the
   dispatching model.
   A branch-and-link operation unconditionally branches to a VM target, and also
   saves the return address (the beginning of the following VM instruction,
   ignoring any code in the caller instruction past the branch-and-link) in some
   implementation-specific resource, only up to the end of the callee
   instruction.
   The saved return access is accessible in reading from the target VM
   instruction, which *must* be a callee instruction, and only reachable thru
   branch-and-link , as JITTER_LINK .

   This is a generic definition which works everywhere but does not exploit the
   hardware branch target predictor like a machine-specific implementation
   could.  Return operations will easily mispredict.

   This macro is reserved for internal use.  The macro intended for the user
   is named JITTER_BRANCH_AND_LINK , and is only visible from caller
   instructions; this forces the user to correctly declare callers so that
   return labels can be handled in every case. */
#if    defined(JITTER_DISPATCH_SWITCH)             \
    || defined(JITTER_DISPATCH_DIRECT_THREADING)   \
    || defined(JITTER_DISPATCH_MINIMAL_THREADING)
# define JITTER_BRANCH_AND_LINK_INTERNAL(target_rvalue)                      \
    do                                                               \
      {                                                              \
        jitter_state_runtime._jitter_link                            \
          = ((const union jitter_word *)                             \
             (jitter_ip + JITTER_SPECIALIZED_INSTRUCTION_WORD_NO));  \
        JITTER_BRANCH(target_rvalue);                                \
      }                                                              \
    while (false)
#elif    defined(JITTER_DISPATCH_NO_THREADING)
# ifndef JITTER_MACHINE_SUPPORTS_PROCEDURE
#   define JITTER_BRANCH_AND_LINK_INTERNAL(target_rvalue)                              \
      do                                                                       \
        {                                                                      \
          /* Use the return address from the implicit specialized argument */  \
          jitter_state_runtime._jitter_link = JITTER_RETURN_ADDRESS;           \
          JITTER_BRANCH(target_rvalue);                                        \
        }                                                                      \
      while (false)
# endif // ifndef JITTER_MACHINE_SUPPORTS_PROCEDURE
#else
# error "unknown dispatching model"
#endif

/* Define the branch-and-link-with operation, in a way similar to
   JITTER_BRANCH_AND_LINK_INTERNAL just above.  In this case, however, the
   macro is not conditionally defined: a branch-and-link-with doesn't count as a
   call (since it can't return), and therefore the operation is available in any
   VM instruction, even non-callers.  This is why the macro name doesn't begin
   with an underscore. */
#if    defined(JITTER_DISPATCH_SWITCH)                    \
    || defined(JITTER_DISPATCH_DIRECT_THREADING)          \
    || defined(JITTER_DISPATCH_MINIMAL_THREADING)         \
    || (defined(JITTER_DISPATCH_NO_THREADING)             \
        && ! defined(JITTER_MACHINE_SUPPORTS_PROCEDURE))
# define JITTER_BRANCH_AND_LINK_WITH(_jitter_target_rvalue,      \
                                     _jitter_new_link)           \
    do                                                           \
      {                                                          \
        jitter_state_runtime._jitter_link = (_jitter_new_link);  \
        JITTER_BRANCH(_jitter_target_rvalue);                    \
      }                                                          \
    while (false)
#endif

/* Sanity check, useful for writing new ports. */
#if (defined(JITTER_DISPATCH_NO_THREADING)          \
     && defined(JITTER_MACHINE_SUPPORTS_PROCEDURE)  \
     && ! defined(JITTER_BRANCH_AND_LINK_WITH))
# error "The machine claims to support procedures but lacks a definition"
# error "for JITTER_BRANCH_AND_LINK_WITH .  Can't use no-threading."
#endif // #if ... sanity check.

/* An internal definition used for JITTER_RETURN . */
# define _JITTER_RETURN_COMMON(link_rvalue)  \
    do                                       \
      {                                      \
        JITTER_BRANCH(link_rvalue);          \
      }                                      \
    while (false)

/* Return to the caller, using the label provided as the rvalue parameter as the
   destination.  In its generic implementation this is a simple unconditional
   branch, but of course machine-specific implementation will use native return
   or branch-to-link-register instruction, with better branch target prediction
   performance. */
#if    defined(JITTER_DISPATCH_SWITCH)            \
    || defined(JITTER_DISPATCH_DIRECT_THREADING)  \
    || defined(JITTER_DISPATCH_MINIMAL_THREADING)
# define JITTER_RETURN(link_rvalue)  \
    _JITTER_RETURN_COMMON(link_rvalue)
#elif    defined(JITTER_DISPATCH_NO_THREADING)
# ifndef JITTER_MACHINE_SUPPORTS_PROCEDURE
#   define JITTER_RETURN(link_rvalue)  \
      _JITTER_RETURN_COMMON(link_rvalue)
# endif // ifndef JITTER_MACHINE_SUPPORTS_PROCEDURE
#else
# error "unknown dispatching model"
#endif


/* VM conditional non-fast branching.
 * ************************************************************************** */

/* These macros are used internally in replacement instructions, as alternatives
   to fast conditional branches.  They are not very useful for human users, as
   they expand to the same code generated by unconditional branches within
   conditionals; however their direct use is not forbidden. */

/* This macro is only used internally in the following macro definitions in this
   section. */
#define _JITTER_BRANCH_IF(type, operand0, operator, operand1, target)  \
  do                                                                   \
    {                                                                  \
      if ((type) (operand0) operator (type) (operand1))                \
        JITTER_BRANCH (target);                                        \
    }                                                                  \
  while (false)

/* These are the non-fast counterparts of the similarly named macros in
   jitter-fast-branch.h and the machine-specific headers. */
#define JITTER_BRANCH_IF_ZERO(operand, target)            \
  _JITTER_BRANCH_IF(jitter_uint, operand, ==, 0, target)
#define JITTER_BRANCH_IF_NONZERO(operand, target)         \
  _JITTER_BRANCH_IF(jitter_uint, operand, !=, 0, target)
#define JITTER_BRANCH_IF_POSITIVE(operand, target)      \
  _JITTER_BRANCH_IF(jitter_int, operand, >, 0, target)
#define JITTER_BRANCH_IF_NONPOSITIVE(operand, target)    \
  _JITTER_BRANCH_IF(jitter_int, operand, <=, 0, target)
#define JITTER_BRANCH_IF_NEGATIVE(operand, target)      \
  _JITTER_BRANCH_IF(jitter_int, operand, <, 0, target)
#define JITTER_BRANCH_IF_NONNEGATIVE(operand, target)    \
  _JITTER_BRANCH_IF(jitter_int, operand, >=, 0, target)
#define JITTER_BRANCH_IF_EQUAL(operand0, operand1, target)     \
  _JITTER_BRANCH_IF(jitter_int, operand0, ==, operand1, target)
#define JITTER_BRANCH_IF_NOTEQUAL(operand0, operand1, target)  \
  _JITTER_BRANCH_IF(jitter_int, operand0, !=, operand1, target)
#define JITTER_BRANCH_IF_LESS_SIGNED(operand0, operand1, target)  \
  _JITTER_BRANCH_IF(jitter_int, operand0, <, operand1, target)
#define JITTER_BRANCH_IF_LESS_UNSIGNED(operand0, operand1, target)  \
  _JITTER_BRANCH_IF(jitter_uint, operand0, <, operand1, target)
#define JITTER_BRANCH_IF_NOTLESS_SIGNED(operand0, operand1, target)  \
  _JITTER_BRANCH_IF(jitter_int, operand0, >=, operand1, target)
#define JITTER_BRANCH_IF_NOTLESS_UNSIGNED(operand0, operand1, target)  \
  _JITTER_BRANCH_IF(jitter_uint, operand0, >=, operand1, target)
#define JITTER_BRANCH_IF_GREATER_SIGNED(operand0, operand1, target)  \
  _JITTER_BRANCH_IF(jitter_int, operand0, >, operand1, target)
#define JITTER_BRANCH_IF_GREATER_UNSIGNED(operand0, operand1, target)  \
  _JITTER_BRANCH_IF(jitter_uint, operand0, >, operand1, target)
#define JITTER_BRANCH_IF_NOTGREATER_SIGNED(operand0, operand1, target)  \
  _JITTER_BRANCH_IF(jitter_int, operand0, <=, operand1, target)
#define JITTER_BRANCH_IF_NOTGREATER_UNSIGNED(operand0, operand1, target)  \
  _JITTER_BRANCH_IF(jitter_uint, operand0, <=, operand1, target)


#endif // #ifndef JITTER_EXECUTOR_H_
