/* Jitter: VM-independent header for generated interpreters.

   Copyright (C) 2016, 2017 Luca Saiu
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


/* This header is used internally by machine-generated interpreters.  It
   is not for the user to see.

   For example, some functionality defined here may depend on
   JITTER_VM_PREFIX_LOWER_CASE and JITTER_VM_PREFIX_UPPER_CASE, which are
   defined by machine-generated code in a .c file, without polluting the user
   name space. */

#ifndef JITTER_INTERPRETER_PRIVATE_H_
#define JITTER_INTERPRETER_PRIVATE_H_

#include <jitter/jitter-dispatch.h>
#include <jitter/jitter.h>
#include <jitter/jitter-cpp.h>

#ifdef JITTER_HAS_ASSEMBLY
#include <jitter/machine/jitter-machine.h>
#endif // #ifdef JITTER_HAS_ASSEMBLY

#include <jitter/jitter-fast-branch.h>


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




/* Miscellaneous machinery for the interpreter, possibly to move.
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
#define JITTER_COMMENT_IN_ASM(_jitter_string_literal)        \
  asm volatile (JITTER_ASM_COMMENT(_jitter_string_literal))

/* Expand to a string literal containing an asm comment, including containing
   the given text and a unique identifier which will prevent GCC from merging
   different expansions with the same argument. */
#define JITTER_COMMENT_IN_ASM_UNIQUE(_jitter_string_literal)        \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE(_jitter_string_literal))




/* Macros to let GCC see variables as changed.
 * ************************************************************************** */

// FIXME: comment
#define JITTER_MARK_RVALUE_AS_READ_BY_ASSEMBLY(variable)                   \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE("Pretend to read the register or memory variable " \
                                   JITTER_STRINGIFY(variable) " in "          \
                                   "%[_variable]")                            \
                : \
                : [_variable] "X" (variable) \
                : "memory")
#define JITTER_MARK_LVALUE_AS_DEFINED_BY_ASSEMBLY(variable)                   \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE("Pretend to write the register or memory variable " \
                                   JITTER_STRINGIFY(variable) " in "          \
                                   "%[_variable]")                            \
                : [_variable] "=X" (variable))
#define JITTER_MARK_LVALUE_AS_SET_BY_ASSEMBLY(variable)                       \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE("Pretend to read and write the register or memory variable " \
                                   JITTER_STRINGIFY(variable) " in "          \
                                   "%[_variable]")                            \
                : [_variable] "+X" (variable))

#define JITTER_MARK_REGISTER_AS_DEFINED_BY_ASSEMBLY(variable)                     \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE("Pretend to write the register variable " \
                                   JITTER_STRINGIFY(variable) " in "          \
                                   "%[register_variable]")                    \
                : [register_variable] "=r" (variable))
#define JITTER_MARK_REGISTER_AS_SET_BY_ASSEMBLY(variable)                     \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE("Pretend to read and write the register variable " \
                                   JITTER_STRINGIFY(variable) " in "          \
                                   "%[register_variable]")                    \
                : [register_variable] "+r" (variable))

#define JITTER_MARK_MEMORY_AS_DEFINED_BY_ASSEMBLY(variable)   \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE("Pretend to write the memory variable "  \
                                   JITTER_STRINGIFY(variable))               \
                : "=m" (variable))
#define JITTER_MARK_MEMORY_AS_SET_BY_ASSEMBLY(variable)   \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE("Pretend to read and write the memory variable "  \
                                   JITTER_STRINGIFY(variable))               \
                : "+m" (variable))
#define JITTER_MARK_ARRAY_ELEMENT_AS_SET_BY_ASSEMBLY(variable, offset)      \
  asm volatile (JITTER_ASM_COMMENT_UNIQUE("Pretend to read and write a memory word from "  \
                                   JITTER_STRINGIFY(variable)               \
                                   " at offset " JITTER_STRINGIFY(offset))  \
                : "+m" (* (jitter_int*) ((char*) (variable) + (offset))))




/* Macros to let GCC see jumps to indeterminate locations.
 * ************************************************************************** */

/* Expand to a two-statement sequence:
   - some empty inline asm code whose constraints claim to update
     jitter_anywhere_variable ;
   - a jump to jitter_anywhere_variable 's content, reaching one of the labels
     in the current C functions -- GCC cannot discover which one.

   Rationale: there is only one instance of this in the interpreter and it is
   actually unreachable at run time, differently from what asm constraints
   claim.  The role of the unreachable statement is to alter GCC's program
   representation to assume that a jump to any label is possible from it,
   defeating analyses.  This serves to make GCC's register allocation compatible
   across VM instructions, so that the end of any VM instruction matches the
   beginning of any other.

   Notice that jitter_anywhere_variable's declaration requires it to be in
   memory rather than in a register, and every constarint on reinforces this.
   The idea is to reduce register pressure, preventing GCC from wasting a
   register for this variable, which is never touched by actually running
   code. */
#define JITTER_JUMP_ANYWHERE                                      \
  do                                                              \
    {                                                             \
      asm (JITTER_ASM_COMMENT_UNIQUE("Jump anywhere: pretend to"  \
                                     " update %[jitter_anywhere]" \
                                     " before jumping to it")     \
           : [jitter_anywhere] "+m" (jitter_anywhere_variable));  \
      goto *(jitter_anywhere_variable.pointer);                   \
      __builtin_unreachable ();                                   \
    }                                                             \
  while (false)

/* Expand to zero assembly instructions, but with inline asm constraints
   affecting GCC's program representation as if the generated code could either
   jump to the content of jitter_anywhere_variable or fall thru.

   Rationale: see the comment about JITTER_PRETEND_TO_JUMP_ANYWHERE below. */
#define JITTER_PRETEND_TO_POSSIBLY_JUMP_ANYWHERE                                \
  do                                                                            \
    {                                                                           \
      asm goto (JITTER_ASM_COMMENT_UNIQUE("Pretend to possibly jump "           \
                                          "to %l[jitter_jump_anywhere_label] "  \
                                          "based on %[jitter_anywhere]")        \
                : /* outputs */                                                 \
                : /* inputs */ [jitter_anywhere] "m" (jitter_anywhere_variable) \
                : /* clobbers */                                                \
                : /* jump destinations */ jitter_jump_anywhere_label);          \
    }                                                                           \
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




/* VM program termination.
 * ************************************************************************** */

/* Exit the "interpreter" function and return to C. */
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




/* Sequencing.
 * ************************************************************************** */

/* The implemenation of this part is still tentative.  The idea is introducing
   (empty) inline asm statements with dependencies on a dummy variable, in order
   to force GCC not to move statements across labels when the generated code
   ordering is critical, particularly at the beginning of VM instructions. */
#define JITTER_SEQUENCE_POINT_ACTUAL \
  do \
    { \
      asm volatile (JITTER_ASM_COMMENT_UNIQUE("foo %[jitter_anywhere]") \
                    : [jitter_anywhere] "+m" (jitter_anywhere_variable) \
                    : \
                    : "memory"); \
      JITTER_PRETEND_TO_POSSIBLY_JUMP_ANYWHERE; \
    } \
  while (false)

/* FIXME: this is disabled.  Sequence points seem to not matter with GCC 6, 7
   and 8, and make the generated code worse.  The code still deserves to be kept
   around for the time being. */
#define JITTER_SEQUENCE_POINT             \
  do                                      \
    {                                     \
       /*JITTER_SEQUENCE_POINT_ACTUAL;*/  \
    }                                     \
  while (false)



/* VM instruction prolog and epilog.
 * ************************************************************************** */

/* VM instruction prolog. */
#if   defined(JITTER_DISPATCH_SWITCH)
  /* VM instruction prolog: switch dispatching. */
# define JITTER_INSTRUCTION_PROLOG(name, mangled_name, residual_arity)  \
  case JITTER_CONCATENATE_THREE(JITTER_VM_PREFIX_LOWER_CASE,            \
                                _specialized_instruction_opcode_,       \
                                mangled_name):                          \
    JITTER_COMMENT_IN_ASM_UNIQUE("Specialized instruction "             \
                                 JITTER_STRINGIFY(name)                 \
                                 ": begin");
#else
  /* VM instruction prolog: every non-switch dispatches. */
# define JITTER_INSTRUCTION_PROLOG(name, mangled_name, hotness_attribute)   \
{ \
JITTER_SEQUENCE_POINT; \
  JITTER_SPECIALIZED_INSTRUCTION_BEGIN_LABEL_OF(mangled_name):             \
    __attribute__ ((hotness_attribute));                                   \
JITTER_SEQUENCE_POINT; \
JITTER_COMMENT_IN_ASM("Specialized instruction " JITTER_STRINGIFY(name)  \
                      ": begin");
#endif // defined(JITTER_DISPATCH_SWITCH)

/* How many words are used to encode the current specialized instruction, given
   its residual arity.  According to the dispatching model there may be a word
   for the opcode or the thread, or not.
   This relies on JITTER_SPECIALIZED_INSTRUCTION_RESIDUAL_ARITY being defined,
   which is the case when this macro is used as intended, from specialized
   instruction code within the interpreter. */
#if   defined(JITTER_DISPATCH_SWITCH)           \
   || defined(JITTER_DISPATCH_DIRECT_THREADING)
# define JITTER_SPECIALIZED_INSTRUCTION_WORD_NO \
    (1 + JITTER_SPECIALIZED_INSTRUCTION_RESIDUAL_ARITY)
#elif defined(JITTER_DISPATCH_MINIMAL_THREADING)
# define JITTER_SPECIALIZED_INSTRUCTION_WORD_NO \
    JITTER_SPECIALIZED_INSTRUCTION_RESIDUAL_ARITY
#elif defined(JITTER_DISPATCH_NO_THREADING)
# define JITTER_SPECIALIZED_INSTRUCTION_WORD_NO \
    JITTER_SPECIALIZED_INSTRUCTION_RESIDUAL_ARITY
#else
# error "unknown dispatching model"
#endif

/* Modify the instruction pointer, if any, to skip the appropriate number of
   residuals for the current instruction plus the opcode or thread, if any.
   This relies on JITTER_SPECIALIZED_INSTRUCTION_RESIDUAL_ARITY being defined,
   which is the case when this macro is used as intended, from specialized
   instruction code within the interpreter.
   FIXME: shall I use the do..while (false) trick here?  This macro is expanded
   a lot of times, and never from user code. */
#if   defined(JITTER_DISPATCH_SWITCH)                          \
   || defined(JITTER_DISPATCH_DIRECT_THREADING)
# define JITTER_SKIP_RESIDUALS                                 \
  JITTER_SET_IP(ip + JITTER_SPECIALIZED_INSTRUCTION_WORD_NO);
#elif defined(JITTER_DISPATCH_MINIMAL_THREADING)
# define JITTER_SKIP_RESIDUALS                                 \
  JITTER_SET_IP(ip + JITTER_SPECIALIZED_INSTRUCTION_WORD_NO);
#elif defined(JITTER_DISPATCH_NO_THREADING)
# define JITTER_SKIP_RESIDUALS                                 \
  /* do nothing. */
#else
# error "unknown dispatching model"
#endif // #if   defined([dispatching model]...

/* A VM instruction epilog. */
#if   defined(JITTER_DISPATCH_SWITCH)
# define JITTER_INSTRUCTION_EPILOG(name, mangled_name, residual_arity)  \
    JITTER_SKIP_RESIDUALS;                                              \
    JITTER_BRANCH_TO_IP();                                              \
    JITTER_COMMENT_IN_ASM_UNIQUE("Specialized instruction "             \
                                 JITTER_STRINGIFY(name)                 \
                                 ": after jumping back to switch");
#elif defined(JITTER_DISPATCH_DIRECT_THREADING)
# define JITTER_INSTRUCTION_EPILOG(name, mangled_name, residual_arity)  \
      JITTER_SKIP_RESIDUALS;                                            \
      JITTER_BRANCH_TO_IP();                                            \
      JITTER_COMMENT_IN_ASM("Specialized instruction "                  \
                            JITTER_STRINGIFY(name) ": end");            \
} \
    JITTER_SPECIALIZED_INSTRUCTION_END_LABEL_OF(mangled_name):          \
      __builtin_unreachable ();
#elif defined(JITTER_DISPATCH_MINIMAL_THREADING)
# define JITTER_INSTRUCTION_EPILOG(name, mangled_name, residual_arity)  \
      JITTER_SKIP_RESIDUALS;                                            \
JITTER_SEQUENCE_POINT; \
    JITTER_COMMENT_IN_ASM("Specialized instruction "                    \
                          JITTER_STRINGIFY(name) ": end");              \
} \
    JITTER_SPECIALIZED_INSTRUCTION_END_LABEL_OF(mangled_name):          \
JITTER_SEQUENCE_POINT; \
      JITTER_PRETEND_TO_JUMP_ANYWHERE;
#elif defined(JITTER_DISPATCH_NO_THREADING)
# define JITTER_INSTRUCTION_EPILOG(name, mangled_name, residual_arity)  \
JITTER_SEQUENCE_POINT; \
    JITTER_COMMENT_IN_ASM("Specialized instruction "                    \
                          JITTER_STRINGIFY(name) ": end");              \
} \
JITTER_SEQUENCE_POINT; \
    JITTER_SPECIALIZED_INSTRUCTION_END_LABEL_OF(mangled_name):          \
JITTER_SEQUENCE_POINT; \
      JITTER_PRETEND_TO_JUMP_ANYWHERE;
#else
# error "unknown dispatching model"
#endif // #if   defined(JITTER_DISPATCH_DIRECT_THREADING)




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
# define JITTER_COMPUTED_GOTO_IN_ASM(target)                               \
    do                                                                     \
      {                                                                    \
        const void *_jitter_the_target = (const void*) (target);           \
        asm goto (JITTER_ASM_COMMENT_UNIQUE("goto* in assembly")           \
                  JITTER_ASM_COMPUTED_GOTO_TEMPLATE                        \
                  : /* outputs */                                          \
                  :   [_jitter_the_target]                                 \
                         JITTER_ASM_COMPUTED_GOTO_INPUT_CONSTRAINT         \
                            (_jitter_the_target)                           \
                    , [_jitter_jump_anywhere_label]                        \
                         "X" (&& jitter_jump_anywhere_label) /* inputs */  \
                  : JITTER_ASM_COMPUTED_GOTO_CLOBBERS /* clobbers */       \
                  : jitter_jump_anywhere_label /* gotolabels */);          \
        __builtin_unreachable ();                                          \
      }                                                                    \
    while (false)
#endif // #if (! defined(JITTER_ASM_COMPUTED_GOTO_TEMPLATE)) || (! defined ...

/* Implement a version of goto * with complicated control flow expressed by
   inline asm constraints associated to empty code, making it difficult for GCC
   to tail-merge.
   This is a fallback (relatively fragile) solution, for architectures not
   providing enough information to define JITTER_COMPUTED_GOTO_IN_ASM . */
#define JITTER_COMPUTED_GOTO_WEIRD_COUNT(_jitter_target_expression, count)      \
  do                                                                            \
    {                                                                           \
      void *_jitter_target = (void*) (_jitter_target_expression);               \
      volatile jitter_int _jitter_fake_condition;                               \
      JITTER_MARK_MEMORY_AS_DEFINED_BY_ASSEMBLY(_jitter_fake_condition);        \
    JITTER_CONCATENATE_TWO(_before__, count):                                   \
      asm goto (JITTER_ASM_COMMENT_UNIQUE("Pretend to possibly branch forward " \
                                          "based on %[_jitter_target] and "     \
                                          "%[_jitter_fake_condition]")          \
                : /* outputs */                                                 \
                :   [_jitter_target] "r" (_jitter_target)                       \
                    , [_jitter_fake_condition] "m" (_jitter_fake_condition)     \
                    /* inputs */                                                \
                : /* clobbers */                                                \
                : JITTER_CONCATENATE_TWO(_after__, count) /* gotolabels */);    \
      JITTER_MARK_REGISTER_AS_SET_BY_ASSEMBLY(_jitter_target);                  \
      JITTER_MARK_MEMORY_AS_SET_BY_ASSEMBLY(_jitter_fake_condition);            \
      goto * _jitter_target;                                                    \
    JITTER_CONCATENATE_TWO(_after__, count):                                    \
      asm goto (JITTER_ASM_COMMENT_UNIQUE("Pretend to possibly jump back "      \
                                          "based on %[_jitter_fake_condition]") \
                : /* outputs */                                                 \
                : [_jitter_fake_condition] "m" (_jitter_fake_condition)         \
                  /* inputs */                                                  \
                : /* clobbers*/                                                 \
                : JITTER_CONCATENATE_TWO(_before__, count) /* gotolabels */);   \
      __builtin_unreachable ();                                                 \
    }                                                                           \
  while (false)

/* Define JITTER_COMPUTED_GOTO_WEIRD to instantiate
   JITTER_COMPUTED_GOTO_WEIRD_COUNT using a fresh count at every expansion. */
#define JITTER_COMPUTED_GOTO_WEIRD(_jitter_target_expression)     \
  JITTER_COMPUTED_GOTO_WEIRD_COUNT(_jitter_target_expression,     \
                                   JITTER_INTEGER_LITERAL_UNIQUE)

/* Define JITTER_COMPUTED_GOTO using one of the macros above. */
#ifdef JITTER_REPLICATE
  /* Replication is enabled: use the assembly version of goto * , or the weird
     version if there is no assembly version available. */
# ifdef JITTER_COMPUTED_GOTO_IN_ASM
#   define JITTER_COMPUTED_GOTO(target)  \
      JITTER_COMPUTED_GOTO_IN_ASM(target)
#   else
#   define JITTER_COMPUTED_GOTO(target)  \
      JITTER_COMPUTED_GOTO_WEIRD(target)
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

/* Expand to a statement jumping back to the dispatching switch.  This only
   makes sense for switch dispatching, and is not defiend for any other
   dispatching model. */
#ifdef JITTER_DISPATCH_SWITCH
# define JITTER_JUMP_TO_SWITCH                 \
    do                                         \
      {                                        \
        goto jitter_dispatching_switch_label;  \
      }                                        \
    while (false)

#endif // #ifdef JITTER_DISPATCH_SWITCH

/* Set the VM instruction pointer.  This is only defined for dispatching
   models where an instruction pointer exists. */
#if      defined(JITTER_DISPATCH_SWITCH)             \
      || defined(JITTER_DISPATCH_DIRECT_THREADING)   \
      || defined(JITTER_DISPATCH_MINIMAL_THREADING)
# define JITTER_SET_IP(target_pointer)                    \
    do                                                    \
      {                                                   \
        ip = (const union jitter_word*)(target_pointer);  \
      }                                                   \
    while (false)
#endif // #if      defined(JITTER_DISPATCH_SWITCH) || ...

/* Jump to the current VM instruction pointer.  This is only defined for
   dispatching models where an instruction pointer exists.*/
#if      defined(JITTER_DISPATCH_SWITCH)
# define JITTER_BRANCH_TO_IP()  \
    do                          \
      {                         \
        JITTER_JUMP_TO_SWITCH;  \
      }                         \
    while (false)
#elif    defined(JITTER_DISPATCH_DIRECT_THREADING)   \
      || defined(JITTER_DISPATCH_MINIMAL_THREADING)
# define JITTER_BRANCH_TO_IP()             \
    do                                     \
      {                                    \
        JITTER_COMPUTED_GOTO(ip->thread);  \
      }                                    \
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

   As usual every identifier starting with '_' is reserved to the
   implementation.  The macro intended for the user has no initial underscore,
   and is only visible from caller instructions; this forces the user to
   correctly declare callers so that return labels can be handled in every
   case. */
#if    defined(JITTER_DISPATCH_SWITCH)             \
    || defined(JITTER_DISPATCH_DIRECT_THREADING)   \
    || defined(JITTER_DISPATCH_MINIMAL_THREADING)
# define _JITTER_BRANCH_AND_LINK(target_rvalue)               \
    do                                                        \
      {                                                       \
        jitter_state_runtime._jitter_link                     \
          = ((const union jitter_word *)                      \
             (ip + JITTER_SPECIALIZED_INSTRUCTION_WORD_NO));  \
        JITTER_BRANCH(target_rvalue);                         \
      }                                                       \
    while (false)
#elif    defined(JITTER_DISPATCH_NO_THREADING)
# ifndef JITTER_MACHINE_SUPPORTS_PROCEDURE
#   define _JITTER_BRANCH_AND_LINK(target_rvalue)                              \
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




/* PC-relative address poisoning.
 * ************************************************************************** */

/* Using PC-relative addressing within instruction bodies when replicating
   always leads to memory accesses at the incorrect places, and we want to
   prevent that.  It's better to fail at link time instead of generating wrong
   code which will crash later, or introduce subtle bugs.

   This is a heavyweight solution yielding big executables and heavily stressing
   as and ld, potentially to an extent not suitable for production use; but it's
   good to be able to build a testing executable, and have the build fail if
   there will be problem; if not, an efficient executable can be built in a
   second step.

   By inserting useless data just larger than the maximum allowed value for
   PC-relative offsets we guarantee that any code using PC-relative loads or
   stores will fail to link.

   PC-relative offsets are signed in all the architectures I know except SH
   ( mov.w @(disp,pc),rn and mov.l @(disp,pc),rn : unfortunately they are the
   useful cases here) but they may be implicitly scaled; the actual range is
   architecture-dependent. */
#ifdef JITTER_REPLICATE
# define JITTER_POISON_PC_RELATIVE_ADDRESSING(size)           \
    do                                                        \
      {                                                       \
        asm volatile ("#.section .bss\n"                      \
                      ".section .rodata\n"                    \
                      "#.data\n"                              \
                      "# .data # .bss\n"                      \
                      ".skip (" JITTER_STRINGIFY(size) ")\n"  \
                      ".text");                               \
      }                                                       \
    while (false)
#else
# define JITTER_POISON_PC_RELATIVE_ADDRESSING(size)                        \
    do                                                                     \
      {                                                                    \
        /* Do nothing: there is nothing to poison without replication. */  \
      }                                                                    \
    while (false)
#endif // #ifdef JITTER_REPLICATE

#endif // #ifndef JITTER_INTERPRETER_PRIVATE_H_
