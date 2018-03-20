/* Jitter: fast-branch header.

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


#ifndef JITTER_FAST_BRANCH_H_
#define JITTER_FAST_BRANCH_H_


/* Check whether patch-ins are needed, and supported on this machine.
 * ************************************************************************** */

/* This amond the rest includes <jitter/machine/jitter-machine.h> , which is
   enough to see the definition of JITTER_MACHINE_SUPPORTS_BRANCH_AND_LINK used
   below. */
#include <jitter/jitter-patch-in.h>




/* Fallback unconditional fast branch definitions.
 * ************************************************************************** */

/* VM specifications use fast branching macros, but these are not implemented on
   every machine, and even on machines supporting them the fast version is not
   compatible with every dispatching model. */

#ifdef JITTER_HAVE_PATCH_IN
/* FIXME: comment.

   Notice that some of the macros used here as the arguments of
   JITTER_FAST_UNCONDITIONAL_BRANCH_PLACEHOLDER are not defined yet at the time
   of the definition of JITTER_BRANCH_FAST , but will be defined before
   JITTER_BRANCH_FAST is called.
   This is the case for JITTER_FAST_BRANCH_PREFIX ,
   JITTER_SPECIALIZED_INSTRUCTION_OPCODE and
   JITTER_SPECIALIZED_INSTRUCTION_MANGLED_NAME .

   I am not personally a fan CPP or of its macro evaluation order, but it works
   to my advantage in this case.  Any static-scoping integralist coming across
   this comment is welcome to propose a nicer solution *not* adding irrelevant
   arguments everywhere. */
# define JITTER_BRANCH_FAST(index) \
  JITTER_FAST_UNCONDITIONAL_BRANCH_PLACEHOLDER(index)
#else /* JITTER_HAVE_PATCH_IN is not defined. */
// FIXME: comment.
# define JITTER_BRANCH_FAST(label) \
  JITTER_BRANCH(label)
#endif // #ifdef JITTER_HAVE_PATCH_IN




/* Fallback conditional fast branch definitions.
 * ************************************************************************** */

/* Machines for which fast branch support is not fully implemented use an
   ordinary C conditional statement containing an unconditional fast branch. */

/* This auxiliary macro factors the common part of fallback fast-branch
   conditionals.  It's not for the user to call directly. */
#define JITTER_BRANCH_FAST_CONDITIONAL_FALLBACK(expression, target)  \
  do                                                                 \
    {                                                                \
      if (expression)                                                \
        JITTER_BRANCH_FAST (target);                                 \
    }                                                                \
  while (false)

/* Fast branch if the given operand is zero. */
#if ! defined(JITTER_HAVE_PATCH_IN) || ! defined(JITTER_BRANCH_FAST_IF_ZERO)
# define JITTER_BRANCH_FAST_IF_ZERO(operand0, target)                 \
  JITTER_BRANCH_FAST_CONDITIONAL_FALLBACK((operand0) == 0, (target))
#endif

/* Fast branch if the given operand is nonzero. */
#if ! defined(JITTER_HAVE_PATCH_IN) || ! defined(JITTER_BRANCH_FAST_IF_NONZERO)
# define JITTER_BRANCH_FAST_IF_NONZERO(operand0, target)              \
  JITTER_BRANCH_FAST_CONDITIONAL_FALLBACK((operand0) != 0, (target))
#endif

/* FIXME: comment. */
#if ! defined(JITTER_HAVE_PATCH_IN) || ! defined(JITTER_BRANCH_FAST_IF_POSITIVE)
# define JITTER_BRANCH_FAST_IF_POSITIVE(operand0, target)                 \
  JITTER_BRANCH_FAST_CONDITIONAL_FALLBACK(((jitter_int) (operand0)) < 0,  \
                                          (target))
#endif

/* FIXME: comment. */
#if ! defined(JITTER_HAVE_PATCH_IN) || ! defined(JITTER_BRANCH_FAST_IF_NONPOSITIVE)
# define JITTER_BRANCH_FAST_IF_NONPOSITIVE(operand0, target)               \
  JITTER_BRANCH_FAST_CONDITIONAL_FALLBACK(((jitter_int) (operand0)) >= 0,  \
                                          (target))
#endif

/* FIXME: comment. */
#if ! defined(JITTER_HAVE_PATCH_IN) || ! defined(JITTER_BRANCH_FAST_IF_NEGATIVE)
# define JITTER_BRANCH_FAST_IF_NEGATIVE(operand0, target)                 \
  JITTER_BRANCH_FAST_CONDITIONAL_FALLBACK(((jitter_int) (operand0)) < 0,  \
                                          (target))
#endif

/* FIXME: comment. */
#if ! defined(JITTER_HAVE_PATCH_IN) || ! defined(JITTER_BRANCH_FAST_IF_NONNEGATIVE)
# define JITTER_BRANCH_FAST_IF_NONNEGATIVE(operand0, target)             \
  JITTER_BRANCH_FAST_CONDITIONAL_FALLBACK((jitter_int) (operand0) >= 0,  \
                                          (target))
#endif

/* FIXME: comment. */
#if ! defined(JITTER_HAVE_PATCH_IN) || ! defined(JITTER_BRANCH_FAST_IF_EQUAL)
# define JITTER_BRANCH_FAST_IF_EQUAL(operand0, operand1, target)     \
  JITTER_BRANCH_FAST_CONDITIONAL_FALLBACK((operand0) == (operand1),  \
                                          (target))
#endif

/* FIXME: comment. */
#if ! defined(JITTER_HAVE_PATCH_IN) || ! defined(JITTER_BRANCH_FAST_IF_NOTEQUAL)
# define JITTER_BRANCH_FAST_IF_NOTEQUAL(operand0, operand1, target)  \
  JITTER_BRANCH_FAST_CONDITIONAL_FALLBACK((operand0) != (operand1),  \
                                          (target))
#endif

/* FIXME: comment. */
#if ! defined(JITTER_HAVE_PATCH_IN) || ! defined(JITTER_BRANCH_FAST_IF_LESS_SIGNED)
# define JITTER_BRANCH_FAST_IF_LESS_SIGNED(operand0, operand1, target)  \
  JITTER_BRANCH_FAST_CONDITIONAL_FALLBACK((jitter_int) (operand0)       \
                                          < (jitter_int) (operand1),    \
                                          (target))
#endif
/* FIXME: comment. */
#if ! defined(JITTER_HAVE_PATCH_IN) || ! defined(JITTER_BRANCH_FAST_IF_LESS_UNSIGNED)
# define JITTER_BRANCH_FAST_IF_LESS_UNSIGNED(operand0, operand1, target)  \
  JITTER_BRANCH_FAST_CONDITIONAL_FALLBACK((jitter_uint) (operand0)        \
                                          < (jitter_uint) (operand1),     \
                                          (target))
#endif

/* FIXME: comment. */
#if ! defined(JITTER_HAVE_PATCH_IN) || ! defined(JITTER_BRANCH_FAST_IF_NOTLESS_SIGNED)
# define JITTER_BRANCH_FAST_IF_NOTLESS_SIGNED(operand0, operand1, target)  \
  JITTER_BRANCH_FAST_CONDITIONAL_FALLBACK((jitter_int) (operand0)          \
                                          >= (jitter_int) (operand1),      \
                                          (target))
#endif
/* FIXME: comment. */
#if ! defined(JITTER_HAVE_PATCH_IN) || ! defined(JITTER_BRANCH_FAST_IF_NOTLESS_UNSIGNED)
# define JITTER_BRANCH_FAST_IF_NOTLESS_UNSIGNED(operand0, operand1, target)  \
  JITTER_BRANCH_FAST_CONDITIONAL_FALLBACK((jitter_uint) (operand0)           \
                                          >= (jitter_uint) (operand1),       \
                                          (target))
#endif

/* FIXME: comment. */
#if ! defined(JITTER_HAVE_PATCH_IN) || ! defined(JITTER_BRANCH_FAST_IF_GREATER_SIGNED)
# define JITTER_BRANCH_FAST_IF_GREATER_SIGNED(operand0, operand1, target)       \
  JITTER_BRANCH_FAST_CONDITIONAL_FALLBACK((jitter_int) (operand0)               \
                                          > (jitter_int) (operand1), (target))
#endif
/* FIXME: comment. */
#if ! defined(JITTER_HAVE_PATCH_IN) || ! defined(JITTER_BRANCH_FAST_IF_GREATER_UNSIGNED)
# define JITTER_BRANCH_FAST_IF_GREATER_UNSIGNED(operand0, operand1, target)      \
  JITTER_BRANCH_FAST_CONDITIONAL_FALLBACK((jitter_uint) (operand0)               \
                                          > (jitter_uint) (operand1), (target))
#endif

/* FIXME: comment. */
#if ! defined(JITTER_HAVE_PATCH_IN) || ! defined(JITTER_BRANCH_FAST_IF_NOTGREATER_SIGNED)
# define JITTER_BRANCH_FAST_IF_NOTGREATER_SIGNED(operand0, operand1, target)    \
  JITTER_BRANCH_FAST_CONDITIONAL_FALLBACK((jitter_int) (operand0)               \
                                          <= (jitter_int) (operand1), (target))
#endif
/* FIXME: comment. */
#if ! defined(JITTER_HAVE_PATCH_IN) || ! defined(JITTER_BRANCH_FAST_IF_NOTGREATER_UNSIGNED)
# define JITTER_BRANCH_FAST_IF_NOTGREATER_UNSIGNED(operand0, operand1, target)  \
  JITTER_BRANCH_FAST_CONDITIONAL_FALLBACK((jitter_uint) (operand0)              \
                                          <= (jitter_uint) (operand1),          \
                                          (target))
#endif

/* This is convenient to have as a macroexpansion of conditions known to be
   false.  If does not need to be defined conditionally, as this one definition
   suffices for every configuration. */
#define JITTER_BRANCH_FAST_IF_NEVER_UNARY(operand, target)  \
  do                                                        \
    {                                                       \
      /* Do nothing: in particular, don't branch. */        \
    }                                                       \
  while (false)

/* Similar to JITTER_BRANCH_FAST_IF_NEVER_UNARY , but always branching. */
#define JITTER_BRANCH_FAST_IF_ALWAYS_UNARY(operand, target)  \
  do                                                         \
    {                                                        \
      JITTER_BRANCH_FAST(target);                            \
    }                                                        \
  while (false)




/* Fallback fast branch-and-link definition.
 * ************************************************************************** */

/* Provide a fallback definition of _JITTER_BRANCH_AND_LINK_FAST , if needed.
   The definition will either use the slow _JITTER_BRANCH_AND_LINK if fast
   labels are not used in this configuration, or use a generic branch-and-link
   implementation but do the actual branch as a fast branch.

   No fallback is provided if the machine already defiens its own
   _JITTER_BRANCH_AND_LINK_FAST : this means that the dispatching model is
   no-threading, and the machine supports both patch-ins and procedures.

   The user should never call _JITTER_BRANCH_AND_LINK_FAST , or even
   _JITTER_BRANCH_AND_LINK .  Suitable user macros, without the initial
   underscore, are automatically defined to be only visible in caller
   instructions .  This forces the user to correctly declare callers so that
   return labels can be handled in every case. */

/* Provide a definition of JITTER_BRANCH_AND_LINK_FAST if needed. */
#if ! defined(JITTER_DISPATCH_NO_THREADING)
  /* With any dispatching model different from no-threading fast branches revert
     to generic slow branches; in the same way fast branch-and-link operations
     revert to generic slow branch-and-link operations. */
# define _JITTER_BRANCH_AND_LINK_FAST(target) \
  _JITTER_BRANCH_AND_LINK(target)
#elif ! defined(JITTER_HAVE_PATCH_IN)
# if defined(JITTER_MACHINE_SUPPORTS_PROCEDURE)
    /* Currently machine-specific support must also provide patch-ins if it
       provides procedures. */
#   error "the machine supports procedures but not patch-ins"
# else
    /* The machine supports neither patch-ins, nor procedures.  We can use the
       generic implementation of branch-and-link with slow branches. */
#   define _JITTER_BRANCH_AND_LINK_FAST(target) \
      _JITTER_BRANCH_AND_LINK(target)
# endif // if defined(JITTER_MACHINE_SUPPORTS_PROCEDURE)
#elif ! defined(JITTER_MACHINE_SUPPORTS_PROCEDURE)
  /* The machine supports patch-ins, but not procedures.  The best we can do is
     using a generic branch-and-link implementation (relying on a hidden
     residual parameter for caller instructions) which performs the actual
     branch with a fast branch. */
# define _JITTER_BRANCH_AND_LINK_FAST(target)                                \
    do                                                                       \
      {                                                                      \
        /* Use the return address from the implicit specialized argument */  \
        jitter_state_runtime._jitter_link = JITTER_RETURN_ADDRESS;           \
        JITTER_BRANCH_FAST(target);                                          \
        __builtin_unreachable ();                                            \
      }                                                                      \
    while (false)
#elif ! defined(_JITTER_BRANCH_AND_LINK_FAST)
# error "The machine claims to support procedures but _JITTER_BRANCH_AND_LINK_FAST is not defiend."
#endif




/* Ignore the rest of this header if not using patch-ins.
 * ************************************************************************** */

/* The rest of this header expands to nothing if fast branches are not supported
   in this configuration.  The previous CPP includes suffices to make the
   JITTER_HAVE_PATCH_IN visible, if any. */

#ifdef JITTER_HAVE_PATCH_IN




/* Include headers.
 * ************************************************************************** */

#include <stdio.h>
#include <stdbool.h>

#include <jitter/jitter.h>
#include <jitter/jitter-cpp.h>




/* Fast-branch types.
 * ************************************************************************** */

/* The type of a fast jump as a patch-in case, each corresponding to different
   assembly non-zero natural code to insert in assembly code.

   These are CPP macros rather than enum cases because the case values are used
   in inline assembly code (actually data), where a known value is used.  An
   assembly "immediate" constraint is not usable, as in some platforms immediate
   literals are always prefixed by a character such as $ or #, which is not
   needed or correct in data definitions.

   This complication is a pity: GCC documents the "x86 Operand Modifiers" as
   non-portable; having the equivalent of the 'c' modifier on every platform
   would allow for cleaner code. */
#define JITTER_PATCH_IN_CASE_FAST_BRANCH_UNCONDITIONAL                    1
#define JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_ZERO                 2
#define JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NONZERO              3

#define JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_POSITIVE             4
#define JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NONPOSITIVE          5
#define JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NEGATIVE             6
#define JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NONNEGATIVE          7

#define JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_EQUAL                8
#define JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NOTEQUAL             9
#define JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_LESS_UNSIGNED        10
#define JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_LESS_SIGNED          11
#define JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NOTGREATER_UNSIGNED  12
#define JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NOTGREATER_SIGNED    13
#define JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_GREATER_UNSIGNED     14
#define JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_GREATER_SIGNED       15
#define JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NOTLESS_UNSIGNED     16
#define JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_NOTLESS_SIGNED       17

#define JITTER_PATCH_IN_CASE_FAST_BRANCH_CONDITIONAL_ANY                  18

#define JITTER_PATCH_IN_CASE_FAST_BRANCH_BRANCH_AND_LINK                  19
/* /\* These should be extended in the future with floating-point conditionals. *\/ */




/* Fast-branch macros expanding to patch-ins within the intepreter.
 * ************************************************************************** */

// FIXME: add comments.
// FIXME: move?  The separation between fast branches and patch-ins feels arbitrary in many places.

#define JITTER_FAST_UNCONDITIONAL_BRANCH_PLACEHOLDER(residual_index)  \
  do                                                                  \
    {                                                                 \
/*asm ("" : : : "memory"); / * FIXME: remove now, unless really needed. * / */     \
      JITTER_PATCH_IN_PLACEHOLDER_GOTO_(                              \
         JITTER_PATCH_IN_SIZE_FAST_BRANCH_UNCONDITIONAL,              \
         JITTER_PATCH_IN_CASE_FAST_BRANCH_UNCONDITIONAL,              \
         residual_index,                                              \
         0, 0, 0); /* Unused for this case. */                        \
      __builtin_unreachable ();                                       \
    }                                                                 \
  while (false)


#endif // #ifdef JITTER_HAVE_PATCH_IN
#endif // #ifndef JITTER_FAST_BRANCH_H_
