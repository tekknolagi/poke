/* Jitter: patch-in header.

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


#ifndef JITTER_PATCH_IN_H_
#define JITTER_PATCH_IN_H_


/* Do nothing if not using patch-ins.
 * ************************************************************************** */

/* Include macro definitions about whether we have a machine file, and about the
   dispatching model. */
#include <jitter/jitter-config.h>
#include <jitter/jitter-dispatch.h>

/* First include the machine header which may contain a CPP definition of
   JITTER_MACHINE_SUPPORTS_PATCH_IN ; if we don't even have a machine header or
   it's disabled (which is to say if JITTER_HAS_ASSEMBLY is not defined), or if
   the dispatching model is different from no-threading then patch-ins are
   always disabled. */
#if defined(JITTER_HAS_ASSEMBLY) && defined(JITTER_DISPATCH_NO_THREADING)
# include <jitter/jitter-machine-common.h>
# include <jitter/machine/jitter-machine.h>
# ifdef JITTER_MACHINE_SUPPORTS_PATCH_IN
#   define JITTER_HAVE_PATCH_IN 1
# endif // #ifdef JITTER_MACHINE_SUPPORTS_PATCH_IN
#endif //#if defined(JITTER_HAS_ASSEMBLY) && defined(JITTER_DISPATCH_NO_THREADING)

/* The rest of this header expands to nothing if patch-ins are not supported in
   this configuration. */
#ifdef JITTER_HAVE_PATCH_IN




/* Include headers.
 * ************************************************************************** */

#include <stdio.h>
#include <stdbool.h>

#include <jitter/jitter.h>
#include <jitter/jitter-cpp.h>




/* Prefix-dependent names for globals.
 * ************************************************************************** */

/* The name of the global descriptor vector. */
#define JITTER_PATCH_IN_DESCRIPTORS_NAME(prefix) \
  JITTER_CONCATENATE_TWO(prefix, _fast_branch_descriptors)

/* The name of the assembly label at the end of the global descriptor vector. */
#define JITTER_PATCH_IN_DESCRIPTORS_NAME_END(prefix) \
  JITTER_CONCATENATE_TWO(JITTER_PATCH_IN_DESCRIPTORS_NAME(prefix), \
                         _end)

/* The name of the global, defined in assembly, holding the number of patch-in
   descriptors. */
#define JITTER_PATCH_IN_DESCRIPTOR_NO_NAME(prefix) \
  JITTER_CONCATENATE_TWO(prefix, _fast_branch_descriptor_no)

/* Expand to extern declaration of the variables defined in assembly, to be used
   from C. */
#define JITTER_PATCH_IN_DESCRIPTOR_DECLARATIONS(prefix)                \
  extern struct jitter_patch_in_descriptor                             \
  JITTER_PATCH_IN_DESCRIPTORS_NAME(prefix) [];                         \
  extern const jitter_uint JITTER_PATCH_IN_DESCRIPTOR_NO_NAME(prefix);




/* Fast-branch descriptor data structures: C API.
 * ************************************************************************** */

// FIXME: comment

/* These structures are accessed from C, but their only definition is in assembly
   code; see the page right below this one. */

/* The descriptor for a specific patch-in.  Instances of this are defined in
   assembly, but are meant to be accessed from C as part of a global array.

   It is convenient that every field be word-sized.  Reordering fields requires
   changes in the assembly code below. */
struct jitter_patch_in_descriptor
{
  /* The specialized instruction opcode for this patch-in. */
  jitter_uint specialized_instruction_opcode;

  /* The patch-in offset, in bytes, from the beginning of the specialized
     instruction code. */
  jitter_uint offset;

  /* The patch-in length, in bytes.  This information might be redundant if
     patch_in_case assumes a known length, as it is the case now, but I keep
     this for future extensibility. */
  jitter_uint length;

  /* The patch-in case, indicating the kind of patching to perform.  In order to
     prevent mistakes, zero will not be used as a valid case. */
  jitter_uint patch_in_case;

  /* The following fields are case-dependent arguments, not all used for every
     case. */

  /* 0-th case-dependent word.  Each word is implemented as a union of
     word-sized elements, to make access more convenient from C code. */
  union
  {
    /* For a fast goto: fast label index, 0-based. */
    jitter_uint residual_index;

    /* Generic 0-th word, as an unsigned datum. */
    jitter_uint case_dependent_word_0_uint;

    /* Generic 0-th word, as an signed datum. */
    jitter_int  case_dependent_word_0_int;
  } __attribute__ ((packed));

  /* 1-st case-dependent word, not used yet. */
  union
  {
    jitter_uint case_dependent_word_1_uint;
    jitter_int  case_dependent_word_1_int;
  } __attribute__ ((packed));

  /* 2-nd case-dependent word, not used yet. */
  union
  {
    jitter_uint case_dependent_word_2_uint;
    jitter_int  case_dependent_word_2_int;
  } __attribute__ ((packed));

  /* 3-rd case-dependent word, not used yet. */
  union
  {
    jitter_uint case_dependent_word_3_uint;
    jitter_int  case_dependent_word_3_int;
  } __attribute__ ((packed));
} __attribute__ ((packed));

/* The size of a struct jitter_patch_in_descriptor in words, as a literal
   integer which can be also be stringified and emitted as part of the assembly
   code; sizeof wouldn't wokr in this context.
   FIXME: check that this is correct with an assert at initialization; it will
   be optimized away if everything is correct, but it's important to have it:
   the macro is expanded into assembly in a context where a sizeof wouldn't
   work, and the assertion is a way to check that C and assembly have a
   consistent view of data.

   This complication is a pity: GCC documents the "x86 Operand Modifiers" as
   non-portable; having the equivalent of the 'c' modifier on every platform
   would allow for cleaner code. */
#define JITTER_PATCH_IN_DESCRIPTOR_SIZE_IN_WORDS     8




/* Infrastructure for assembly definitions.
 * ************************************************************************** */

/* Every macro whose name starts with JITTER_ASM_ expands to a literal string,
   meant to be emitted as part of the generated assembly code. */

// FIXME: add comments.

#define JITTER_ASM_FAST_LABEL_PLACEHOLDER_SUBSECTION  \
  "10"

#define JITTER_ASM_PUSH_SUBSECTION  \
  "\n.data " JITTER_ASM_FAST_LABEL_PLACEHOLDER_SUBSECTION "\n"
  // "\n.pushsection .rodata, " JITTER_ASM_FAST_LABEL_PLACEHOLDER_SUBSECTION "\n"

#define JITTER_ASM_POP_SUBSECTION  \
  "\n.text 0\n\t"
  // ".popsection\n\t"

/* Emit a string literal for inclusion in inline assembly, skipping the given
   number of bytes (a constant assembly expression not referring variables) in
   the current section.  The fill byte is JITTER_ASM_PATCH_IN_FILL_BYTE . */
#define JITTER_ASM_SKIP_BYTES(size)                                           \
  JITTER_ASM_COMMENT_PREFIX "Skip " JITTER_STRINGIFY(size) "B, but use a "    \
    "conditional to avoid a warning if " JITTER_STRINGIFY(size) " is zero.\n" \
  ".ifgt (" JITTER_STRINGIFY(size) ")\n"                                      \
  "  .skip " JITTER_STRINGIFY(size) ", " JITTER_ASM_PATCH_IN_FILL_BYTE "\n"   \
  ".endif\n"

#define JITTER_PATCH_IN_DESCRIPTORS_NAME_AS_STRING(prefix)    \
  JITTER_STRINGIFY(JITTER_PATCH_IN_DESCRIPTORS_NAME(prefix))

#define JITTER_PATCH_IN_DESCRIPTORS_NAME_END_AS_STRING(prefix)    \
  JITTER_STRINGIFY(JITTER_PATCH_IN_DESCRIPTORS_NAME_END(prefix))

#define JITTER_PATCH_IN_DESCRIPTOR_SIZE_IN_BYTES_AS_STRING        \
  "(" JITTER_STRINGIFY(JITTER_PATCH_IN_DESCRIPTOR_SIZE_IN_WORDS)  \
      "*" JITTER_STRINGIFY(SIZEOF_VOID_P) ")"

#define JITTER_ASM_DEFINE(name)                                       \
  ".globl " JITTER_STRINGIFY(name) "\n\t"                             \
  ".type  " JITTER_STRINGIFY(name) ", STT_OBJECT\n"                   \
  JITTER_STRINGIFY(name) ":\n\t"




/* Patch-in macros.
 * ************************************************************************** */

// FIXME: add comments.

#define JITTER_ASM_PATCH_IN_HEADER(prefix)                       \
  JITTER_ASM_PUSH_SUBSECTION                                     \
    JITTER_ASM_DEFINE(JITTER_PATCH_IN_DESCRIPTORS_NAME(prefix))  \
  JITTER_ASM_POP_SUBSECTION

#define JITTER_PATCH_IN_HEADER(prefix)               \
  asm volatile (JITTER_ASM_PATCH_IN_HEADER(prefix))

#define JITTER_ASM_PATCH_IN_FOOTER(prefix)                            \
  JITTER_ASM_PUSH_SUBSECTION                                          \
    JITTER_PATCH_IN_DESCRIPTORS_NAME_END_AS_STRING(prefix) ":\n"      \
    JITTER_ASM_DEFINE(JITTER_PATCH_IN_DESCRIPTOR_NO_NAME(prefix))     \
    JITTER_ASM_WORD " "                                               \
      "(("  JITTER_PATCH_IN_DESCRIPTORS_NAME_END_AS_STRING(prefix)    \
            " - "                                                     \
            JITTER_PATCH_IN_DESCRIPTORS_NAME_AS_STRING(prefix) ")"    \
      " / " JITTER_PATCH_IN_DESCRIPTOR_SIZE_IN_BYTES_AS_STRING ")\n"  \
  JITTER_ASM_POP_SUBSECTION

#define JITTER_PATCH_IN_FOOTER(prefix)               \
  asm volatile (JITTER_ASM_PATCH_IN_FOOTER(prefix))

#define JITTER_ASM_PATCH_IN_PLACEHOLDER(size_in_bytes,                \
                                        case,                         \
                                        arg0,                         \
                                        arg1,                         \
                                        arg2,                         \
                                        arg3)                         \
  "\n" JITTER_ASM_COMMENT_PREFIX                                      \
     " Patch-in " JITTER_STRINGIFY(case)" %= \n"                      \
  "1:\n\t"                                                            \
  JITTER_ASM_SKIP_BYTES(size_in_bytes) "\n"                           \
  JITTER_ASM_PUSH_SUBSECTION "\t"                                     \
    JITTER_ASM_WORD " "                                               \
      JITTER_STRINGIFY(JITTER_SPECIALIZED_INSTRUCTION_OPCODE) "\n\t"  \
    JITTER_ASM_WORD " (1b - %l["                                      \
      JITTER_SPECIALIZED_INSTRUCTION_BEGIN_LABEL_AS_STRING "])\n\t"   \
    JITTER_ASM_WORD " " JITTER_STRINGIFY(size_in_bytes) "\n\t"        \
    JITTER_ASM_WORD " " JITTER_STRINGIFY(case) "\n\t"                 \
    JITTER_ASM_WORD " " JITTER_STRINGIFY(arg0) "\n\t"                 \
    JITTER_ASM_WORD " " JITTER_STRINGIFY(arg1) "\n\t"                 \
    JITTER_ASM_WORD " " JITTER_STRINGIFY(arg2) "\n\t"                 \
    JITTER_ASM_WORD " " JITTER_STRINGIFY(arg3) "\n"                   \
  JITTER_ASM_POP_SUBSECTION                                           \

#define JITTER_PATCH_IN_PLACEHOLDER_GOTO(size_in_bytes,     \
                                         case,              \
                                         arg0,              \
                                         arg1,              \
                                         arg2,              \
                                         arg3)              \
  asm goto (JITTER_ASM_PATCH_IN_PLACEHOLDER(size_in_bytes,  \
                                            case,           \
                                            arg0,           \
                                            arg1,           \
                                            arg2,           \
                                            arg3)           \
            : /* outputs */                                 \
            : /* inputs */                                  \
            : /* clobbers */                                \
            :   jitter_jump_anywhere_label                  \
                /* not actually used as a jump target */    \
              , JITTER_SPECIALIZED_INSTRUCTION_BEGIN_LABEL  \
                /* gotolabels */)




/* Fallback fill-in byte.
 * ************************************************************************** */

/* Unless a definition is not already in machine-specific code. define the byte
   with which placeholder code is to be filled in.  This default is easy to
   recognize at a glance, but the best choice is machine-specific: ideally the
   byte should always be invalid at the beginning of an instruction, to make
   spotting mistakes easier. */
#ifndef JITTER_ASM_PATCH_IN_FILL_BYTE
# define JITTER_ASM_PATCH_IN_FILL_BYTE "0xff"
#endif // #ifndef JITTER_ASM_PATCH_IN_FILL_BYTE




/* Patch-in debugging.
 * ************************************************************************** */

/* Dump the pointed patch-in descriptor in human-readable form to the pointed
   stream. */
void
jitter_dump_patch_in_descriptor (FILE *f,
                                 const struct jitter_patch_in_descriptor *p)
  __attribute__ ((nonnull (1, 2)));

/* Like jitter_dump_patch_in_descriptor, but prepend the given string to each
   printed line. */
void
jitter_dump_patch_in_descriptor_with_prefix
   (FILE *f,
    const char *prefix,
    const struct jitter_patch_in_descriptor *p)
  __attribute__ ((nonnull (1, 2, 3)));

/* Dump the given patch-in descriptor array, having the given size in
   elements, in human-readable form to the pointed stream. */
void
jitter_dump_patch_in_descriptors
   (FILE *f,
    const struct jitter_patch_in_descriptor patch_ins[],
    size_t patch_in_no)
  __attribute__ ((nonnull (1)));

/* A convenience macro to call jitter_dump_patch_in_descriptors with the correct
   parameters. */
#define JITTER_DUMP_PATCH_IN_DESCRIPTORS(prefix)        \
  do                                                    \
    {                                                   \
      jitter_dump_patch_in_descriptors                  \
         (stderr,                                       \
          JITTER_PATCH_IN_DESCRIPTORS_NAME(prefix),     \
          JITTER_PATCH_IN_DESCRIPTOR_NO_NAME(prefix));  \
    }                                                   \
  while (false)

#endif // #ifdef JITTER_HAVE_PATCH_IN
#endif // #ifndef JITTER_PATCH_IN_H_
