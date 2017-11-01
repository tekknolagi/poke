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

/* Expand to the name of the global descriptor vector as a string literal. */
#define JITTER_PATCH_IN_DESCRIPTORS_NAME_AS_STRING(prefix)    \
  JITTER_STRINGIFY(JITTER_PATCH_IN_DESCRIPTORS_NAME(prefix))

/* Expand to the end label for the global descriptor vector as a string
   literal. */
#define JITTER_PATCH_IN_DESCRIPTORS_NAME_END_AS_STRING(prefix)    \
  JITTER_STRINGIFY(JITTER_PATCH_IN_DESCRIPTORS_NAME_END(prefix))

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

/* Expand to a literal string defining the given identifier as a global in
   assembly. */
#define JITTER_ASM_DEFINE(name)                      \
  ".globl " JITTER_STRINGIFY(name) "\n\t"            \
  ".type  " JITTER_STRINGIFY(name) ", STT_OBJECT\n"  \
  JITTER_STRINGIFY(name) ":\n\t"

/* Expand to a string literal for inclusion in inline assembly, skipping the
   given number of bytes (a constant assembly expression not referring
   variables) in the current section.  The fill byte is
   JITTER_ASM_PATCH_IN_FILL_BYTE . */
#define JITTER_ASM_SKIP_BYTES(size)                                          \
  JITTER_ASM_COMMENT_UNIQUE("Skip " JITTER_STRINGIFY(size) "B, but use a "   \
                            "conditional to avoid a warning if "             \
                            JITTER_STRINGIFY(size) " is zero")               \
  ".ifgt (" JITTER_STRINGIFY(size) ")\n"                                     \
  "  .skip " JITTER_STRINGIFY(size) ", " JITTER_ASM_PATCH_IN_FILL_BYTE "\n"  \
  ".endif\n"

/* Expand to a string literal containing a constant assembly expression
   whose result evaluates to the size of one patch-in descriptor in bytes. */
#define JITTER_ASM_PATCH_IN_DESCRIPTOR_SIZE_IN_BYTES_AS_STRING    \
  "(" JITTER_STRINGIFY(JITTER_PATCH_IN_DESCRIPTOR_SIZE_IN_WORDS)  \
      "*" JITTER_STRINGIFY(SIZEOF_VOID_P) ")"




/* Patch-in descriptor macros.
 * ************************************************************************** */

/* Patch-in descriptors are stored in a read-only global array, generated in its
   own assembly subsection.  Each patch-in descriptor contains information about
   a patch-in placeholder.
   This is delicate, since each patch-in placeholder is contained in compiled C
   code; some patch-in placeholders may even be duplicated or optimized away by
   GCC.

   The solution is using assembler subsections: the same inline asm code
   expanding to a patch-in placeholder temporarily enters the descriptor
   subsection, adds an element to the descriptor array referring the label
   for the current placeholder plus other information such as the specialized
   instruction opcode, then exits the subsection and goes back to .text .

   The descriptor array is defined, of course within its subsection, from a
   "header" in top-level inline assembly, which *must* come before the
   interpreter function containing patch-in placeholders.  The interpreter
   function relies on the no_reorder attribute which prevents it from being
   moved with respect to top-level inline asm.  Similarly, a top-level inline
   asm "footer" closes the global array definition, and defined a further global
   storing the array size.

   Interestingly no assembly *instructions* are required for this: the generated
   inline assembly contains only data, which is machine-independent.  The
   subsection mechanism, however, relies on ELF.  Support for other binary
   formats is almost certainly possible, but not prioritary.  Modern GNU systems
   use ELF. */

/* Expand to the literal string to be used in a top-level inline asm statement
   as a patch-in descriptor header.  The generated "code", which contains no
   machine instructions, switches to the appropriate subsection, opens the
   definition, and goes back to .text . */
#define JITTER_ASM_PATCH_IN_HEADER(prefix)                       \
  JITTER_ASM_ENTER_PATCH_IN_DESCRIPTOR_SUBSECTION                \
    JITTER_ASM_DEFINE(JITTER_PATCH_IN_DESCRIPTORS_NAME(prefix))  \
  JITTER_ASM_EXIT_PATCH_IN_DESCRIPTOR_SUBSECTION

/* Expand to the literal string to be used in a top-level inline asm statement
   as a patch-in descriptor footer.  The generated "code", which contains no
   machine instructions, switches to the appropriate subsection, closes the
   array definition, defines another global holding the array sizein bytes, and
   goes back to .text . */
#define JITTER_ASM_PATCH_IN_FOOTER(prefix)                                \
  JITTER_ASM_ENTER_PATCH_IN_DESCRIPTOR_SUBSECTION                         \
    JITTER_PATCH_IN_DESCRIPTORS_NAME_END_AS_STRING(prefix) ":\n"          \
    JITTER_ASM_DEFINE(JITTER_PATCH_IN_DESCRIPTOR_NO_NAME(prefix))         \
    JITTER_ASM_WORD " "                                                   \
      "(("  JITTER_PATCH_IN_DESCRIPTORS_NAME_END_AS_STRING(prefix)        \
            " - "                                                         \
            JITTER_PATCH_IN_DESCRIPTORS_NAME_AS_STRING(prefix) ")"        \
      " / " JITTER_ASM_PATCH_IN_DESCRIPTOR_SIZE_IN_BYTES_AS_STRING ")\n"  \
  JITTER_ASM_EXIT_PATCH_IN_DESCRIPTOR_SUBSECTION

/* Expand to a C top-level inline asm statement containing the patch-in
   header. */
#define JITTER_PATCH_IN_HEADER(prefix)      \
  asm (JITTER_ASM_PATCH_IN_HEADER(prefix))

/* Expand to a C top-level inline asm statement containing the patch-in
   footer. */
#define JITTER_PATCH_IN_FOOTER(prefix)      \
  asm (JITTER_ASM_PATCH_IN_FOOTER(prefix))




/* Patch-in descriptor macros for ELF systems.
 * ************************************************************************** */

/* Macros to enter and exit the patch-in descriptor subsection.  Right now this
   is only implemented for ELF systems, but other systems might be possible to
   support.
   By default configure disables no-threading on non-ELF systems, so this should
   not fail unless the user asks for it. */
#ifdef JITTER_HOST_OS_IS_ELF
# define JITTER_ASM_ENTER_PATCH_IN_DESCRIPTOR_SUBSECTION  \
    JITTER_ASM_ENTER_PATCH_IN_DESCRIPTOR_SUBSECTION_ELF
# define JITTER_ASM_EXIT_PATCH_IN_DESCRIPTOR_SUBSECTION  \
    JITTER_ASM_EXIT_PATCH_IN_DESCRIPTOR_SUBSECTION_ELF
#else
# error "patch-ins currently rely on ELF"
#endif // #ifdef JITTER_HOST_OS_IS_ELF

/* Expand to a string literal containing the .rodata subsection number
   containing the patch-in descriptors. */
#define JITTER_ASM_FAST_LABEL_PLACEHOLDER_SUBSECTION_ELF  \
  "10"

/* Solution relying on ELF: */
#define JITTER_ASM_ENTER_PATCH_IN_DESCRIPTOR_SUBSECTION_ELF  \
  "\n.pushsection .rodata, "                                 \
     JITTER_ASM_FAST_LABEL_PLACEHOLDER_SUBSECTION_ELF "\n\t"
#define JITTER_ASM_EXIT_PATCH_IN_DESCRIPTOR_SUBSECTION_ELF  \
  "\n.popsection\n\t"




/* Patch-in placeholder macros.
 * ************************************************************************** */

/* Expand to a string literal meant to be used as the the assembly template for
   a patch-in, referring the named input argument
   "jitter_vm_instruction_beginning".

   The "l" in the assembly template does *not* indicate a "gotolabel" argument
   for asm goto: jitter_vm_instruction_beginning is not passed as a gotolabel,
   but as an ordinary input argument.
   I used to pass the argument as a gotolabel, but the label was sometimes
   resolved to a slightly different address compared to what is computed by the
   prefix && operator under GCC 6 -- which broke patch-ins.  Elsewhere in Jitter
   I use prefix && to delimit the beginning and end of VM instruction code, so
   it is sensible to always compute these address the same way.

   The "l" modifier serves to omit the immediate-argument prefix "$" on i386 and
   x86_64, which would be syntactically incorrect in this context, out of an
   instruction.
   This "l" is documented in the GCC manual as one of the "x86 Operand
   Modifiers", but I see it working on any architecture -- doing nothing on
   architectures different from i386 and x86_64.  Should GCC's behavior change
   in the future it will be trivial to replace "l" with an architecture-specific
   macro expanding to "l" on i386 and x86_64 and nothing on every other. */
#define JITTER_ASM_PATCH_IN_PLACEHOLDER(size_in_bytes, case,          \
                                        arg0, arg1, arg2, arg3)       \
  JITTER_ASM_COMMENT_UNIQUE("Patch-in " JITTER_STRINGIFY(case))       \
  "\n1:\n\t"                                                          \
  JITTER_ASM_SKIP_BYTES(size_in_bytes) "\n"                           \
  JITTER_ASM_ENTER_PATCH_IN_DESCRIPTOR_SUBSECTION                     \
    JITTER_ASM_WORD " "                                               \
      JITTER_STRINGIFY(JITTER_SPECIALIZED_INSTRUCTION_OPCODE) "\n\t"  \
    JITTER_ASM_WORD " (1b - %l[jitter_vm_instruction_beginning])\n\t" \
    JITTER_ASM_WORD " " JITTER_STRINGIFY(size_in_bytes) "\n\t"        \
    JITTER_ASM_WORD " " JITTER_STRINGIFY(case) "\n\t"                 \
    JITTER_ASM_WORD " " JITTER_STRINGIFY(arg0) "\n\t"                 \
    JITTER_ASM_WORD " " JITTER_STRINGIFY(arg1) "\n\t"                 \
    JITTER_ASM_WORD " " JITTER_STRINGIFY(arg2) "\n\t"                 \
    JITTER_ASM_WORD " " JITTER_STRINGIFY(arg3) "\n"                   \
  JITTER_ASM_EXIT_PATCH_IN_DESCRIPTOR_SUBSECTION

/* Expand to the (named) input argument jitter_vm_instruction_beginning for
   an inline asm template generated by JITTER_ASM_PATCH_IN_PLACEHOLDER.

   The constraint is given as "X" rather than the more intuitive "i" because of
   the SH architecture, where that constraint would fail because of the small
   immediate size.
   Should this prove inadequate, it will be easy to make the constraint an
   architecture-specific macro in the future.  Anyway a failure will be very
   visible, since any problem caused by this will happen early, at compile
   or assemble time. */
#define JITTER_INPUT_VM_INSTRUCTION_BEGINNING  \
  [jitter_vm_instruction_beginning]            \
     "X" (&& JITTER_SPECIALIZED_INSTRUCTION_BEGIN_LABEL)

/* Expand to a C statement (currently not protected with do..while (false), as
   this is meant to be only used within other macros) containing the given
   patch-in for an unconditional VM branch.

   Notice how the (only) input argument is the label for the beginning of the
   current VM instruction, obtained by the prefix && operator: see the comment
   before JITTER_ASM_PATCH_IN_PLACEHOLDER . */
#define JITTER_PATCH_IN_PLACEHOLDER_GOTO_(size_in_bytes, case,       \
                                          arg0, arg1, arg2, arg3)    \
  asm goto (JITTER_ASM_PATCH_IN_PLACEHOLDER(size_in_bytes, case,     \
                                            arg0, arg1, arg2, arg3)  \
            : /* outputs */                                          \
            : JITTER_INPUT_VM_INSTRUCTION_BEGINNING /* inputs */     \
            : /* clobbers */                                         \
            : jitter_jump_anywhere_label /* gotolabels */)




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
