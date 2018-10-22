/* Jitter: defective VM instruction header.

   Copyright (C) 2018 Luca Saiu
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


#ifndef JITTER_DEFECT_H_
#define JITTER_DEFECT_H_

/* Include core headers.
 * ************************************************************************** */

/* Include macro definitions about whether we have a machine file, and about the
   dispatching model. */
#include <jitter/jitter.h>
#include <jitter/jitter-config.h>
#include <jitter/jitter-dispatch.h>

/* Include sectioning macros. */
#include <jitter/jitter-sections.h>

/* The debugging facility relies on standard I/O. */
#include <stdio.h>




/* Introduction.
 * ************************************************************************** */

// FIXME: write.




/* Defect descriptor data structures: C API.
 * ************************************************************************** */

/* A descriptor associated to each program point possibly causing a specialized
   VM instruction to be defective.  A specialized VM instruction is defective if
   it has even one such descritpor with non-zero displacement. */
struct jitter_defect_descriptor
{
  /* The opcode of the specialized instruction in question. */
  jitter_uint specialized_opcode;

  /* The distance between a used label as seen from C and the same label as seen
     from assembly.  A displacement of zero means that the C and the assembly
     views agree; a non-zero displacement means that the instruction is
     defective. */
  jitter_int displacement;
};




/* Prefix-dependent names for globals.
 * ************************************************************************** */

/* The name of the global descriptor vector. */
#define JITTER_DEFECT_DESCRIPTORS_NAME(_jitter_vm_the_prefix)         \
  JITTER_CONCATENATE_TWO(_jitter_vm_the_prefix, _defect_descriptors)

/* The name of the global descriptor vector. */
#define JITTER_DEFECT_DESCRIPTORS_SIZE_IN_BYTES_NAME(_jitter_vm_the_prefix)     \
  JITTER_CONCATENATE_TWO(JITTER_DEFECT_DESCRIPTORS_NAME(_jitter_vm_the_prefix), \
                         _size_in_bytes)

/* Expand to extern declaration of the variables defined in assembly, to be used
   from C. */
#define JITTER_DEFECT_DESCRIPTOR_DECLARATIONS_(_jitter_vm_the_prefix)   \
  extern const struct jitter_defect_descriptor                          \
  JITTER_DEFECT_DESCRIPTORS_NAME(_jitter_vm_the_prefix) [];             \
  extern const jitter_uint                                              \
  JITTER_DEFECT_DESCRIPTORS_SIZE_IN_BYTES_NAME(_jitter_vm_the_prefix);




/* Defect descriptor opening and closing macros.
 * ************************************************************************** */

/* Each defect descriptor contains information about one program point possibly
   causing a defect. */

/* Expand to a string literal containing the .rodata subsection number
   containing the defect descriptors. */
#define JITTER_ASM_DEFECT_SUBSECTION  \
  "11"

/* Expand to a C top-level inline asm statement containing the patch-in
   header. */
#define JITTER_DEFECT_HEADER(_jitter_vm_the_prefix)                \
  asm (JITTER_ASM_OPEN_DEFINITION(JITTER_ASM_DEFECT_SUBSECTION,    \
                                  JITTER_DEFECT_DESCRIPTORS_NAME(  \
                                     _jitter_vm_the_prefix)))

/* Expand to a C top-level inline asm statement containing the patch-in
   footer. */
#define JITTER_DEFECT_FOOTER(_jitter_vm_the_prefix)                 \
  asm (JITTER_ASM_CLOSE_DEFINITION(JITTER_ASM_DEFECT_SUBSECTION,    \
                                   JITTER_DEFECT_DESCRIPTORS_NAME(  \
                                      _jitter_vm_the_prefix)))




/* Defect descriptor macros.
 * ************************************************************************** */

/* Expand to a literal template string adding a defect descriptor for the
   current specialized instruction.  This should be used within an inline asm
   goto statement having jitter_dispatch_label as gotolabel. */
#define JITTER_ASM_DEFECT_DESCRIPTOR                                \
  JITTER_ASM_COMMENT_UNIQUE(                                        \
     "Defect descriptor for "                                       \
     JITTER_STRINGIFY(JITTER_SPECIALIZED_INSTRUCTION_MANGLED_NAME)) \
  JITTER_ASM_ENTER_SUBSECTION(JITTER_ASM_DEFECT_SUBSECTION) "\n\t"  \
  JITTER_ASM_WORD " "                                               \
     JITTER_STRINGIFY(JITTER_SPECIALIZED_INSTRUCTION_OPCODE) "\n\t" \
  JITTER_ASM_WORD " "                                               \
     "(%l[jitter_dispatch_label] - jitter_dispatch_label_asm)\n"    \
  JITTER_ASM_EXIT_SUBSECTION "\n\t"




/* Defect debugging.
 * ************************************************************************** */

/* Dump the given defect descriptor array, having the given size in
   elements, in human-readable form to the pointed stream. */
void
jitter_dump_defect_descriptors
   (FILE *f,
    const char * const specialized_instruction_names [],
    size_t specialized_instruction_no, 
    const struct jitter_defect_descriptor defects [],
    size_t defect_no)
  __attribute__ ((nonnull (1, 2, 4)));

/* A convenience macro to call jitter_dump_defect_descriptors with the correct
   parameters. */
#define JITTER_DUMP_DEFECT_DESCRIPTORS(_jitter_vm_the_prefix,     \
                                       _jitter_vm_THE_PREFIX)     \
  do                                                              \
    {                                                             \
      size_t defect_no                                            \
        = (JITTER_DEFECT_DESCRIPTORS_SIZE_IN_BYTES_NAME(          \
              _jitter_vm_the_prefix)                              \
           / sizeof (struct jitter_defect_descriptor));           \
      jitter_dump_defect_descriptors                              \
         (stderr,                                                 \
          JITTER_CONCATENATE_TWO(_jitter_vm_the_prefix, \
                                 _specialized_instruction_names), \
          JITTER_CONCATENATE_TWO(_jitter_vm_THE_PREFIX, \
                                 _SPECIALIZED_INSTRUCTION_NO), \
          JITTER_DEFECT_DESCRIPTORS_NAME(_jitter_vm_the_prefix),  \
          defect_no);                                             \
    }                                                               \
  while (false)


#endif // JITTER_DEFECT_H_
