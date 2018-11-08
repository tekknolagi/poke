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




/* Defect efficient data structures.
 * ************************************************************************** */

/* Compiling code with defect descriptors yields an object file containing all
   the required information, but the subsection hack leaves the data ordered in
   a very inconvenient and inefficient way.  This functionality generates a
   "defect table", which is a C array which can be indexed by a specialized
   instruction opcode, whose elements are each the opcode of the replacement
   specialized instruction; this opcode will be the same as the index if the
   instruction is not defective.

   The "worst-case defect table" is a defect table mapping each specialized
   opcode for an instruction which can possibly be defective to its replacement.
   The worst-case defect table is a global constant for each VM.

   The idea is, of course, to make searching for a replacement instruction a
   fast O(1) operation which can be executed unconditionally on any specialized
   instruction, be it defective or not. */

/* Given an initial pointer to the defect descriptor array, the number of
   defects descriptors, a pointer to the worst-case defect table and the number
   of specialized instructions, initialize the pointed defect table.  The defect
   table is a global, already existing for any given VM, which only needs to be
   initialized once even if a VM subsystem is finalized and re-initialized
   multiple times. */
void
jitter_fill_defect_table (jitter_uint *defect_table,
                          size_t specialized_instruction_no,
                          const jitter_uint *worst_case_defect_table,
                          const struct jitter_defect_descriptor *descs,
                          size_t desc_no)
  __attribute__ ((nonnull (1, 3, 4)));




/* Defect debugging.
 * ************************************************************************** */

/* Dump the pointed defect table to the given stream. */
void
jitter_dump_defect_table (FILE *f,
                          const jitter_uint *defect_table,
                          size_t specialized_instruction_no,
                          const char * const specialized_instruction_names [])
  __attribute__ ((nonnull (1, 2, 4)));

/* A convenience macro to call jitter_dump_defect_descriptors with the correct
   parameters. */
#define JITTER_DUMP_DEFECT_TABLE(_jitter_vm_the_prefix, _jitter_vm_THE_PREFIX)  \
  do                                                                            \
    {                                                                           \
      jitter_dump_defect_table                                                  \
         (stderr,                                                               \
          JITTER_CONCATENATE_TWO(_jitter_vm_the_prefix, _defect_table),         \
          JITTER_CONCATENATE_TWO(_jitter_vm_THE_PREFIX,                         \
                                 _SPECIALIZED_INSTRUCTION_NO),                  \
          JITTER_CONCATENATE_TWO(_jitter_vm_the_prefix,                         \
                                 _specialized_instruction_names));              \
    }                                                                           \
  while (false)


#endif // JITTER_DEFECT_H_
