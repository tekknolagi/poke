/* VM library: macros to be optionally included in architecture-specific asm.

   Copyright (C) 2017, 2019 Luca Saiu
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


/* This is an assembly file independent from the architecture, containing Gas
   macros to be optionally used (by CPP inclusion) in the architecture-specific
   file machine/ARCHITECTURE/jitter/machine/jitter-machine-assembly.S .
   This file contains macro definitions only, and by itself generates no code.
   Because of this it's safe to include it simply as a source in Makefile.am ,
   which tracks dependencies in a correct way. */
#ifndef MACHINE_COMMON_S_
#define MACHINE_COMMON_S_


/* Include feature macros.
 * ************************************************************************** */

/* The code from this header defines macros but expands to nothing, so it is
   safe to include from assembly as well. */
#include <jitter/jitter-config.h>




/* Stack executability not required.
 * ************************************************************************** */

/* Do not require an executable stack.  Jitter, or a Jittery VM runtime, never
   directly relies on that.

   Rationale: none of my assembly code relies on executable stacks, even if GCC
   assumes, by default, that an assembly source file does.  Since this file is
   included in every assembly source this is a good place to put this.  If some
   other source linked to the Jitter runtime requires an executable stack then
   the linked executable will have it, thanks to another
     .section .note.GNU-stack, "x", @progbits
   written somewhere else.
   The Jitter runtime itself, along with every example in the distribution,
   could easily be compiled and linked with
     -Wa,--noexecstack -Wl,-z,noexecstack
   , if there were nothing else. */
#if defined (JITTER_HOST_OS_IS_ELF) \
    && defined (JITTER_HAVE_SECTION_NOTE_GNU_STACK)
.section .note.GNU-stack, "", @progbits
.previous
#endif /* #ifdef JITTER_HOST_OS_IS_ELF */




/* Global GNU Assembler settings and definitions.
 * ************************************************************************** */

/* I like the & separator and <> quoting. */
.altmacro

/* Declare the jitter_native_snippet_sizes symbol in the .data subsection 1,
   the jitter_native_snippet_pointers symbol in the .data subsection 2 and the
   jitter_native_snippet_names symbol in the .data subsection 3 (also using the
   .data subsection 4 to store internal data); by using the Gas macro
   jitter_snippet the subsections will be filled, respectively, with .long
   elements holding the size of each snippet and with elements of the type
   given as a parameter (in practice .quad for 64-bit pointers or .long for
   32-bit pointers) holding pointers to the beginning of each snippet, and
   with pointers (again of the given type) to snippet names as strings.
   Every array element is added in the order of jitter_snippet calls.  The three
   symbols will be visible from C as objects of type const uint32_t [],
   const char * const [] and const char* const [], which will be convenient
   for implementing jitter_snippet_size, jitter_snippet_code and jitter_snippet_name .
   The three arrays are meant to be indexed with enum jitter_snippet_to_patch
   objects, whose cases must follow the same order as the jitter_native calls
   here. */
.macro jitter_arrays pointer_type
/* Define another macro called jitter_snippet_pointer_type , which takes an
   expression as a parameter and generates a \pointer_type with that value. */
        .macro jitter_snippet_pointer_type value
                \pointer_type \value
        .endm
/* Define the array jitter_native_snippet_sizes in .data subsection 1 , making it
   visible from C. */
.pushsection .data, 1
        .type jitter_native_snippet_sizes, @object
        .globl jitter_native_snippet_sizes
jitter_native_snippet_sizes:
.popsection
/* Define the array jitter_native_snippet_pointers in .data subsection 2 , making
   it visible from C. */
.pushsection .data, 2
        .type jitter_native_snippet_pointers, @object
        .globl jitter_native_snippet_pointers
jitter_native_snippet_pointers:
.popsection
/* Define the array jitter_native_snippet_names in .data subsection 3 , making it
   visible from C. */
.pushsection .data, 3
        .type jitter_native_snippet_names, @object
        .globl jitter_native_snippet_names
jitter_native_snippet_names:
.popsection
.endm

/* A simple Gas macro generating a global function of up to twenty instructions
   (or generically "lines") with the given name prefixed by "jitter_native_", plus
   a 32-bit global object with the same name as the function followed by "_size"
   holding the function code size in bytes. */
.macro jitter_snippet snippet_name, \
                  insn1, insn2=<>, insn3=<>, insn4=<>, insn5=<>, insn6=<>, \
                  insn7=<>, insn8=<>, insn9=<>, insn10=<>, insn11=<>, insn12=<>, \
                  insn13=<>, insn14=<>, insn15=<>, insn16=<>, insn17=<>, \
                  insn18=<>, insn19=<>, insn20=<>
/* Define the snippet in the current section. */
#        .globl  jitter_native_&\snippet_name
        .align 8
        .type   jitter_native_&\snippet_name, @function
jitter_native_&\snippet_name&:
        \insn1
        \insn2
        \insn3
        \insn4
        \insn5
        \insn6
        \insn7
        \insn8
        \insn9
        \insn10
        \insn11
        \insn12
        \insn13
        \insn14
        \insn15
        \insn16
        \insn17
        \insn18
        \insn19
        \insn20
jitter_native_&\snippet_name&_end:
/* Add a global containing the procedure size in .data subsection 1 ; the
   global is also one element of jitter_native_snippet_sizes . */
.pushsection .data, 1
#        .type jitter_native_&\snippet_name&_size, @object
#        .globl jitter_native_&\snippet_name&_size
#jitter_native_&\snippet_name&_size:
        .long (jitter_native_&\snippet_name&_end - jitter_native_&\snippet_name&)
.popsection
/* Add a global containing a procedure pointer in .data subsection 2 . */
.pushsection .data, 2
        jitter_snippet_pointer_type jitter_native_&\snippet_name
.popsection
/* Add a global containing the procedure name in .data subsection 4 . */
.pushsection .data, 4
        jitter_native_&\snippet_name&_name:
        .ascii "\snippet_name"
        .byte 0x0
.popsection
/* Add a global containing a pointer to the the procedure name in .data
   subsection 3 . */
.pushsection .data, 3
        jitter_snippet_pointer_type jitter_native_&\snippet_name&_name
.popsection
.endm

#endif // #ifndef MACHINE_COMMON_S_
