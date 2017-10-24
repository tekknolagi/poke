/* VM library: specializer header file.

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


#ifndef JITTER_SPECIALIZE_H_
#define JITTER_SPECIALIZE_H_

// #include <jitter/jitter-dispatch.h> // No.
#include <jitter/jitter.h>
#include <jitter/jitter-program.h>
#include <jitter/jitter-instruction.h>

//#include "vmprefix-frontend.h"


/* Program specialization.
 * ************************************************************************** */

/* Some specialized instruction opcodes whose values must always be valid,
   independently from the VM.  This is checked with assertions at the first
   run of the specializer. */
enum jitter_specialized_instruction_opcode
  {
    jitter_specialized_instruction_opcode_INVALID = 0,
    jitter_specialized_instruction_opcode_BEGINBASICBLOCK = 1,
    jitter_specialized_instruction_opcode_EXITVM = 2,
    jitter_specialized_instruction_opcode_UNREACHABLE0 = 3,
    jitter_specialized_instruction_opcode_UNREACHABLE1 = 4,
    jitter_specialized_instruction_opcode_UNREACHABLE2 = 5,
  };

/* Specialize the given program. */
void
jitter_specialize_program (struct jitter_program *p)
  __attribute__ ((nonnull (1)));

/* Add an opcode to the specialized program which is being built.  This is an
   auxiliary function used by vmprefix_specialize_instruction .  The opcode type
   should actually be the VM-dependent enum
   vmprefix_specialized_instruction_opcode , but it's safe to use a sufficiently
   wide unsigned type so that this code is VM-independent. */
void
jitter_add_specialized_instruction_opcode
   (struct jitter_program *p,
    /* This is actually an enum vmprefix_specialized_instruction_opcode , but
       the type is VM-dependent. */
    jitter_uint opcode);

/* Add a fixnum literal to the specialized program which is being built.  This
   is an auxiliary function used by vmprefix_specialize_instruction . */
void
jitter_add_specialized_instruction_literal (struct jitter_program *p,
                                            jitter_uint literal);

/* Add a label literal (as an instruction index) to the specialized program
   which is being built.  This is an auxiliary function used by
   vmprefix_specialize_instruction . */
void
jitter_add_specialized_instruction_label_index (struct jitter_program *p,
                                                jitter_label_as_index
                                                unspecialized_instruction_index);

#endif // #ifndef JITTER_SPECIALIZE_H_
