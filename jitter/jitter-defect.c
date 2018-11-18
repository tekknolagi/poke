/* Jitter: defective VM instructions.

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


#include <stdio.h>
#include <stdbool.h>
#include <jitter/jitter-defect.h>


/* Conditional expansion: begin.
 * ************************************************************************** */

/* Define feature macros. */
#include <jitter/jitter-patch-in.h>

/* Expand to nothing if we aren't using patch-ins.  It's harmless to
   unconditionally keep the declarations in the header. */
#ifdef JITTER_HAVE_PATCH_IN


#include <jitter/jitter-vm.h>




/* Defect efficient data structures.
 * ************************************************************************** */

void
jitter_fill_defect_table (jitter_uint *defect_table,
                          const struct jitter_vm *vm,
                          const jitter_uint *worst_case_defect_table,
                          const struct jitter_defect_descriptor *descs,
                          size_t desc_no)
{
  size_t specialized_instruction_no = vm->specialized_instruction_no;

  /* Temporarily use the defect table we are filling to store booleans: false
     for non-defective instructions, true for defective instructions.  These
     booleans will be replaced with opcodes in the final pass. */

  /* First pass: mark every specialized instruction as non-defective by
     default. */
  int i;
  for (i = 0; i < specialized_instruction_no; i ++)
    defect_table [i] = false;

  /* Second pass, over the defect descriptor array (not the defect table): scan
     defect descriptors, and for every defect found mark the associated
     specialized instruction as defective.  Of course one defect is enough to
     make a specialized instruction defective, even if the same specialized
     instruction has other descriptors not encoding any defect. */
  for (i = 0; i < desc_no; i ++)
    if (__builtin_expect (descs [i].displacement != 0,
                          false))
      defect_table [descs [i].specialized_opcode] = true;

  /* Third and final pass, over the defect table: replace booleans with
     specialized opcodes.  The specialized opcode will be equal to the index for
     non-defective instruction, and the replacement specialized opcode for
     defective specialized instructions. */
  for (i = 0; i < specialized_instruction_no; i ++)
    if (__builtin_expect (defect_table [i],
                          false))
      {
        // FIXME: move this warning to some other place.
        if (worst_case_defect_table [i] == i)
          fprintf (stderr,
                   "WARNING: specialized instruction %s (opcode %i) "
                   "is defective but has no replacement\n",
                   vm->specialized_instruction_names [i], i);
        defect_table [i] = worst_case_defect_table [i];
      }
    else
      defect_table [i] = i;
}




/* Defect debugging.
 * ************************************************************************** */

void
jitter_dump_defect_table (FILE *f,
                          const jitter_uint *defect_table,
                          const struct jitter_vm *vm)
{
  size_t specialized_instruction_no = vm->specialized_instruction_no;
  const char * const * specialized_instruction_names
    = vm->specialized_instruction_names;

  int defect_count = 0;
  jitter_uint i;
  for (i = 0; i < specialized_instruction_no; i ++)
    {
      jitter_uint replacement_opcode = defect_table [i];
      if (replacement_opcode != i)
        {
          fprintf (f, "Defect: %s (opcode %i) replaced by %s (opcode %i)\n",
                   specialized_instruction_names [i], (int) i,
                   specialized_instruction_names [replacement_opcode],
                   (int) replacement_opcode);
          defect_count ++;
        }
    }
  if (defect_count > 0)
    fprintf (f, "There are %i defective specialized instructions.\n", defect_count);
}




/* Conditional expansion: end.
 * ************************************************************************** */

/* End of the part to be only expanded when patch-ins are in use. */
#endif // #ifdef JITTER_HAVE_PATCH_IN
