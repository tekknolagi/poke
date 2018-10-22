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
#include <jitter/jitter-malloc.h>


/* Defect debugging.
 * ************************************************************************** */

void
jitter_dump_defect_descriptors (FILE *f,
                                const char * const specialized_instruction_names [],
                                size_t specialized_instruction_no,
                                const struct jitter_defect_descriptor defects [],
                                size_t defect_no)
{
  /* Make a temporary boolean array, one element per specialized instruction,
     all initialized to false. */
  bool *defective = jitter_xmalloc (sizeof (bool) * specialized_instruction_no);
  bool at_least_one_defective = false;
  int i;
  for (i = 0; i < specialized_instruction_no; i ++)
    defective [i] = false;

  /* Scan the defect array, marking as defective in the temporary array each
     instruction found to have at least one defect. */
  for (i = 0; i < defect_no; i ++)
    if (defects [i].displacement != 0)
      {
        defective [defects [i].specialized_opcode] = true;
        at_least_one_defective = true;
      }

  /* Print defecive instructions (only). */
  if (at_least_one_defective)
    {
      int defective_no = 0;
      fflush (stdout);
      fflush (stderr);
      for (i = 0; i < specialized_instruction_no; i ++)
        if (defective [i])
          {
            defective_no ++;
            fprintf (f, "WARNING: %s (opcode %i) is defective.\n",
                     specialized_instruction_names [i], i);
          }
      fprintf (f, "         There are %i defective specialized instructions!\n",
               defective_no);
      fflush (f);
    }

  /* Destroy the temporary array. */
  free (defective);
}
