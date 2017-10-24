/* Instruction rewrite functionality: non-generated part.

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


#include <jitter/jitter.h>
#include <jitter/jitter-rewrite.h>

#include <jitter/jitter-dynamic-buffer.h>
#include <jitter/jitter-fatal.h>

const struct jitter_instruction*
jitter_last_instruction (struct jitter_program *p)
{
  if (p->stage != jitter_program_stage_unspecialized)
    jitter_fatal ("jitter_last_instruction: non non-unspecialized program");
  if (p->expected_parameter_no != 0)
    jitter_fatal ("jitter_last_instruction: previous instruction incomplete");

  struct jitter_instruction **
    resp = ((struct jitter_instruction **)
            ((char *)
             jitter_dynamic_buffer_first_unused_char (& p->instructions)
             - sizeof (struct jitter_instruction*)));
  return * resp;
}

struct jitter_instruction*
jitter_pop_instruction (struct jitter_program *p)
{
  if (p->stage != jitter_program_stage_unspecialized)
    jitter_fatal ("jitter_last_instruction: non non-unspecialized program");
  if (p->expected_parameter_no != 0)
    jitter_fatal ("jitter_last_instruction: previous instruction incomplete");

  return * ((struct jitter_instruction **)
            (jitter_dynamic_buffer_pop (& p->instructions,
                                        sizeof (struct jitter_instruction*))));
}
