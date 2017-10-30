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
#include <jitter/jitter-vm.h>

#include <jitter/jitter-dynamic-buffer.h>
#include <jitter/jitter-fatal.h>

struct jitter_instruction*
jitter_last_instruction (struct jitter_program *p)
{
  return * jitter_last_instructions (p, 1);
}

struct jitter_instruction**
jitter_last_instructions (struct jitter_program *p, size_t how_many)
{
  // FIXME: probably useless here.  Make this always unsafe, as it's only called
  // from a safe plaace.
  if (p->stage != jitter_program_stage_unspecialized)
    jitter_fatal ("jitter_last_instruction: non non-unspecialized program");
  if (p->expected_parameter_no != 0)
    jitter_fatal ("jitter_last_instruction: previous instruction incomplete");
  // FIXME: possibly too defensive?
  if (p->rewritable_instruction_no < how_many)
    jitter_fatal ("getting more last instructions than we have as rewritable");

  struct jitter_instruction **resp
    = ((struct jitter_instruction **)
       ((char *) jitter_dynamic_buffer_first_unused_char (& p->instructions)
        - sizeof (struct jitter_instruction*) * how_many));
  return resp;
}

struct jitter_instruction*
jitter_pop_instruction (struct jitter_program *p)
{
  // FIXME: possibly too defensive?
  if (p->rewritable_instruction_no == 0)
    jitter_fatal ("popping an instruction when rewritable ones are zero");

  // FIXME: probably useless here.  Make this always unsafe, as it's only called
  // from a safe plaace.
  if (p->stage != jitter_program_stage_unspecialized)
    jitter_fatal ("jitter_last_instruction: non non-unspecialized program");
  if (p->expected_parameter_no != 0)
    jitter_fatal ("jitter_last_instruction: previous instruction incomplete");

  /* There will be one fewer rewritable instruction after this returns. */
  p->rewritable_instruction_no --;

  return * ((struct jitter_instruction **)
            (jitter_dynamic_buffer_pop (& p->instructions,
                                        sizeof (struct jitter_instruction*))));
}

void
jitter_destroy_last_instructions (struct jitter_program *p,
                                  size_t how_many)
{
  // FIXME: possibly too defensive?
  if (p->rewritable_instruction_no < how_many)
    jitter_fatal ("destroying more instruction than we have rewritable");

  /* Let lasts be a pointer to the pointer to the first instruction to destroy;
     the lasts + 1 will point to the second instruction to destroy, and so
     on. */
  struct jitter_instruction **lasts = jitter_last_instructions (p, how_many);

  /* Destroy instructions, left-to-right. */
  int i;
  for (i = 0; i < how_many; i ++)
    jitter_destroy_instruction (lasts [i]);

  /* There will be how_many fewer rewritable instructions after this returns. */
  p->rewritable_instruction_no -= how_many;

  /* Pop the pointers in one go.  The pointed heap memory, including parameters
     which were held in separate malloc'ed buffers, has been freed already. */
  jitter_dynamic_buffer_pop (& p->instructions,
                             sizeof (struct jitter_instruction*) * how_many);
}

void
jitter_rewrite (struct jitter_program *p)
{
  // FIXME: this loop is probably not needed.  The rewrite rule itself will call
  // jitter_rewrite by indirect recursion when needed.
  /* static int the_index = 0; */
  /* int index = ++ the_index; */
  /* fprintf (stderr, "jitter_rewrite (%i): begin\n", index); */
  /* /\* Keep calling jitter_rewrite_once until there is nothing more to change. *\/ */
  /* int round = 0; */
  /* do */
  /*   { */
  /*     if (++ round > 1) */
  /*       fprintf (stderr, "  jitter_rewrite (%i): round %i\n", index, round); */
  /*     /\* Nothing. *\/ */
  /*   } */
  /* while (   p->rewritable_instruction_no > 0 */
  /*        && p->vm->rewrite_once (p, p->rewritable_instruction_no)); */
  /* fprintf (stderr, "jitter_rewrite (%i): end\n", index); */

  static int the_index = 0;
  int index = ++ the_index;
  fprintf (stderr, "Before rewrite %i:   there are %i instructions, %i rewritable...\n",
           index,
           (int) jitter_program_instruction_no (p),
           (int) p->rewritable_instruction_no);

  if (p->rewritable_instruction_no > 0)
    p->vm->rewrite_once (p, p->rewritable_instruction_no);

  fprintf (stderr, "...after rewrite %i: there are %i instructions, %i rewritable.\n",
           index,
           (int) jitter_program_instruction_no (p),
           (int) p->rewritable_instruction_no);

  /* If the last instruction we have in the program (post-rewriting, since it
     might have been changed) is a caller, then everything up it its point can
     no longer be rewritten: the return address right after the call is an
     implicit label. */
  if (   jitter_program_instruction_no (p) > 0
      && jitter_last_instruction (p)->meta_instruction->caller)
    p->rewritable_instruction_no = 0;
}
