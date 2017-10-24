/* A program meant for compilation into assembly to be read.

   Copyright (C) 2016 Luca Saiu
   Updated in 2017 by Luca Saiu
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


#include <config.h>

#include <stdio.h>
#include <limits.h>


int
main (void)
{
  return 0;
}

union bad_word
{
  epsilon_int i;
  void* p;
};

#define VM_STACK_DEPTH 1024

#define BAD_REGISTER_NO 3

struct bad_state
{
  union bad_word registers [BAD_REGISTER_NO];
  union bad_word* restrict slow_registers;

  /* FIXME: this use of restrict probably works, but I'm not completely sure
     it's formally correct.  Maybe the right solution is using an intentionally
     different type for the stack.  Or maybe, more simply, the stack bottom
     should not be part of the state. */
  union bad_word* restrict stack_undertop_pointer;
  union bad_word stack[VM_STACK_DEPTH];
  union bad_word stack_top;
};

typedef union bad_word
bad_instruction;

struct bad_program
{
  int instruction_no;
  bad_instruction *instructions;
  int first_instruction_index;
};

/* An expression computing a pointer obtained from a base in union bad_word* plus
   an offset in chars, as a union bad_word* .  This can be used as an lvalue. */
#define WITH_CHAR_OFFSET(base, offset_in_chars) \
  ((union bad_word*)(((char*)(base)) + (offset_in_chars)))

#define BAD_BRANCH(index_)                                  \
  do                                                        \
    {                                                       \
      ip = instructions + index_;                           \
      goto *(ip->p);                                        \
    }                                                       \
  while (false)

#define BAD_NEXT                                            \
  do                                                        \
    goto * ((++ ip)->p);                                    \
  while (false)

#define BAD_INSTRUCTION(name) \
  name: __attribute__ ((hot)); \
  asm ("# Instruction " #name)

__attribute__ ((noclone, noinline,
                /* This is the same as in vm/interpreter-private.h .
                   I should factor it.  Or remove this demo at some point. */
                optimize("O2", "omit-frame-pointer"
#ifdef VM_REPLICATE
                  /* Replication is very fragile, with correctness *relying* on
                     some GCC optimizations being enabled and others being
                     disabled.  [FIXME: explain the details.]  -O3 and -Ofast tend
                     to be pointless here, and it's better to reduce memory
                     usage a little when compiling the generated interpreter
                     files, which can quickly become huge when there are many VM
                     registers. */
#ifndef DASHFPIC_MAKES_GCC_SEGFAULT
                           , "pic" /*"PIE", "pie",*/ //,"PIC"
#endif // #ifndef DASHFPIC_MAKES_GCC_CRASH
                           , "plt"
                           , "no-reorder-blocks"
                           /* It makes no sense to align instructions with
                              replications, since I'm moving the code away
                              and recombining in unpredictable ways, therefore
                              breaking any alignment intended by the GCC; I may
                              reconsider this later if I actually make an effort
                              to maintain alignment in replicated code. */
                           , "no-align-loops", "no-align-jumps", "no-align-labels"
#else
                  /* Here I chose to be able to disassemble code, in exchange
                     for a little performance overhead.  I might want to
                     reconsider this in production. */
                  , "no-reorder-blocks"
#endif // #ifdef VM_REPLICATE
                           ))) void
initialize_or_interpret (struct bad_state *original_state,
                         struct bad_program *p,
                         const union bad_word * restrict the_program_literals,
                         bool initialize)
{
  if (initialize)
    {
      static const void* labels[] =
        {
          &&nop,
          &&incr_r0, &&incr_r1, &&incr_r2,
          &&incr_r2_skip,
          &&slow_add_indices,
          &&slow_add_offsets,
          &&add_r0_slow_r1,
          &&add_r0_r1_r1,
          &&add_r0_r1_r2,
          &&almost_trouble,
          //&&trouble,
          &&swap_r0_r1,
          &&car_r0_r1, &&cdr_r0_r1, &&cadr_r0_r1, &&caddr_r0_r1,
          &&forth_one_plus, &&forth_plus, &&forth_dup, &&forth_drop, &&forth_nip, &&forth_swap,
          &&branch, &&exit,
          //&&load_from_right_after_code_to_r0,
          &&load_from_literals_42_to_r0,
        };
      const int instruction_no = p->instruction_no;
      int i;
      for (i = 0; i < instruction_no; i ++)
        p->instructions[i].p = (void*) labels [i];
      return;
    }
  register const union bad_word * restrict program_literals = the_program_literals;
  struct bad_state s = *original_state;
  const bad_instruction *instructions = p->instructions;
  const bad_instruction *ip;
  BAD_BRANCH (p->first_instruction_index);

  BAD_INSTRUCTION(nop);
  BAD_NEXT;

  BAD_INSTRUCTION(incr_r0);
  s.registers[0].i ++;
  BAD_NEXT;

  BAD_INSTRUCTION(incr_r1);
  s.registers[1].i ++;
  BAD_NEXT;

  BAD_INSTRUCTION(incr_r2);
  s.registers[2].i ++;
  BAD_NEXT;

  BAD_INSTRUCTION(incr_r2_skip);
  s.registers[2].i ++;
  ip ++;
  BAD_NEXT;

  BAD_INSTRUCTION(slow_add_indices);
  {
    s.slow_registers[(ip + 3)->i].i
      = s.slow_registers[(ip + 1)->i].i + s.slow_registers[(ip + 2)->i].i;
    ip += 3;
  }
  BAD_NEXT;

  BAD_INSTRUCTION(slow_add_offsets);
  {
    WITH_CHAR_OFFSET(s.slow_registers, ((ip + 3)->i))->i
      = WITH_CHAR_OFFSET(s.slow_registers, ((ip + 1)->i))->i
        + WITH_CHAR_OFFSET(s.slow_registers, ((ip + 2)->i))->i;
    ip += 3;
    BAD_NEXT;
  }

  BAD_INSTRUCTION(add_r0_slow_r1); // the slow register is provided as an offset
  s.registers[1].i = s.registers[0].i + WITH_CHAR_OFFSET(s.slow_registers, ((ip + 1)->i))->i;
  ip ++;
  BAD_NEXT;

  BAD_INSTRUCTION(add_r0_r1_r1);
  s.registers[1].i = s.registers[0].i + s.registers[1].i;
  BAD_NEXT;

  BAD_INSTRUCTION(add_r0_r1_r2);
  s.registers[2].i = s.registers[0].i + s.registers[1].i;
  BAD_NEXT;

  BAD_INSTRUCTION(swap_r0_r1);
  {
    union bad_word copy_0 = s.registers[0];
    s.registers[0] = s.registers[1];
    s.registers[1] = copy_0;
  }
  BAD_NEXT;

  BAD_INSTRUCTION(car_r0_r1);
  s.registers[1].i = ((epsilon_int*)s.registers[0].p)[0];
  BAD_NEXT;

  BAD_INSTRUCTION(cdr_r0_r1);
  s.registers[1].i = ((epsilon_int*)s.registers[0].p)[1];
  BAD_NEXT;

  BAD_INSTRUCTION(cadr_r0_r1);
  { epsilon_int* cdr = ((epsilon_int**)s.registers[0].p)[1];
    epsilon_int cadr = ((epsilon_int*)cdr)[0];
    s.registers[1].i = cadr; }
  BAD_NEXT;

  BAD_INSTRUCTION(caddr_r0_r1);
  { epsilon_int* cdr = ((epsilon_int**)s.registers[0].p)[1];
    epsilon_int* cddr = ((epsilon_int**)cdr)[1];
    epsilon_int caddr = ((epsilon_int*)cddr)[0];
    s.registers[1].i = caddr; }
  BAD_NEXT;

  BAD_INSTRUCTION(forth_one_plus);
  s.stack_top.i ++;
  BAD_NEXT;

  BAD_INSTRUCTION(forth_plus);
  s.stack_top.i += (s.stack_undertop_pointer --)->i;
  BAD_NEXT;

  BAD_INSTRUCTION(forth_dup);
  (++ s.stack_undertop_pointer)->i = s.stack_top.i;
  BAD_NEXT;

  BAD_INSTRUCTION(forth_drop);
  s.stack_top.i = (s.stack_undertop_pointer --)->i;
  BAD_NEXT;

  BAD_INSTRUCTION(forth_nip);
  s.stack_undertop_pointer --;
  BAD_NEXT;

  BAD_INSTRUCTION(forth_swap);
  {
    union bad_word tos_copy = s.stack_top;
    s.stack_top = * s.stack_undertop_pointer;
    * s.stack_undertop_pointer = tos_copy;
  }
  BAD_NEXT;

  BAD_INSTRUCTION(almost_trouble);
  {
    if (s.registers[0].i < 0 || s.registers[0].i > 0)
      __builtin_unreachable ();
    else // s.registers[0].i is 0
      s.registers[s.registers[0].i].i = 0;
  }
  BAD_NEXT;

  /*
  BAD_INSTRUCTION(trouble);
  {
    if (s.registers[0].i < 0)
      __builtin_unreachable ();
    else if (s.registers[0].i > 1)
      __builtin_unreachable ();
    else // s.registers[0].i is 0 or 1
      s.registers[s.registers[0].i].i = 0;
  }
  BAD_NEXT;
  */

  BAD_INSTRUCTION(branch);
  ip ++;
  BAD_BRANCH (ip->i);
  BAD_NEXT;

  BAD_INSTRUCTION(exit);
  *original_state = s;

  /*
  BAD_INSTRUCTION(load_from_right_after_code_to_r0);
  s.registers[0].i
    = * (epsilon_int*)&&right_after_the_instruction;
  BAD_NEXT;
 right_after_the_instruction:
#if SIZEOF_VOID_P * CHAR_BIT == 32
  asm volatile(".long 42");
#elif SIZEOF_VOID_P * CHAR_BIT == 64
  asm volatile(".quad 42");
#else
  #error The word size is not 32 nor 64 bits.
#endif
  */

  BAD_INSTRUCTION(load_from_literals_42_to_r0);
  s.registers[0].i = program_literals[42].i;
  BAD_NEXT;

  return;
}
