/* Instruction rewrite functionality header.

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


#ifndef JITTER_REWRITE_H_
#define JITTER_REWRITE_H_

#include <jitter/jitter.h>
#include <jitter/jitter-instruction.h>
#include <jitter/jitter-program.h>


/* Rewriter API.
 * ************************************************************************** */

/* This functionality is used internally by the rewriter, and not intended for
   the user. */

/* FIXME: verify that this comment is still all correct after I actually
   implement the system.  I suspect my ideas have evolved already since the time
   I wrote it. */
/* Instruction rewriting entails replacing one unspecialized instruction with a
   sequence of zero or more equivalent unspecified instructions; each
   instruction in the replacement will be specializable, while the original
   instruction might not be.

   The rewriting mechanism is intended for reducing the number of specialized
   instructions by removing redundant ways of describing the same operation.
   Let us assume for example a meta-instruction add (?r, ?r, !r); then the
   two unspecialized instructions
     add %r0, %r1, %r2
   and
     add %r1, %r0, %r2
   have the same semantics because of commutativity, and we can systematically
   rewrite one of them into the other (for example the one having input register
   indices in non-decreasing order), arbitrarily chosen as the normal form.
   This cuts the specialized instructions in half, allowing the user to compile
   a Jittery system using less resources, or to keep using the same resources
   but affording a higher number of fast registers.

   Other rewritings yield slightly suboptimal code, but reduce the specialized
  instruction set even more dramatically.  The add instruction above has O(n^3)
   specializations, but by requring the second and third arguments to be equal
   we can cut specialized instructions to O(n^2).  Assuming a register copying
   meta-instruction mov (?r, !r) such a rewriting would require replacing, for
   example,
     add %r0, %r1, %r2
   with the two-instruction sequence
     mov %r1, %r2
     add %r0, %r2, %r2
   . Other add instructions already having the same register as their second
   and third arguments would not be rewritten.

   Rewriting happens after each instruction is completed, by a call to
   jitter_append_instruction_name or one of the jitter_append_*_parameter
   functions.  Rewriting can never happen "across" a label: an original
   unspecialized instruction before rewriting is replaced with a sequence, and
   both the original and the rewritten sequence lie completely before or
   completely after each label: it is not possible, by construction, to have part
   of the replacement before and part of the replacement after a label. */




/* Rewriter entry point.
 * ************************************************************************** */

/* Perform rewriting on the last part of the pointed program, until no more
   changes are possible.

   This is called every time an instruction is appended, either by the user
   or by the rewriter itself.  */
void
jitter_rewrite (struct jitter_program *p);




/* Rewriter internal functions.
 * ************************************************************************** */

/* These functions are used internally for the implementation of
   jitter_rewrite. */

/* Return a pointer to the last instruction in the given program; the returned
   pointer refers the only instance of the instruction, and is not a copy.  Fail
   fatally if no instructions exist or if the last one is not complete, or if the
   program is not unspecialized. */
const struct jitter_instruction*
jitter_last_instruction (struct jitter_program *p);

/* Return a pointer pointing within an array of pointers to instructions,
   how_many elements from the last one.  The pointed memory may be invalidated
   by any instruction modification, so this is only meant for *reading* the last
   few instructions in order to check whether a rewrite rule applies.
   The intended way of using this is for checking whether a rule applying
   to N instruction can fire: it will be called with how_many = N, and
   the result will be an array of N pointers to the last instructions. */
/* Fail fatally if no instructions exist or if the last one is not complete, or
   if the program is not unspecialized.  [FIXME: possibly change this] */
const struct jitter_instruction**
jitter_last_instructions (struct jitter_program *p, size_t how_many);

/* Pop the last instruction off the pointed program, which must be complete, and
   return a pointer to it; fail fatally if the instruction is not complete or
   does not exist, or if the program is unspecialized.  This removes the pointer
   to the instruction from the dynamic buffer in the program, but does not
   deallocate the instruction: in fact the result of this function is the
   pointer to the still-valid instruction, allocated according to the
   conventions in jitter-instruction.h .  It is the caller's responsibility to
   release memory after usage, normally by a call to jitter_destroy_instruction
   .  The intended use case for this is fisrt checking whether a replacement
   should occour, and then if so calling jitter_pop_instruction once; the result
   pointer will be used to determine which instructions to append as a
   replacement, by the usual jitter_append_instruction_name /
   jitter_append_*_parameter functions, which may in their turn trigger rewrites
   -- or by jitter_append_instruction, which also triggers rewrites.

   It is the user's responsiblity to ensure that her rewrite rules don't loop
   forever. */
struct jitter_instruction*
jitter_pop_instruction (struct jitter_program *p);


#endif // #ifndef JITTER_REWRITE_H_
