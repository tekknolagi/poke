/* Jitter: VM-independent program data structures: header.

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


#ifndef JITTER_PROGRAM_H_
#define JITTER_PROGRAM_H_

#include <stdio.h>

#include <jitter/jitter.h>
#include <jitter/jitter-dynamic-buffer.h>
#include <jitter/jitter-hash.h>
#include <jitter/jitter-instruction.h>


/* Program data structures.
 * ************************************************************************** */

// FIXME: comment.
enum jitter_program_stage
  {
    jitter_program_stage_unspecialized,
    jitter_program_stage_specialized,
    jitter_program_stage_replicated
  };

/* A descriptor for a replicated block of code. */
struct jitter_replicated_block
{
  /* The opcode of the specialized instruction the block is a translation of.
     This is actually an enum vmprefix_specialized_instruction_opcode , but
     since the fixnum type is wide enough to represent it on any VM we can
     define this once and for all. */
  //enum vmprefix_specialized_instruction_opcode specialized_opcode;
  jitter_uint specialized_opcode;

  /* A pointer to the beginning ot the native code which is the translation of
     one specific instance of a specialized instruction; the native code may
     also include (at the beginning) some instructions to load residual
     arguments.
     The pointer is NULL as long as the code has not been replicated.
     When non-NULL this field points into the same native_code block pointed to
     by a program, so the referred memory must not be freed separately. */
  char *native_code;

  /* The native code size, in bytes.  This is not necessarily the same as the
     specialized_opcode-th element of vmprefix_thread_sizes, again because of added
     code to load residuals.
     The size is 0 as long as the block has not been replicated. */
  size_t native_code_size;
};

/* The internal representation of a program.  This should be considered
   an abstract type, as the internal structure is subject to change. */
struct jitter_program
{
  /* The program stage at the present time. */
  enum jitter_program_stage stage;

  /* A dynamic array containing struct jitter_instruction * elements.  This is
     filled by parsing or by initialization with unspecialized instructions.
     The buffer contains pointers rather than directly instructions to make
     rewriting easier. */
  struct jitter_dynamic_buffer instructions;

  /* The opaque label to be generated as fresh at the next request. */
  jitter_label next_unused_opaque_label;

  /* A map associating symbolic label names to opaque labels.  There can be
     opaque labels without any symbolic names, but every symbolic name in use
     must always be associated to an opaque label.  The datum is handled as a
     pointer by the hash API, but here it should just be cast to and from an
     jitter_label .  Symbolic label names are copied into the map
     whenever a binding is added, and do not share memory with user data
     structures. */
  struct jitter_hash_table label_name_to_opaque_label;

  /* A dynamic array containing jitter_int elements.  Each array index
     represents an opaque label, and its associated array element the
     unspecialized instruction index where that label leads, or -1 if the
     label is unresolved.  It is not an error to have unresolved labels,
     even late after label resolution, as long as such labels are never
     used in instruction parameters.
     Rewriting poses no particular problem with respect to this mapping:
     once a label is assigned an instruction index, the *following*
     instructions may be rewritten, but not the previous one, maintaining
     the existing mapping valid. */
  struct jitter_dynamic_buffer opaque_label_to_instruction_index;

  /* A pointer to the instruction currently being initialized within
     instructions. */
  struct jitter_instruction *current_instruction;

  /* The next uninitialized parameter of *current_instruction . */
  struct jitter_parameter *next_uninitialized_parameter;

  /* The next expected parameter type, which *next_uninitialized_parameter will
     need to match. */
  const struct jitter_meta_instruction_parameter_type *
  next_expected_parameter_type;

  /* How many parameters are we still expecting before completing the
     instruction which is currently being appended.  If no instruction is
     incomplete, including right after initialization, the field is zero. */
  int expected_parameter_no;

  /* How many complete instructions at the end of the program are candidate for
     rewriting.  This starts at zero, and gets reset every time a label is
     added. */
  size_t rewritable_instruction_no;

  /* A pointer to a malloced array of booleans, having the same size as the
     number of instructions in the program.  Each element of the array is true
     if and only if the corresponding program instruction is a jump target.
     This is allocated at specialization time, and NULL before. */
  bool *jump_targets;

  /* We need to map unspecialized instruction indices into specialized
     instruction offsets within specialized_program, in chars from the
     beginning.  This is needed because specialized instructions have variable
     sizes, and label arguments need to be backpatched in to point to
     instruction beginning addresses rather than indices.
     Notice that this array is indexed by unspecialized instruction indices, and
     that not every possible index is valid: if an unspecialized instruction
     interval is specialized into one superinstruction then only its first
     unspecialized instruction in the interval has a corresponding
     superinstruction. */
  jitter_int *instruction_index_to_specialized_instruction_offset;

  /* The sequence of replicated blocks in the specialized program, in order.
     Each element is a struct jitter_replicated_block .  This is only used with
     replication, and for disassembly. */
  struct jitter_dynamic_buffer replicated_blocks;

  /* Generated native code and its size, only used with replication [FIXME:
     generalize later, after I introduce alternatives even more sophisticated
     than replication, which will probably still need this]. */
  char *native_code;
  size_t native_code_size;

  /* The specialized program.  Each element is a union jitter_specialized_word . */
  struct jitter_dynamic_buffer specialized_program;

  /* The indices of label parameters within specialized_instructions as
     jitter_int's, to be patched at the end of specialization.  While
     specialized_instructions is being added to, label parameters are
     instruction indices; after patching they become pointers to the beginning
     of instructions within specialized_instructions. */
  struct jitter_dynamic_buffer specialized_label_indices;

  /* The number of slow registers needed *per class* in order to run this
     program; or, alternatively, the maximum number of slow registers needed to
     run this program in any class.  Slow registers are always added in the same
     number for all classes, even if each class may have a different number of
     fast registers. */
  jitter_int slow_register_per_class_no;

  /* A pointer to the VM-specific definitions of the VM for this program. */
  const struct jitter_vm *vm;
};

/* Return the number of unspecialized instruction in the pointed program
   also counting the one which is currently incomplete, if any. */
size_t
jitter_program_instruction_no (const struct jitter_program *p)
  __attribute__ ((pure));

/* Return a freshly-allocated, empty program with an empty vm field. */
struct jitter_program*
jitter_make_program (const struct jitter_vm *vm)
  __attribute__ ((nonnull (1), returns_nonnull));

/* Destroy the pointed program. */
void
jitter_destroy_program (struct jitter_program *p);




/* Label handing.
 * ************************************************************************** */

/* Return a fresh label for the pointed program, without an associated symbolic
   name.  The caller may later use the label in an instruction argument or
   associate it to a program point. */
jitter_label
jitter_fresh_label (struct jitter_program *p)
  __attribute__ ((nonnull (1)));

/* Return a label for the pointed program, associated to the given symbolic
   name.  If the symbolic name is new for the program, associate the label
   to an internally-allocated copy of it; if the symbolic name is already
   known, return the label already associated to it. */
jitter_label
jitter_symbolic_label (struct jitter_program *p, const char *symbolic_name)
  __attribute__ ((nonnull (1)));




/* Program construction API.
 * ************************************************************************** */

/* Update the given program, adding the given label before the instruction which
   is coming next.
   When this function is called the previous instruction, if any, must have been
   completed. */
void
jitter_append_label (struct jitter_program *p,
                     jitter_label label)
  __attribute__ ((nonnull (1)));

/* Update the given program, adding a label with the given name before the
   instruction which is coming next, and return the label.
   When this function is called the previous instruction, if any, must have been
   completed. */
jitter_label
jitter_append_symbolic_label (struct jitter_program *p,
                              const char *label_name)
  __attribute__ ((nonnull (1, 2)));

/* Update the given program, beginning a new instruction with the given name, to
   be looked up in the meta-instruction hash table; the instruction parameters,
   if any, have to be supplied with calls to vmprefix_append_*_parameter .  When
   this function is called the previous instruction, if any, must have been
   completed.

   This function is convenient but requires a hash lookup on the name.
   jitter_append_meta_instruction is faster; see its comment for the recommended
   way to use it. */
void
jitter_append_instruction_name (struct jitter_program *p,
                                const char *instruction_name);

/* Update the given program, beginning a new instruction which is an instance of
   the pointed meta-instruction; the instruction parameters, if any, have to be
   supplied with calls to vmprefix_append_*_parameter .  When this function is
   called the previous instruction, if any, must have been completed.

   Supplying a meta-instruction pointer from a user program is very
   inconvenient.  The recommended way of using this function, which is more
   efficient than jitter_append_instruction_name, is thru the machine-generated
   macro [VMPREFIX]_APPEND_INSTRUCTION. */
void
jitter_append_meta_instruction (struct jitter_program *p,
                                const struct jitter_meta_instruction * const
                                mi);

/* Update the given program, adding one more parameter (left-to-right) to the
   unspecialized instruction currently being described.  Fail fatally if there
   are no instructions yet, or the last added instruction is already
   complete.  If appending a symbolic-label argument, return its opaque
   label identifier.
   Notice that the macro [VMPREFIX]_APPEND_REGISTER_PARAMETER provides a more
   convenient way of adding a register parameter. */
void
jitter_append_literal_parameter (struct jitter_program *p,
                                 union jitter_word immediate)
  __attribute__((nonnull (1)));
void
jitter_append_signed_literal_parameter (struct jitter_program *p,
                                        jitter_int immediate)
  __attribute__((nonnull (1)));
void
jitter_append_unsigned_literal_parameter (struct jitter_program *p,
                                          jitter_uint immediate)
  __attribute__((nonnull (1)));
void
jitter_append_pointer_literal_parameter (struct jitter_program *p,
                                         void *immediate)
  __attribute__((nonnull (1)));
void
jitter_append_register_parameter (struct jitter_program *p,
                                  const struct jitter_register_class *c,
                                  jitter_register_index register_index)
  __attribute__((nonnull (1, 2)));
jitter_label
jitter_append_symbolic_label_parameter (struct jitter_program *p,
                                        const char *label_name)
  __attribute__((nonnull (1, 2)));
void
jitter_append_label_parameter (struct jitter_program *p,
                               jitter_label label)
  __attribute__((nonnull (1)));




/* Lower-level program-construction API.
 * ************************************************************************** */

/* This is used internally for rewriting. */

/* Append the pointed instruction (without cloning it) to the pointed program.
   When this function is called the previous instruction, if any, must have been
   completed. */
void
jitter_append_instruction (struct jitter_program *p,
                           const struct jitter_instruction *ip);




/* Unspecialized program printer.
 * ************************************************************************** */

/* Print a readable representation of the pointed program to the pointed
   stream. */
void
jitter_print_program (FILE *out, const struct jitter_program *p);

/* Like jitter_print_program , but also accept a boolean parameter telling
   whether the program has been parsed so as to use slow registers only.  By
   passing true the program is un-modified for printing, so that the register
   indices appear like in the original VM program. */
void
jitter_print_program_possibly_with_slow_registers_only
   (FILE *out,
    const struct jitter_program *p,
    bool slow_registers_only);




/* Jump target computation on unspecialized programs.
 * ************************************************************************** */

/* Given a program return a pointer to a new array of booleans, allocated with
   malloc, having the same size as the number of instructions in the program.
   Each element of the array is true if and only if the corresponding program
   instruction is a jump target.

   This is used at specialization time to compute the jump_targets field of a
   struct jitter_program , but also elsewhere, for printing unspecialized programs
   -- therefore it cannot be a static function.

   This function is used internally, and the user does not need to see it. */
bool*
jitter_jump_targets (const struct jitter_program *p)
  __attribute__ ((returns_nonnull, nonnull (1)));




/* Label resolution in unspecialized programs.
 * ************************************************************************** */

/* Resolve label arguments in unspecialized instruction parameters, replacing
   opaque labels with unspecialized instruction indices.  After this is done
   instruction parameters refer labels as unspecialized instruction indices.
   Fail fatally if any referred label is still undefined, or if the program is
   not unspecialized. */
void
jitter_resolve_labels_in_unspecialized_program (struct jitter_program *p)
  __attribute__ ((nonnull (1)));

#endif // #ifndef JITTER_PROGRAM_H_
