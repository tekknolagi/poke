/* Jitter: VM-independent routine data structures.

   Copyright (C) 2016, 2017, 2018, 2019 Luca Saiu
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
#include <string.h>

#include <jitter/jitter.h>
#include <jitter/jitter-mmap.h>
#include <jitter/jitter-routine.h>
#include <jitter/jitter-vm.h>
#include <jitter/jitter-rewrite.h>

#include <jitter/jitter-hash.h>
#include <jitter/jitter-malloc.h>
#include <jitter/jitter-fatal.h>

#include "config.h" // this is the non-Gnulib file.


/* Program options.
 * ************************************************************************** */

/* Set the pointed program-option struct to the default state. */
static void
jitter_initialize_options (struct jitter_routine_options *op)
{
  op->can_change = true;
  op->slow_registers_only = false;
  op->slow_literals_only = false;
  op->add_final_exitvm = true;
  op->optimization_rewriting = true;
}

/* Change the options in the given program to make options unchangeable from now
   on.  This is harmless to call even if the options are already unchangeable.
   In practice this is called when appending labels and meta-instructions; it is
   not necessary to call when appending instruction parameters, as those always
   follow meta-instructions.  Based on the same reasoning, rewriting rules
   always follow instruction appending, so the low-level API only used for
   rewriting does not need to use this. */
static void
jitter_routine_make_options_unchangeable (struct jitter_routine *p)
{
  p->options.can_change = false;
}

/* Fail fatally if the options in the pointed program are no changeable. */
static void
jitter_fail_unless_options_changeable (struct jitter_routine *p)
{
  if (! p->options.can_change)
    jitter_fatal ("cannot change options in non-empty program");
}

void
jitter_set_routine_option_slow_registers_only (struct jitter_routine *p,
                                               bool option)
{
  jitter_fail_unless_options_changeable (p);
  p->options.slow_registers_only = option;
}

void
jitter_set_routine_option_slow_literals_only (struct jitter_routine *p,
                                              bool option)
{
  jitter_fail_unless_options_changeable (p);
  p->options.slow_literals_only = option;
}

void
jitter_set_routine_option_slow_literals_and_registers_only
   (struct jitter_routine *p, bool option)
{
  jitter_set_routine_option_slow_registers_only (p, option);
  jitter_set_routine_option_slow_literals_only (p, option);
}

void
jitter_set_routine_option_add_final_exitvm (struct jitter_routine *p,
                                            bool option)
{
  jitter_fail_unless_options_changeable (p);
  p->options.add_final_exitvm = option;
}

void
jitter_set_routine_option_optimization_rewriting (struct jitter_routine *p,
                                                  bool option)
{
  jitter_fail_unless_options_changeable (p);
  p->options.optimization_rewriting = option;
}



/* Initialization and finalization
 * ************************************************************************** */

static void
jitter_initialize_program (struct jitter_routine *p)
{
  p->stage = jitter_routine_stage_unspecialized;

  jitter_initialize_options (& p->options);
  p->expected_parameter_no = 0;
  p->rewritable_instruction_no = 0;
  p->current_instruction = NULL;
  p->next_uninitialized_parameter = NULL; /* An intentionally invalid value. */
  p->next_expected_parameter_type = NULL; /* An intentionally invalid value. */

  jitter_dynamic_buffer_initialize (& p->instructions);
  p->next_unused_opaque_label = 0;
  jitter_hash_initialize (& p->label_name_to_opaque_label);
  jitter_dynamic_buffer_initialize (& p->opaque_label_to_instruction_index);
  p->jump_targets = NULL;
  p->instruction_index_to_specialized_instruction_offset = NULL;

  jitter_dynamic_buffer_initialize (& p->specialized_program);
  jitter_dynamic_buffer_initialize (& p->replicated_blocks);
  p->native_code = NULL;
  jitter_dynamic_buffer_initialize (& p->specialized_label_indices);

  /* No slow registers have been seen yet. */
  p->slow_register_per_class_no = 0;
}

static void
jitter_finalize_program (struct jitter_routine *p)
{
  struct jitter_dynamic_buffer * const is = & p->instructions;
  while (jitter_dynamic_buffer_size (is) != 0)
    {
      struct jitter_instruction *i
        = * (struct jitter_instruction **)
            jitter_dynamic_buffer_pop (is, sizeof (struct jitter_instruction*));
      jitter_destroy_instruction (i);
    }
  jitter_dynamic_buffer_finalize (is);

  jitter_string_hash_finalize (& p->label_name_to_opaque_label, NULL);
  jitter_dynamic_buffer_finalize (& p->opaque_label_to_instruction_index);

  if (p->jump_targets != NULL)
    free (p->jump_targets);
  if (p->instruction_index_to_specialized_instruction_offset != NULL)
    free (p->instruction_index_to_specialized_instruction_offset);

  jitter_dynamic_buffer_finalize (& p->specialized_program);
  jitter_dynamic_buffer_finalize (& p->replicated_blocks);
#ifdef JITTER_REPLICATE  
  if (p->native_code != 0)
    jitter_executable_deallocate (p->native_code);
#endif // #ifdef JITTER_REPLICATE  
  jitter_dynamic_buffer_finalize (& p->specialized_label_indices);
}

struct jitter_routine*
jitter_make_routine (const struct jitter_vm *vm)
{
  struct jitter_routine *res = jitter_xmalloc (sizeof (struct jitter_routine));
  jitter_initialize_program (res);
  res->vm = vm;
  return res;
}

void
jitter_destroy_routine (struct jitter_routine *p)
{
  if (p == NULL)
    return;

  jitter_finalize_program (p);
  free (p);
}

size_t
jitter_routine_instruction_no (const struct jitter_routine *p)
{
  return jitter_dynamic_buffer_size (& p->instructions)
         / sizeof (struct jitter_instruction*);
}




/* Label handing.
 * ************************************************************************** */

jitter_label
jitter_fresh_label (struct jitter_routine *p)
{
  /* Allocate an identifier. */
  jitter_label res = p->next_unused_opaque_label ++;

  /* Associate the label to an invalid instruction index. */
  jitter_int invalid_instruction_index = -1;
  jitter_dynamic_buffer_push (& p->opaque_label_to_instruction_index,
                              & invalid_instruction_index,
                              sizeof (invalid_instruction_index));

  /* Return the identifier. */
  return res;
}

jitter_label
jitter_symbolic_label (struct jitter_routine *p, const char *symbolic_name)
{
  /* If the name is already known, return its label. */
  if (jitter_string_hash_table_has (& p->label_name_to_opaque_label,
                                    symbolic_name))
    return jitter_string_hash_table_get (& p->label_name_to_opaque_label,
                                         symbolic_name).fixnum;

  /* The name is new.  Allocate a new label, bind it to a copy of the name
     (copies are handled by the hash table routines), and return the label. */
  jitter_label res = jitter_fresh_label (p);
  union jitter_word datum = {.fixnum = res};
  jitter_string_hash_table_add (& p->label_name_to_opaque_label,
                                symbolic_name,
                                datum);
  return res;
}

/* Return the unspecialized instruction index for the given label in the pointed
   program, or -1 if the label is not associated to any instruction.
   Unspecified behavior if the label is out of bounds. */
static jitter_int
jitter_get_label_instruction_index (struct jitter_routine *p,
                                    jitter_label label)
{
  jitter_int *array
    = jitter_dynamic_buffer_to_pointer (& p->opaque_label_to_instruction_index);

  return array [label];
}

/* Associate the given label in the pointed program to the given unspecialized
   instruction index.  Fail fatally if the label was already associated to an
   index.  Unspecified behavior if the label is out of bounds. */
static void
jitter_set_label_instruction_index (struct jitter_routine *p,
                                    jitter_label label,
                                    jitter_int instruction_index)
{
  jitter_int *array
    = jitter_dynamic_buffer_to_pointer (& p->opaque_label_to_instruction_index);
  jitter_int previous_index = array [label];
  if (previous_index != -1)
    jitter_fatal ("label %li appended twice", (long) label);

  array [label] = instruction_index;
}




/* Program construction API.
 * ************************************************************************** */

void
jitter_append_label (struct jitter_routine *p, jitter_label label)
{
  if (p->stage != jitter_routine_stage_unspecialized)
    jitter_fatal ("appending label in non non-unspecialized program");
  if (p->expected_parameter_no != 0)
    jitter_fatal ("appending label %li with previous instruction "
                  "incomplete", (long) label);
  jitter_routine_make_options_unchangeable (p);

  jitter_int instruction_index = jitter_routine_instruction_no (p);
  jitter_set_label_instruction_index (p, label, instruction_index);

  /* We added a label.  Everything before it can no longer be rewritten. */
  p->rewritable_instruction_no = 0;
}

jitter_label
jitter_append_symbolic_label (struct jitter_routine *p, const char *label_name)
{
  jitter_label res = jitter_symbolic_label (p, label_name);
  jitter_append_label (p, res);
  return res;
}

/* Close the current instruction which must have all of its parameters already
   added (fail fatally otherwise), by calling the rewriter on it.  Set pointers
   about the next parameter in the program to reflect the fact that its address
   is not known yet, and a new instruction is expected first.  Increment the
   instruction counter. */
static void
jitter_close_current_instruction (struct jitter_routine *p)
{
  if (p->stage != jitter_routine_stage_unspecialized)
    jitter_fatal ("closing instruction in non-unspecialized program");
  if (p->expected_parameter_no != 0)
    jitter_fatal ("closing an instruction still expecting parameters");

  p->next_uninitialized_parameter = NULL;
  p->next_expected_parameter_type = NULL;

  /* The instruction we just added is a candidate for rewriting, along with the
     previous ones which were candidate already. */
  p->rewritable_instruction_no ++;

  /* Unless optimization rewrites were disabled for this program, rewrite the
     last part of the program, using the instruction we have just closed; that
     instruction, along with some others preceding it, might very well change or
     disappear after rewriting is done. */
  if (p->options.optimization_rewriting)
    p->vm->rewrite (p);
}

/* Check that the pointed program's last instruction is incomplete, and that the next
   parameter it expects is compatible with the given actual type and, in case it's a
   register, register class.  Fail fatally if that's not the case. */
static void
jitter_check_paremater_compatibility
   (struct jitter_routine *p,
    enum jitter_parameter_type actual_type,
    const struct jitter_register_class *register_class)
{
  /* Check that the program is in the right stage, and that we're
     actually expecting a parameter. */
  if (p->stage != jitter_routine_stage_unspecialized)
    jitter_fatal ("appending parameter in non-unspecialized program");
  if (p->expected_parameter_no == 0)
    jitter_fatal ("appending parameter with previous instruction complete");
  if (p->next_expected_parameter_type == NULL)
    jitter_fatal ("impossible if we passed the previous check");

  /* Check that the parameter type is compatible with what we expect. */
  const struct jitter_meta_instruction_parameter_type * expected_type =
    p->next_expected_parameter_type;
  enum jitter_meta_instruction_parameter_kind expected_kind =
    expected_type->kind;
  switch (actual_type)
    {
    case jitter_parameter_type_register_id:
      if (   expected_kind
             != jitter_meta_instruction_parameter_kind_register
          && expected_kind
             != jitter_meta_instruction_parameter_kind_register_or_literal_fixnum
          && expected_kind
             != jitter_meta_instruction_parameter_kind_register_or_literal_label
          && expected_kind
             != jitter_meta_instruction_parameter_kind_register_or_literal_fixnum_or_literal_label)
        jitter_fatal ("appending register argument not admitted by instruction");
      if (expected_type->register_class != register_class)
        jitter_fatal ("invalid register class for register argument");
      break;
    case jitter_parameter_type_literal:
      if (   expected_kind
             != jitter_meta_instruction_parameter_kind_literal_fixnum
          && expected_kind
             != jitter_meta_instruction_parameter_kind_literal_fixnum_or_literal_label
          && expected_kind
             != jitter_meta_instruction_parameter_kind_register_or_literal_fixnum
          && expected_kind
             != jitter_meta_instruction_parameter_kind_register_or_literal_fixnum_or_literal_label)
        jitter_fatal ("appending immediate argument not admitted by instruction");
      break;
    case jitter_parameter_type_label:
      if (   expected_kind
             != jitter_meta_instruction_parameter_kind_literal_label
          && expected_kind
             != jitter_meta_instruction_parameter_kind_literal_fixnum_or_literal_label
          && expected_kind
             != jitter_meta_instruction_parameter_kind_register_or_literal_label
          && expected_kind
             != jitter_meta_instruction_parameter_kind_register_or_literal_fixnum_or_literal_label)
        jitter_fatal ("appending label argument not admitted by instruction");
      break;
    default:
      jitter_fatal ("jitter_append_uninitialized_paremater: "
                    "invalid actual argument type %lu",
                    (unsigned long) actual_type);
    }
}

/* Make the current instruction, which must be incomplete, have the next
   expected parameter as its own and start a new instruction if that parameter
   was the last, but *don't* close the old instruction.  Don't make checks. */
static void
jitter_advance_past_next_parameter (struct jitter_routine *p)
{
  if ((-- p->expected_parameter_no) != 0)
    {
      const struct jitter_instruction *in = p->current_instruction;
      const struct jitter_meta_instruction *min = in->meta_instruction;

      /* There are other parameters expected in the current instruction after
         the one we have just added.  Point to the next one. */
      p->next_uninitialized_parameter
        = in->parameters [min->parameter_no - p->expected_parameter_no];
      p->next_expected_parameter_type ++;
    }
}

/* Add a parameter of the given parameter type (which is a
   jitter_parameter_type, therefore an "actual" type and *not* a
   jitter_meta_instruction_parameter type, expressing the set of "formal"
   accepted types) to the last instruction of the given unspecialized program,
   which must be incomplete.  A register class is also given to check that it
   matches with the expected parameter, in case of a register-type parameter; it
   is ignored otherwise.
   Fail fatally if the program is not unspecialized
   or its last instruction is complete, or if the kind or register class is
   wrong.  Return a pointer to the parameter data structure, to be filled in by
   the caller.  In any case, do *not* close the instruction, even if the appended
   parameter was the last; doing that would interfere badly with rewriting, which
   has to see every parameter correctly initialized.
   This is used by jitter_append_literal_parameter ,
   jitter_append_register_parameter and
   jitter_append_label_parameter . */
static struct jitter_parameter *
jitter_append_uninitialized_paremater
   (struct jitter_routine *p,
    enum jitter_parameter_type actual_type,
    const struct jitter_register_class *register_class)
{
  /* Check that the parameter is compatbile; fail fatally if it isn't. */
  jitter_check_paremater_compatibility (p, actual_type, register_class);

  /* Keep a pointer to the next uninitialized program to be returned; it will no
     longer be the next at the end of this function. */
  struct jitter_parameter *res = p->next_uninitialized_parameter;

  /* Advance pointers in the program past this parameter, starting a new
     instruction if needed -- but don't close the current instruction: see the
     comment before this function. */
  jitter_advance_past_next_parameter (p);

  return res;
}

/* Close the current instruction if the last appended parameter was the last
   one.  See the comment before as to why this is not done there. */
static void
jitter_close_instruction_when_no_more_parameters (struct jitter_routine *p)
{
  if (p->expected_parameter_no == 0)
    jitter_close_current_instruction (p);
}

void
jitter_append_literal_parameter (struct jitter_routine *p,
                                 union jitter_word immediate)
{
  struct jitter_parameter * const pa
    = jitter_append_uninitialized_paremater (p, jitter_parameter_type_literal,
                                             NULL);

  pa->type = jitter_parameter_type_literal;
  pa->literal = immediate;
  jitter_close_instruction_when_no_more_parameters (p);
}

/* This is just a convenience wrapper around jitter_append_literal_parameter
   . */
void
jitter_append_signed_literal_parameter (struct jitter_routine *p,
                                        jitter_int immediate)
{
  union jitter_word immediate_union = {.fixnum = immediate};
  jitter_append_literal_parameter (p, immediate_union);
}

/* This is just a convenience wrapper around jitter_append_literal_parameter
   . */
void
jitter_append_unsigned_literal_parameter (struct jitter_routine *p,
                                          jitter_uint immediate)
{
  union jitter_word immediate_union = {.ufixnum = immediate};
  jitter_append_literal_parameter (p, immediate_union);
}

/* This is just a convenience wrapper around jitter_append_literal_parameter
   . */
void
jitter_append_pointer_literal_parameter (struct jitter_routine *p,
                                         void *immediate)
{
  union jitter_word immediate_union = {.pointer = immediate};
  jitter_append_literal_parameter (p, immediate_union);
}

void
jitter_append_register_parameter
   (struct jitter_routine *p,
    const struct jitter_register_class *register_class,
    jitter_register_index register_index)
{
  /* If we have to always residualize registers, then increment this register
     index by the number of fast registers in the class.  This way it is
     guaranteed that the actual register used in execution will be slow.  (When
     printing out the program the register index will be shown *decremented* by
     the number of fast registers in the class to compensate for this.) */
  if (p->options.slow_registers_only)
    register_index += register_class->fast_register_no;

  /* Append the register parameter. */
  struct jitter_parameter * const pa
    = jitter_append_uninitialized_paremater
         (p, jitter_parameter_type_register_id, register_class);
  pa->type = jitter_parameter_type_register_id;
  pa->register_index = register_index;
  pa->register_class = register_class;
  jitter_close_instruction_when_no_more_parameters (p);

  /* If this register is slow and its slow index is the highest up to this
     point, record it: it will be needed to know how many slow registers to
     allocate.  Notice that a negative slow_index (indicating a fast register)
     will never satisfy the if condition, as p->slow_register_per_class_no is
     initialized to zero and non-decreasing. */
  jitter_int slow_index = register_index - register_class->fast_register_no;
  jitter_int slow_register_no = slow_index + 1;
  if (slow_register_no > p->slow_register_per_class_no)
    p->slow_register_per_class_no = slow_register_no;
}

jitter_label
jitter_append_symbolic_label_parameter (struct jitter_routine *p,
                                        const char *label_name)
{
  jitter_label res = jitter_symbolic_label (p, label_name);
  jitter_append_label_parameter (p, res);
  return res;
}
void
jitter_append_label_parameter (struct jitter_routine *p,
                               jitter_label label)
{
  struct jitter_parameter * const pa
    = jitter_append_uninitialized_paremater
         (p, jitter_parameter_type_label, NULL);
  pa->type = jitter_parameter_type_label;
  pa->label = label;
  jitter_close_instruction_when_no_more_parameters (p);
}

void
jitter_append_meta_instruction (struct jitter_routine *p,
                                const struct jitter_meta_instruction * const mi)
{
  if (p->stage != jitter_routine_stage_unspecialized)
    jitter_fatal ("appending instruction %s in non-unspecialized program",
                  mi->name);
  if (p->expected_parameter_no != 0)
    jitter_fatal ("appending instruction %s with previous instruction"
                  " incomplete", mi->name);
  jitter_routine_make_options_unchangeable (p);

  /* Make the instruction. */
  struct jitter_instruction *i
    = p->current_instruction
    = jitter_make_instruction (mi);

  /* Add a pointer to it to the dynamic buffer for instructions within the
     program. */
  jitter_dynamic_buffer_push (& p->instructions,
                              & i,
                              sizeof (struct jitter_meta_instruction *));

  /* If this instruction has zero parameters then we're already done with it, and
     we can close it immediately.  Otherwise set parameter pointers to the frist
     parameter to be appended. */
  if ((p->expected_parameter_no = mi->parameter_no) == 0)
    jitter_close_current_instruction (p);
  else
    {
      p->next_uninitialized_parameter = i->parameters [0];
      p->next_expected_parameter_type = mi->parameter_types;
    }
}

void
jitter_append_instruction_id (struct jitter_routine *p,
                              const struct jitter_meta_instruction * const mis,
                              size_t meta_instruction_no,
                              unsigned unspecialized_opcode)
{
  /* Sanity check. */
  if (unspecialized_opcode >= meta_instruction_no)
    jitter_fatal ("jitter_append_instruction_id: invalid id %u",
                  unspecialized_opcode);

  /* Find the address of the appropriate meta-instruction in the array. */
  const struct jitter_meta_instruction * const mi
    = mis + unspecialized_opcode;

  /* Append the meta-instruction. */
  jitter_append_meta_instruction (p, mi);
}

void
jitter_append_instruction_name (struct jitter_routine *p,
                                const char *instruction_name)
{
  const struct jitter_meta_instruction * const mi
    = jitter_lookup_meta_instruction (p->vm->meta_instruction_string_hash,
                                      instruction_name);
  jitter_append_meta_instruction (p, mi);
}




/* Lower-level program-construction API.
 * ************************************************************************** */

void
jitter_append_instruction (struct jitter_routine *p,
                           const struct jitter_instruction *ip)
{
  if (p->stage != jitter_routine_stage_unspecialized)
    jitter_fatal ("jitter_append_instruction: non non-unspecialized program");
  if (p->expected_parameter_no != 0)
    jitter_fatal ("jitter_append_instruction: previous instruction incomplete");
  fprintf (stderr, "Pushing instruction at %p (%s)\n", ip,
           ip->meta_instruction->name);

  /* Add the provided instruction.  There is no need to touch the fields about
     expected parameters, since the previous instruction was already closed. */
  jitter_dynamic_buffer_push (& p->instructions,
                              & ip,
                              sizeof (struct jitter_instruction*));

  /* Close the new instruction.  This increments the number of rewritable
     instructions and triggers rewriting.  Notice that the instruction that was
     just added, along with some others preceding it, might very well change or
     disappear after rewriting is done. */
  jitter_close_current_instruction (p);
}

void
jitter_append_parameter_copy (struct jitter_routine *p,
                              const struct jitter_parameter *pp)
{
  /* Check that the parameter is compatbile with what we are expecting; fail
     fatally if it isn't. */
  jitter_check_paremater_compatibility (p, pp->type, pp->register_class);

  /* The next parameter is pre-allocated.  Copy the pointed one into it. */
  jitter_copy_instruction_parameter (p->next_uninitialized_parameter, pp);

  /* Advance pointers in the program past this parameter, starting a new
     instruction if needed.  This however doesn't close the instruction... */
  jitter_advance_past_next_parameter (p);

  /* ...So do it if needed. */
  jitter_close_instruction_when_no_more_parameters (p);
}




/* Jump target computation on unspecialized programs.
 * ************************************************************************** */

bool*
jitter_jump_targets (const struct jitter_routine *p)
{
  if (p->expected_parameter_no != 0)
    jitter_fatal ("computing jump targets with an instruction incomplete");

  const int instruction_no = jitter_routine_instruction_no (p);
  const struct jitter_instruction **ins
    = (const struct jitter_instruction **)
      jitter_dynamic_buffer_to_const_pointer (& p->instructions);
  bool *res = jitter_xmalloc (sizeof (bool) * instruction_no);

  /* Initialize: no instruction is a jump target by default. */
  int i;
  for (i = 0; i < instruction_no; i ++)
    res [i] = false;

  /* Scan every instruction... */
  for (i = 0; i < instruction_no; i ++)
    {
      const struct jitter_parameter **ps
        = (const struct jitter_parameter **) ins[i]->parameters;

      /* Any callee instruction is a branch target, reachable by a
         branch-and-link. */
      if (ins [i]->meta_instruction->callee)
        res [i] = true;

      /* Any instruction directly *following* a caller instruction is a branch
         target, implicitly reachable by a return. */
      if (ins [i]->meta_instruction->caller && (i + 1) < instruction_no)
        res [i + 1] = true;

      /* Look at every argument of the i-th instruction: for every one referring
         another instruction as a jump target, set the target to true. */
      const int arity = ins[i]->meta_instruction->parameter_no;
      int j;
      for (j = 0; j < arity; j ++)
        {
          const struct jitter_parameter *p = ps [j];
          if (p->type == jitter_parameter_type_label)
            {
              const long target = p->label_as_index;
              if (target < 0 || target >= instruction_no)
                {
                  printf ("# Warning: invalid label literal in instruction "
                          "at L%li\n", (long) i);
                  res [i] = true; /* Make the invalid instruction visible. */
                }
              else
                res [target] = true;
            }
        } /* inner for: parameter loop. */
    } /* outer for: instruction loop. */

  return res;
}




/* Unspecialized program printer.
 * ************************************************************************** */

/* Return the length of the longest instruction name actually used in this
   program. */
static size_t
jitter_maximum_instruction_name_length (const struct jitter_routine *p)
{
  const int instruction_no = jitter_routine_instruction_no (p);
  const struct jitter_instruction **ins
    = (const struct jitter_instruction **)
      jitter_dynamic_buffer_to_const_pointer (& p->instructions);

  /* Iterate on every instruction, and find the maximum length. */
  size_t res = 0;
  int i;
  for (i = 0; i < instruction_no; i ++)
    {
      const struct jitter_instruction * in = ins [i];
      const struct jitter_meta_instruction * mi = in->meta_instruction;
      size_t instruction_name_length = strlen (mi->name);
      if (instruction_name_length > res)
        res = instruction_name_length;
    }
  return res;
}

void
jitter_print_routine (FILE *out, const struct jitter_routine *p)
{
  const bool slow_registers_only = p->options.slow_registers_only;
  const int instruction_no = jitter_routine_instruction_no (p);
  const struct jitter_instruction **ins
    = (const struct jitter_instruction **)
      jitter_dynamic_buffer_to_const_pointer (& p->instructions);

  /* We need to keep track of which instructions are jump targets.  If we
     already have the information, which is computed at specialization time,
     then use it; otherwise compute it now. */
  bool *is_target;
  if (p->stage >= jitter_routine_stage_specialized)
    is_target = p->jump_targets;
  else
    is_target = jitter_jump_targets (p);

  /* Prepare a printf format string leaving the correct amount of space to align
     the first argument of every instruction to the same column.  The format
     string describes a single conversion specification, the instruction name as
     a string. */
  size_t max_instruction_name_length
    = jitter_maximum_instruction_name_length (p);
  char instruction_format [100];
  sprintf (instruction_format, "        %%-%us ",
           (unsigned) max_instruction_name_length);
  int i;
  for (i = 0; i < instruction_no; i ++)
    {
      const struct jitter_instruction * in = ins [i];
      const struct jitter_meta_instruction * mi = in->meta_instruction;
      const struct jitter_parameter **ps
        = (const struct jitter_parameter **) in->parameters;

      /* It is okay to use "$L" followed by an unspecialized index as a label
         name; this way we have a guarantee that all names are unique. */
      if (is_target [i])
        fprintf (out, "$L%i:\n", i);
      fprintf (out, instruction_format, mi->name);
      const int arity = mi->parameter_no;
      int j;
      for (j = 0; j < arity; j ++)
        {
          const struct jitter_parameter *p = ps [j];
          switch (p->type)
            {
            case jitter_parameter_type_uninitialized:
              fprintf (out, "<uninitialized>");
              break;
            case jitter_parameter_type_register_id:
              {
                int register_index = p->register_index;
                /* If the program has been modified to use slow registers only,
                   unmodify it for printing: the modification simply consists in
                   changing register indices so that the first one maps to the
                   first slow register for each class. */
                const struct jitter_register_class *register_class
                  = mi->parameter_types [j].register_class;
                if (slow_registers_only)
                  register_index -= register_class->fast_register_no;
                /* Print the possibly unmodified register. */
                fprintf (out, "%%%c%i", register_class->character,
                         register_index);
                break;
              }
            case jitter_parameter_type_literal:
              {
                const jitter_literal_parameter_printer printer
                  = mi->parameter_types [j].literal_printer;
                printer (out, p->literal.ufixnum);
                break;
              }
            case jitter_parameter_type_label:
              fprintf (out, "$L%li", (long) p->label_as_index);
              break;
            default:
              /* Even if in the future I remove some parameter types this
                 default case remains useful for debugging.  Don't touch it. */
              fprintf (out, "<INVALID-ARGUMENT-TYPE>");
              break;
            } /* switch */
          if (j + 1 != arity)
            fprintf (out, ", ");
        } /* inner for: parameter loop. */
      fprintf (out, "\n");
    } /* outer for: instruction loop. */

  /* If we have allocated the boolean array here then free it; if not then we
     were reusing some part of the program, and of course we should not break
     it. */
  if (p->stage < jitter_routine_stage_specialized)
    free (is_target);
}




/* Label resolution.
 * ************************************************************************** */

void
jitter_resolve_labels_in_unspecialized_routine (struct jitter_routine *pr)
{
  if (pr->stage != jitter_routine_stage_unspecialized)
    jitter_fatal ("resolving unspecialized labels in non-unspecialized routine");

  const int instruction_no = jitter_routine_instruction_no (pr);
  struct jitter_instruction **ins
    = jitter_dynamic_buffer_to_pointer (& pr->instructions);

  /* Scan instructions sequentially, and for each of them scan parameters
     sequentially, replacing opaque labels with instruction indices.  This
     relies of parameters *not* being shared: each label parameter must be
     touched exactly once. */

  /* For every instruction... */
  int i;
  for (i = 0; i < instruction_no; i ++)
    {
      const struct jitter_instruction * in = ins [i];
      const struct jitter_meta_instruction * mi = in->meta_instruction;
      struct jitter_parameter **ps = in->parameters;

      /* ...For every instruction parameter... */
      const int arity = mi->parameter_no;
      int j;
      for (j = 0; j < arity; j ++)
        {
          struct jitter_parameter *p = ps [j];
          /* ...If the parameter is a label... */
          if (p->type == jitter_parameter_type_label)
            {
              /* Replace the label with its instruction index. */
              jitter_label label = p->label;
              jitter_int label_instruction_index
                = jitter_get_label_instruction_index (pr, label);
              if (label_instruction_index == -1)
                jitter_fatal ("undefined label %li", (long) label);

              /* Notice that this assignment invalidates p->label . */
              p->label_as_index = label_instruction_index;
            }
        }
    }
}
