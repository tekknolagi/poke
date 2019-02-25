/* Jitter: VM-specific configuration and internal implementation header.

   Copyright (C) 2017, 2018 Luca Saiu
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


#ifndef JITTER_VM_H_
#define JITTER_VM_H_

#include <stdio.h>

#include <jitter/jitter.h>
#include <jitter/jitter-program.h>
#include <jitter/jitter-patch-in.h>




/* VM-specific attributes.
 * ************************************************************************** */

/* A struct containing the configuration-specific parameters of a VM. */
struct jitter_vm_configuration
{
  /* Identifier prefixes for the generated C code. */
  char *lower_case_prefix, *upper_case_prefix;

  /* How many fast registers per class this VM can have, as a maximum.  -1 means
     that there is no limit. */
  int max_fast_register_no_per_class;

  /* How many nonresidual literals we support, as a maximum; -1 means that there
     is no limit. */
  int max_nonresidual_literal_no;

  /* A textual description of the dispatching technique. */
  char *dispatch;
};

/* Print the current VM configuration, as set by jitterc and CPP macros, to the
   given stream in a human-readable format. */
void
jitter_print_vm_configuration (FILE *f,
                               const struct jitter_vm_configuration *c);




/* VM run-time settings.
 * ************************************************************************** */

/* These global configuration functions must be called before adding VM
   instructions, or never called at all. */

/* Enable optimization rewriting rules for the given VM.  Optimization rewriting
   is enabled by default. */
void
jitter_vm_enable_optimization_rewriting (struct jitter_vm *vm);

/* Disable optimization rewriting rules for the given VM.  This is mostly useful
   for debugging and benchmarking. */
void
jitter_vm_disable_optimization_rewriting (struct jitter_vm *vm);




/* VM internal implementation.
 * ************************************************************************** */

/* Everything from this point on is subject to change and not meant for the
   user. */

/* A struct defining the VM-specific attributes of a VM.  Each VM has its own
   unique instance of this, shared by every program for the same VM and
   initialized by vmprefix_initialize in template code.
   This structure is used internally, and the user does not need to see it. */
struct jitter_vm
{
  /* Configuration-specific data for this VM. */
  struct jitter_vm_configuration configuration;

/* Threads or pointers to native code blocks of course don't exist with
   switch-dispatching. */
#ifndef JITTER_DISPATCH_SWITCH
  /* True iff threads appear to be valid: of non-negative size, sequential,
     non-overlapping. */
  bool threads_validated;

  // FIXME: add a comment per field.
  jitter_thread *threads;
  long *thread_sizes;
#endif // #ifndef JITTER_DISPATCH_SWITCH

  const size_t *specialized_instruction_residual_arities;
  const unsigned long *specialized_instruction_label_bitmasks;

  /* This is NULL when using a dispatching model not needing the bitmask. */
  const unsigned long *specialized_instruction_fast_label_bitmasks;

#ifdef JITTER_HAVE_PATCH_IN
  const struct jitter_patch_in_descriptor *patch_in_descriptors;
  size_t patch_in_descriptor_no;
  /* A patch-in table as defined in jitter/jitter-patch-in.h . */
  struct patch_in_table_entry *patch_in_table;
#endif // #ifdef JITTER_HAVE_PATCH_IN

  const bool *specialized_instruction_relocatables;
  const bool *specialized_instruction_callers;
  const bool *specialized_instruction_callees;
  const char * const *specialized_instruction_names;
  size_t specialized_instruction_no;

  struct jitter_hash_table *meta_instruction_string_hash;

  struct jitter_meta_instruction *meta_instructions;
  size_t meta_instruction_no;

  /* Specific meta-instruction pointers for implicit instructions.
     VM-independent program specialization relies on those, so they have to be
     accessible to the Jitter library, out of generated code*/
  const struct jitter_meta_instruction *exitvm_meta_instruction;
  const struct jitter_meta_instruction *unreachable_meta_instruction;

  /* The longest unspecialized/meta instruction name length, not mangled,
     without counting the final '\0' character.  Special specialized
     instruction, having no unspecialized counterparts, are ignored here. */
  size_t max_meta_instruction_name_length;

  /* A function returning a pointer to a constant register class descriptor
     given the register class character, or NULL if the character is not
     associated to any register class. */
  const struct jitter_register_class *
  (* register_class_character_to_register_class) (char c);

  /* Translate one or more unspecialized instructions starting from *ins into
     p->specialized_program by calling the appropriate
     vmprefix_add_specialized_instruction_* functions for the opcode and every
     argument, returning the number of unspecialized instructions covered by the
     one new specialized instruction which is being added.  The result is always
     1 or more -- more than 1 when a superinstruction is being recognized.  The
     actual function is machine-generated. */
  int (*specialize_instruction) (struct jitter_program *p,
                                 const struct jitter_instruction *ins);

  /* Rewrite an instruction.  This points to the actual non-nop function
     vmprefix_rewrite declared in templates/vm.h , but is not directly called.
     The rewrite field below is used from the outside, and rewrite may either be
     equal to this field when rewriting is enabled, or point to
     jitter_dont_rewrite when rewriting is disabled. */
  void (*actually_rewrite) (struct jitter_program *p);

  /* Rewrite an instruction or do nothing.  See the comment for
     actually_rewrite, above. */
  void (*rewrite) (struct jitter_program *p);
};

/* A function doing nothing, usable as a value for the rewrite field of struct
   jitter_vm , defined above. */
void
jitter_dont_rewrite (struct jitter_program *p);


#endif // #ifndef JITTER_VM_H_
