/* Jitter: VM-independent instruction header.

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


/* FIXME: this header doesn't need to be included out of the library
   implementation.  It should probably not be installed. */

#ifndef JITTER_INSTRUCTION_H_
#define JITTER_INSTRUCTION_H_

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include <jitter/jitter.h>
#include <jitter/jitter-hash.h>


/* Register classes.
 * ************************************************************************** */

/* A register class descriptor.  Every instance of this struct is a
   machine-generated, statically-allocated constant. */
struct jitter_register_class
{
  /* The character uniquely identifying the register class within the VM.  For
     example registers of the class with character 'r' will be called "%r0",
     "%r1", and so on. */
  char character;

  /* The register class identifier, to be cast to an enum
     vmprefix_register_class_id value. */
  jitter_uint register_class_id;

  /* How many fast registers exist for this class. */
  size_t fast_register_no;
};


/* Labels.
 * ************************************************************************** */

/* Labels are held internally as opaque identifiers, resolved to instruction
   indices late, right before a program is specialized.  The facility for
   handling a consistent mapping from symbolic to opaque labels is provided for
   convenience, but labels are used in an opaque form and, in particular, are
   stored as opaque within instruction parameters.

   Opaque labels are only valid within a single VM program.  Different programs
   may use the same opaque label identifiers, each referring to its own program
   points.

   Labels must be allocated only with the functions provided in jitter-program.h
   , for a specific program. */
typedef jitter_int jitter_opaque_label;





/* Instruction description.
 * ************************************************************************** */

// FIXME: this should be factored with the definitions in jitterc-vm.h .

enum jitter_parameter_type
  {
    /* This is used during program construction, in case we have to
       destroy a partially initialized program. */
    jitter_parameter_type_uninitialized, // FIXME: remove?

    jitter_parameter_type_register_id,
    jitter_parameter_type_literal,
    jitter_parameter_type_label
  };

/* An instruction literal. */
union jitter_literal
{
  jitter_int jitter_literal_signed;
  jitter_uint jitter_literal_unsigned;
};

/* The parameter of an unspecialized instruction.  Every instance of this struct
   is malloc-allocated, and its only pointer is within the parameter field of
   struct jitter_instruction , also always malloc-allocated and referred by
   programs.  This rigid organization makes it easy to deallocate programs with
   all their elements. */
struct jitter_parameter
{
  /* The parameter type. */
  enum jitter_parameter_type type;

  /* The parameter content.  Which value is significant depends on the type. */
  union
  {
    /* An anonymous struct containing data only meaningful for register
       parameters. */
    struct
    {
      /* The register class of a register parameter. */
      const struct jitter_register_class *register_class;

      /* The register index of a register parameter. */
      jitter_register_index register_index;
    };

    /* The parameter, as an immediate literal. */
    union jitter_literal literal;

    /* The parameter as a label. */
    jitter_opaque_label label;

    /* The parameter, as an 0-based unspecialized instruction index.  Opaque
       labels are all replaced with unspecialized instruction indices by
       jitter_resolve_labels_in_unspecialized_program . */
    jitter_label_as_index label_as_index;
  };
};

/* Forward declaration.  The actual definition of struct jitter_program is in
   jitter-program.h . */
struct jitter_program;

/* Return a pointer to a fresh instruction parameter, allocated according to
   the conventions above, with the label_name field set to NULL. */
struct jitter_parameter *
jitter_make_instruction_parameter (void)
  __attribute__ ((returns_nonnull));

/* Given a program and an already initialized unspecialized instruction
   parameter belonging to it, return a pointer to a fresh clone of the parameter
   for the same program.  The clone is malloc-allocated following the struct
   jitter_parameter conventions and shares no structure with the original.
   This is useful for rewriting. */
struct jitter_parameter*
jitter_clone_instruction_parameter (const struct jitter_program *program,
                                    const struct jitter_parameter *original)
  __attribute__ ((returns_nonnull, nonnull (1, 2)));

/* Free the given instruction parameter, which must be completely initialized
   and allocated according to the conventions above. */
void
jitter_destroy_instruction_parameter (struct jitter_parameter *p)
  __attribute__ ((nonnull (1)));




/* VM instruction parameter printing.
 * ************************************************************************** */

/* A function printing a given VM instruction literal actual parameter to the
   pointed stream.  The parameter is arbitrarily represented as an unsigned
   integer to the printer, but is meant to be cast to the appropriate type,
   which may be signed or pointer. */
typedef void (*jitter_literal_parameter_printer) (FILE *out, jitter_uint arg);

/* The default VM parameter printer, showing values in hexadecimal according to
   the C lexicon, including the "0x" prefix. */
void
jitter_default_literal_parameter_printer (FILE *out, jitter_uint arg);




/* Meta-instructions.
 * ************************************************************************** */

// FIXME: again, this should be factored with the definitions in jitterc-vm.h .

// FIXME: Currently there is a big confusion between types and kinds.  I should
// rationalize this, and rename C types accordingly.

/* The accepted kind for each meta-instruction parameter. */
// FIXME: I might want to use enum jitterc_instruction_argument_kind instead of this.
enum jitter_meta_instruction_parameter_kind
  {
    /* The parameter may only be a register id.  In practice this is
       useful for output parameters. */
    jitter_meta_instruction_parameter_kind_register,

    /* The parameter may only be a literal fixnum. */
    jitter_meta_instruction_parameter_kind_literal_fixnum,

    /* The parameter may only be a literal instruction pointer. */
    jitter_meta_instruction_parameter_kind_literal_label,

    /* The parameter may be either a register id or a literal fixnum.  This is
       a common case for input parameters. */
    jitter_meta_instruction_parameter_kind_register_or_literal_fixnum,

    /* The parameter is a either a literal label or the id of a register
       containing an instruction pointer. */
    jitter_meta_instruction_parameter_kind_register_or_literal_label,

    /* The parameter is one of
       (a) the id of a register containing an instruction pointer;
       (b) a literal fixnum;
       (c) a literal label. */
    jitter_meta_instruction_parameter_kind_register_or_literal_fixnum_or_literal_label
};

/* The type of parameter expected in a certain position for a certain
   meta-instruction. */
struct jitter_meta_instruction_parameter_type
{
  /* The expected parameter kind. */
  enum jitter_meta_instruction_parameter_kind kind;

  /* The expected register class, only meaningful if the kind includes a
     register; it will be NULL otherwise. */
  const struct jitter_register_class *register_class;

  /* A non-NULL pointer to the printer for this parameter. */
  const jitter_literal_parameter_printer literal_printer;
};

/* The descriptor of one meta-instruction.  Every instance of this struct is
   statically allocated at startup, and accessible via
   vmprefix_lookup_meta_instruction or by indexing vmprefix_meta_instructions with an
   enum vmprefix_meta_instruction_id . */
struct jitter_meta_instruction
{
  /* The unique id of this meta-instruction.  This is actually VM-dependent, but
     we can use a generic integer type instead of enum
     vmprefix_meta_instruction_id .  The generic type is wide enough to
     accommodate the id for any VM, and ids are non-negative. */
  jitter_uint id;

  /* The name is only for debugging, error messages and human-readable program output. */
  const char *name;

  /* How many parameter this instruction has. */
  size_t parameter_no;

  /* True if and only if the instruction is a caller. */
  bool caller;

  /* True if and only if the instruction is a callee. */
  bool callee;

  /* True if and only if the instruction is relocatable. */
  bool relocatable;

  /* The type of each parameter, in order.  The pointer refers a statically
     allocated buffer. */
  const struct jitter_meta_instruction_parameter_type *parameter_types;
};

/* Initialize the meta-instruction hash table. */
void
jitter_initialize_meta_instructions
   (struct jitter_hash_table *meta_instruction_string_hash,
    const struct jitter_meta_instruction *meta_instructions,
    size_t meta_instruction_no);

/* Finalize the meta-instruction hash table.  No need to finalize the array,
   which is a global constant. */
void
jitter_finalize_meta_instructions (struct jitter_hash_table *
                                   meta_instruction_string_hash);

/* Given a pointer to the meta-instruction hash table and a meta-instruction
   name return a pointer to the meta-instruction statically-allocated
   descriptor.  Undefined behavior if no such meta-instruction exists. */
const struct jitter_meta_instruction*
jitter_lookup_meta_instruction (const struct jitter_hash_table *mi_hash,
                                  const char *name);




/* Instruction representation in an unspecialized program.
 * ************************************************************************** */

/* A VM unspecialized instruction.  This representation is inefficient, but
   much more flexible for rewriting. */
struct jitter_instruction
{
  /* The pointed data structure is some global data structure which is
     initialized at startup and never freed.  Every vmprefix_instruction of
     the same type points to the same meta_instruction. */
  const struct jitter_meta_instruction *meta_instruction;

  /* This points to a malloc-allocated buffer, not shared. */
  struct jitter_parameter **parameters;
};

/* Return a pointer to a fresh instruction, using the given meta-instruction.
   This allocates the parameter array, (unless the meta-instruction has zero
   parameters, in which case the parameter array is set to NULL) with the
   appropriate size according to the meta-instruction, and allocates all
   parameters, leaving them undefined but with NULL label_name fields. */
struct jitter_instruction *
jitter_make_instruction (const struct jitter_meta_instruction * const mi)
  __attribute__ ((returns_nonnull, nonnull (1)));

/* Destroy the given instruction, which must have been allocated with malloc
   following the conventions above, its parameters field if non-NULL and in that
   case each parameter as well. */
void
jitter_destroy_instruction (struct jitter_instruction *i)
  __attribute__ ((nonnull (1)));


#endif // #ifndef JITTER_INSTRUCTION_H_
