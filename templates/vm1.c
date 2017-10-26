/* VM library: main VM C file template.

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


/* Generated file warning.
 * ************************************************************************** */

/* Unless this file is named exactly "vm1.c" , without any prefix, you are
   looking at a machine-generated derived file.  The original source is the vm.c
   template from Jitter, with added code implementing the vmprefix VM. */




#include <assert.h>
#include <string.h>

#include <jitter/jitter.h>
#include <jitter/jitter-dispatch.h>
#include <jitter/jitter-hash.h>
#include <jitter/jitter-instruction.h>
#include <jitter/jitter-program.h>
#include <jitter/jitter-parser.h>
#include <jitter/jitter-specialize.h>
#include <jitter/jitter-patch-in.h>

#include "vmprefix-vm.h"
//#include "vmprefix-specialized-instructions.h"
//#include "vmprefix-meta-instructions.h"
#include <jitter/jitter-fatal.h>


/* Machine-generated data structures.
 * ************************************************************************** */

/* Machine-generated data structures defining this VM.  Initializing a static
   struct is problematic, as it requires constant expressions for each field --
   and const pointers don't qualify.  This is why we initialize the struct
   fields below in vmprefix_initialize. */
static struct jitter_vm
the_vmprefix_vm;

const struct jitter_vm * const
vmprefix_vm = (const struct jitter_vm * const) & the_vmprefix_vm;

const struct jitter_vm_configuration * const
vmprefix_vm_configuration = & the_vmprefix_vm.configuration;




/* Initialization and finalization: internal functions, not for the user.
 * ************************************************************************** */

/* Initialize threads.  This only needs to be called once at initialization, and
   the user doesn't need to bother with it.  Defined along with the interpreter. */
void
vmprefix_initialize_threads (void);

/* Check that the encodings in enum jitter_specialized_instruction_opcode (as
   used in the specializer) are coherent with machine-generated code.  Making a
   mistake here would introduce subtle bugs, so it's better to be defensive. */
static void
vmprefix_check_specialized_instruction_opcode_once (void)
{
  static bool already_checked = false;
  if (already_checked)
    return;

  assert (vmprefix_specialized_instruction_opcode__eINVALID == 0);
  assert (vmprefix_specialized_instruction_opcode__eBEGINBASICBLOCK == 1);
  assert (vmprefix_specialized_instruction_opcode__eEXITVM == 2);
  assert (vmprefix_specialized_instruction_opcode__eUNREACHABLE0 == 3);
  assert (vmprefix_specialized_instruction_opcode__eUNREACHABLE1 == 4);
  assert (vmprefix_specialized_instruction_opcode__eUNREACHABLE2 == 5);

  already_checked = true;
}

/* A prototype for a machine-generated function not needing a public
   declaration, only called thru a pointer within struct jitter_vm . */
int
vmprefix_specialize_instruction (struct jitter_program *p,
                                 const struct jitter_instruction *ins);

/* Forward-declaration.  The implementation of this is machine-generated, and
   occurs further down in this file. */
static void
vmprefix_initialize_vm_configuration (struct jitter_vm_configuration *c);




/* Check that we link with the correct Jitter library.
 * ************************************************************************** */

/* It is possible to make a mistake at link time, and link a VM compiled with
   some threading model with the Jitter runtime for a different model.  That
   would cause crashes, that is better to prevent.  This is a way to detect such
   mistakes very early, by causing a link-time failure in case of mismatch. */
extern volatile const bool
JITTER_DISPATCH_DEPENDENT_GLOBAL_NAME;




/* Initialization and finalization.
 * ************************************************************************** */

#ifdef JITTER_HAVE_PATCH_IN
JITTER_PATCH_IN_DESCRIPTOR_DECLARATIONS(vmprefix)
#endif // #ifdef JITTER_HAVE_PATCH_IN

/* True iff thread sizes are all non-negative and non-huge.  We refuse to
   disassemble otherwise, and when replication is enabled we refuse to run
   altogether.  See the comment right below. */
static bool
vmprefix_thread_sizes_validated = false;

/* Check that VM instruction sizes are all non-negative; if even one is negative
   that is a symptom that the code has not been compiled -fno-reorder-blocks ,
   which would have disastrous effects with replication.  It's better to validate
   sizes at startup and fail immediately than to crash at run time.

   If even one size appears to be wrong, refuse to disassemble when replication
   is disabled, and refuse to run altogether if replication is enabled. */
static void
vmprefix_validate_thread_sizes_once (void)
{
  /* Return if this is not the first time we got here. */
  static bool already_validated = false;
  if (already_validated)
    return;

  /* Check every instruction size. */
  int i;
  bool everything_valid = true;
  for (i = 0; i < VMPREFIX_SPECIALIZED_INSTRUCTION_NO; i ++)
    {
      long size = vmprefix_thread_sizes [i];
      if (__builtin_expect ((size < 0 || size > (1 << 24)),
                            false))
        {
#ifdef JITTER_REPLICATE
          fprintf (stderr,
                   "About specialized instruction %i (%s), with size %li\n",
                   i, vmprefix_specialized_instruction_names [i], size);
          jitter_fatal ("a specialized instruction has negative or huge code "
                        "size: you are not compiling with -fno-reorder-blocks");
#else
          everything_valid = false;
          break;
#endif // #ifdef JITTER_REPLICATE
        }
    }

  /* If we have validated every thread size then disassembling appears safe. */
  if (everything_valid)
    vmprefix_thread_sizes_validated = true;

  /* We have checked the thread sizes, once and for all.  If this function gets
     called again, thru a second vmprefix initialization, it will immediately
     return. */
  already_validated = true;
}

void
vmprefix_initialize (void)
{
  /* Check that the Jitter library we linked is the right one.  This check
     actually only useful to force the global to be used.  I prefer not to use
     an assert, because assertions can be disabled. */
  if (! JITTER_DISPATCH_DEPENDENT_GLOBAL_NAME)
    jitter_fatal ("impossible to reach: the thing should fail at link time");

  /* Perform some sanity checks which only need to be run once. */
  vmprefix_check_specialized_instruction_opcode_once ();

  /* We have to initialize threads before vmprefix_threads , since the struct
     needs threads. */
  vmprefix_initialize_threads ();

  /* Validate thread sizes, to make sure they are all non-negative.  This only
     needs to be done once. */
  vmprefix_validate_thread_sizes_once ();

  /* Initialize the object pointed by vmprefix_vm (see the comment above as to
     why we do it here).  Before actually setting the fields to valid data, fill
     the whole struct with a -- hopefully -- invalid pattern, just to catch
     bugs. */
  static bool vm_struct_initialized = false;
  if (! vm_struct_initialized)
    {
      memset (& the_vmprefix_vm, 0xff, sizeof (struct jitter_vm));

      /* The global pointer vmprefix_vm_configuration points within the_vmprefix_vm ,
         so its data structure has just been invalidated as well. */
      vmprefix_initialize_vm_configuration (& the_vmprefix_vm.configuration);
      //vmprefix_print_vm_configuration (stdout, & the_vmprefix_vm.configuration);

      the_vmprefix_vm.threads = (jitter_thread *)vmprefix_threads;
      the_vmprefix_vm.thread_sizes = (long *) vmprefix_thread_sizes;
      the_vmprefix_vm.thread_sizes_validated = vmprefix_thread_sizes_validated;

      the_vmprefix_vm.specialized_instruction_residual_arities
        = vmprefix_specialized_instruction_residual_arities;
      the_vmprefix_vm.specialized_instruction_label_bitmasks
        = vmprefix_specialized_instruction_label_bitmasks;
#ifdef JITTER_HAVE_PATCH_IN
      the_vmprefix_vm.specialized_instruction_fast_label_bitmasks
        = vmprefix_specialized_instruction_fast_label_bitmasks;
      the_vmprefix_vm.patch_in_descriptors =
        JITTER_PATCH_IN_DESCRIPTORS_NAME(vmprefix);
      the_vmprefix_vm.patch_in_descriptor_no =
        JITTER_PATCH_IN_DESCRIPTOR_NO_NAME(vmprefix);
#else
      the_vmprefix_vm.specialized_instruction_fast_label_bitmasks = NULL;
#endif // #ifdef JITTER_HAVE_PATCH_IN

      /* FIXME: I might want to conditionalize this. */
      the_vmprefix_vm.specialized_instruction_relocatables
        = vmprefix_specialized_instruction_relocatables;

      the_vmprefix_vm.specialized_instruction_callers
        = vmprefix_specialized_instruction_callers;
      the_vmprefix_vm.specialized_instruction_callees
        = vmprefix_specialized_instruction_callees;

      the_vmprefix_vm.specialized_instruction_names
        = vmprefix_specialized_instruction_names;
      the_vmprefix_vm.specialized_instruction_no
        = VMPREFIX_SPECIALIZED_INSTRUCTION_NO;

      the_vmprefix_vm.meta_instruction_string_hash
        = & vmprefix_meta_instruction_hash;
      the_vmprefix_vm.meta_instructions
        = (struct jitter_meta_instruction *) vmprefix_meta_instructions;
      the_vmprefix_vm.meta_instruction_no = VMPREFIX_META_INSTRUCTION_NO;
      the_vmprefix_vm.max_meta_instruction_name_length
        = VMPREFIX_MAX_META_INSTRUCTION_NAME_LENGTH;
      the_vmprefix_vm.register_class_character_to_register_class
        = vmprefix_register_class_character_to_register_class;
      the_vmprefix_vm.specialize_instruction = vmprefix_specialize_instruction;
      the_vmprefix_vm.rewrite_instruction = vmprefix_rewrite_instruction;

      vm_struct_initialized = true;
    }

  jitter_initialize_meta_instructions (& vmprefix_meta_instruction_hash,
                                         vmprefix_meta_instructions,
                                         VMPREFIX_META_INSTRUCTION_NO);
}

void
vmprefix_finalize (void)
{
  /* There's no need to touch the_vmprefix_vm ; we can keep it as it is, as it
     contains no dynamically-allocated fields. */
  /* Threads need no finalization. */
  jitter_finalize_meta_instructions (& vmprefix_meta_instruction_hash);
}




/* Program initialization.
 * ************************************************************************** */

struct jitter_program*
vmprefix_make_program (void)
{
  return jitter_make_program (vmprefix_vm);
}




/* Array re-allocation.
 * ************************************************************************** */

volatile union vmprefix_any_register *
vmprefix_make_place_for_slow_registers (struct vmprefix_state *s,
                                        size_t slow_register_no_per_class)
{
  /* Change nothing if we already have enough space for the required number of
     slow registers.  The no-change case will be the most common one, and
     this function might be worth optimizing. */
  if (__builtin_expect (  slow_register_no_per_class
                        > s->vmprefix_state_backing
                             .jitter_slow_register_no_per_class,
                        false))
    {
      /* Save the new value for slow_register_no_per_class in the state
         structure; reallocate the Array. */
      s->vmprefix_state_backing.jitter_slow_register_no_per_class
        = slow_register_no_per_class;
      s->vmprefix_state_backing.jitter_array
        = (jitter_xrealloc ((void *) s->vmprefix_state_backing.jitter_array,
                            VMPREFIX_ARRAY_SIZE(slow_register_no_per_class)));
    }

  /* Return the new (or unchanged) base, by simply adding the bias to the
     Array as it is now. */
  volatile char *res
    = (((volatile char*) s->vmprefix_state_backing.jitter_array)
       + JITTER_BIAS);
  return (volatile union vmprefix_any_register *) res;
}




/* Program text frontend.
 * ************************************************************************** */

struct jitter_program*
vmprefix_parse_file_star (FILE *input_file)
{
  return jitter_parse_file_star (input_file, vmprefix_vm);
}

struct jitter_program*
vmprefix_parse_file (const char *input_file_name)
{
  return jitter_parse_file (input_file_name, vmprefix_vm);
}

struct jitter_program*
vmprefix_parse_file_star_possibly_with_slow_registers_only (FILE *input_file,
                                                            bool slow_only)
{
  return jitter_parse_file_star_possibly_with_slow_registers_only (input_file,
                                                                   vmprefix_vm,
                                                                   slow_only);
}

struct jitter_program*
vmprefix_parse_file_possibly_with_slow_registers_only (const char *input_file_name,
                                                       bool slow_only)
{
  return jitter_parse_file_possibly_with_slow_registers_only (input_file_name,
                                                              vmprefix_vm,
                                                              slow_only);
}




/* Evrything following this point is machine-generated.
 * ************************************************************************** */

/* What follows could be conceptually split into several generated C files, but
   having too many of them would be inconvenient for the user to compile and
   link.  For this reason we currently generate just three files: one is this,
   another is for the specializer and the last one for the interpreter.  The
   interpreter will be potentially very large, so it is best compiled
   separately.  The specializer might be large as well at this stage, even if
   its compilation is usually much less expensive. */
