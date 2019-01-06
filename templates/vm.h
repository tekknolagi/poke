/* VM library: main header file.

   Copyright (C) 2016, 2017, 2018 Luca Saiu
   Updated in 2019 by Luca Saiu
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

/* Unless this file is named exactly "vm.h" , without any prefix, you are
   looking at a machine-generated derived file.  The original source is the vm.h
   template from Jitter, with added code implementing the vmprefix VM. */




/* This multiple-inclusion guard is opened here in the template, and will be
   closed at the end of the generated code.  It is normal to find no matching
   #endif in the template file.  */
#ifndef VMPREFIX_VM_H_
#define VMPREFIX_VM_H_


/* This is the main VM header to use from hand-written code.
 * ************************************************************************** */

#include <stdio.h>
#include <stdbool.h>

#include <jitter/jitter.h>
#include <jitter/jitter-dispatch.h>
#include <jitter/jitter-hash.h>
#include <jitter/jitter-stack.h>
#include <jitter/jitter-instruction.h>
#include <jitter/jitter-program.h>
#include <jitter/jitter-specialize.h> // FIXME: what about only declaring jitter_specialize in another header, and not including this?
#include <jitter/jitter-disassemble.h>
#include <jitter/jitter-vm.h>




/* Initialization and finalization.
 * ************************************************************************** */

/* Initialize the runtime state for the vmprefix VM.  This needs to be called
   before using VM programs or VM states in any way. */
void
vmprefix_initialize (void);

/* Finalize the runtime state, freeing some resources.  After calling this no
   use of VM programs or states is allowed.  It is possible to re-initialize
   after finalizing; these later re-initializations might be more efficient than
   the first initialization. */
void
vmprefix_finalize (void);




/* Global VM settings.
 * ************************************************************************** */

/* Enable optimization rewriting for the vmprefix VM.  Optimization rewriting is
   on by default, so this is only useful to call after
   vmprefix_disable_optimization_rewriting . */
void
vmprefix_enable_optimization_rewriting (void);

/* Disable optimization rewriting for the vmprefix VM.  Optimization rewriting is
   on by default and almost always desired, but this may be useful for debugging
   or benchmarking. */
void
vmprefix_disable_optimization_rewriting (void);

/* Do not automatically append an "exitvm" instruction at the end of each VM
   program, adding an "unreachable" instruction instead. */
void
vmprefix_disable_final_exitvm (void);

/* Automatically append an "exitvm" instruction at the end of each VM program.
   This is the default. */
void
vmprefix_enable_final_exitvm (void);




/* State data structure initialization and finalization.
 * ************************************************************************** */

/* The machine state is separated into the backing and the more compact runtime
   data structures, to be allocated in registers as far as possible.  These are
   just a forward-declarations: the actual definitions are machine-generated. */
struct vmprefix_state_backing;
struct vmprefix_state_runtime;

/* A data structure containing both the backing and the runtime state.  This is
   a forward-declaration: the actual definition will come after both are
   defined. */
struct vmprefix_state;

/* Initialize the pointed VM state data structure, or fail fatally.  The
   function definition is machine-generated, even if it may include user code.
   The state backing and runtime are initialized at the same time, and in fact
   the distinction between them is invisible to the VM user. */
void
vmprefix_state_initialize (struct vmprefix_state *state)
  __attribute__ ((nonnull (1)));

/* Finalize the pointed VM state data structure, or fail fatally.  The function
   definition is machine-generated, even if it may include user code.  The state
   backing and runtime are finalized at the same time. */
void
vmprefix_state_finalize (struct vmprefix_state *state)
  __attribute__ ((nonnull (1)));




/* Program initialization.
 * ************************************************************************** */

/* Return a freshly-allocated empty program for the vmprefix VM. */
struct jitter_program*
vmprefix_make_program (void)
  __attribute__ ((returns_nonnull));

/* Program finalization is actually VM-independent, but a definition of
   vmprefix_destroy_program is provided below as a macro, for cosmetic
   reasons. */


/* Code-generation C API.
 * ************************************************************************** */

/* This is the preferred way of adding a new VM instruction to a pointed
   program, more efficient than vmprefix_append_instruction_name even if only
   usable when the VM instruction opcode is known at compile time.  The
   unspecialized instruction name must be explicitly mangled by the user as per
   the rules in jitterc_mangle.c .  For example an instruction named foo_bar can
   be added to the program pointed by p with any one of
     vmprefix_append_instruction_name (p, "foo_bar");
   ,
     VMPREFIX_APPEND_INSTRUCTION(p, foo_ubar);
   , and
     VMPREFIX_APPEND_INSTRUCTION_ID(p, vmprefix_meta_instruction_id_foo_ubar);
   .
   The string "foo_bar" is not mangled, but the token foo_ubar is. */
#define VMPREFIX_APPEND_INSTRUCTION(program_p, instruction_mangled_name_root)  \
  do                                                                           \
    {                                                                          \
      jitter_append_meta_instruction                                           \
         ((program_p),                                                         \
          vmprefix_meta_instructions                                           \
          + JITTER_CONCATENATE_TWO(vmprefix_meta_instruction_id_,              \
                                   instruction_mangled_name_root));            \
    }                                                                          \
  while (false)

/* Append the unspecialized instruction whose id is given to the pointed program.
   The id must be a case of enum vmprefix_meta_instruction_id ; such cases have
   a name starting with vmprefix_meta_instruction_id_ .
   This is slightly less convenient to use than VMPREFIX_APPEND_INSTRUCTION
   but more general, as the instruction id is allowed to be a non-constant C
   expression. */
#define VMPREFIX_APPEND_INSTRUCTION_ID(_jitter_program_p,          \
                                       _jitter_instruction_id)     \
  do                                                               \
    {                                                              \
      jitter_append_instruction_id ((_jitter_program_p),           \
                                    vmprefix_meta_instructions,    \
                                    VMPREFIX_META_INSTRUCTION_NO,  \
                                    (_jitter_instruction_id));     \
    }                                                              \
  while (false)

/* This is the preferred way of appending a register argument to the instruction
   being added to the pointed program, more convenient than directly using
   vmprefix_append_register_id_parameter , even if only usable when the register
   class is known at compile time.  Here the register class is only provided as
   a letter, but both the program pointer and the register index are arbitrary C
   expressions.
   For example, in
     VMPREFIX_APPEND_REGISTER_PARAMETER(p, r, variable_to_index (x));
   the second macro argument "r" represents the register class named "r", and
   not the value of a variable named r. */
#define VMPREFIX_APPEND_REGISTER_PARAMETER(program_p, class_letter, index)  \
  do                                                                        \
    {                                                                       \
      vmprefix_append_register_parameter                                    \
         ((program_p),                                                      \
          & JITTER_CONCATENATE_TWO(vmprefix_register_class_,                \
                                   class_letter),                           \
          (index));                                                         \
    }                                                                       \
  while (false)




/* Interpretation.
 * ************************************************************************** */

/* Interpret the given program, which must be already specialized, in the given
   state. */
void
vmprefix_interpret (struct jitter_program const *p, struct vmprefix_state *s)
  __attribute__ ((nonnull (1, 2)));




/* Array element access: residuals, transfers, slow registers, more to come.
 * ************************************************************************** */

/* In order to cover a wider range of addresses with simple base+register
   addressing the base does not necessarily point to the beginning of the Array;
   instead the base points to the beginning of the Array plus VMPREFIX_BIAS
   bytes.
   FIXME: define the bias as a value appropriate to each architecture.  I think
   I should just move the definition to jitter-machine.h and provide a default
   here, in case the definition is missing for some architectures. */
#define JITTER_BIAS 0

/* Transfer registers are not implemented yet.  For the purpose of computing
   Array offsets I will say they are zero. */
#define VMPREFIX_TRANSFER_REGISTER_NO 0

/* Array-based globals are not implemented yet.  For the purpose of computing
   Array offsets I will say they are zero. */
#define VMPREFIX_GLOBAL_NO 0

#ifdef JITTER_DISPATCH_NO_THREADING
/* Expand to the offset of the i-th residual from the base, in bytes.  This is
   not useful with any of the other dispatching modes, where residuals directly
   follow each VM instruction opcode or thread.
   For good performance i should always be a compile-time constant, as it is
   in machine-generated code.

   FIXME: right now there is a separate base for residuals and for slow
   registers, but this will change. */
# define VMPREFIX_RESIDUAL_UNBIASED_OFFSET(i)  \
    (sizeof (union vmprefix_any_register) * (i))
# define VMPREFIX_RESIDUAL_OFFSET(i)  \
    (VMPREFIX_RESIDUAL_UNBIASED_OFFSET(i) - JITTER_BIAS)
#endif // #ifdef JITTER_DISPATCH_NO_THREADING

/* Define a macro holding the first slow register offset in bytes from an
   initial Array pointer Array */
#ifdef JITTER_DISPATCH_NO_THREADING
  /* With no-threading dispatch we have to keep into account residuals and
     transfer registers, which come before slow registers.  [FIXME: and more in
     the future].  This relies on UNINSPIRED_MAX_RESIDUAL_ARITY , defined below
     in machine-generated code. */
# define VMPREFIX_FIRST_SLOW_REGISTER_UNBIASED_OFFSET  \
  (sizeof (union vmprefix_any_register)                \
   * (  VMPREFIX_GLOBAL_NO                             \
      + VMPREFIX_MAX_RESIDUAL_ARITY                    \
      + VMPREFIX_TRANSFER_REGISTER_NO))
#else
  /* With any dispatching model different from no-threading the Array begins
     with slow registers. */
# define VMPREFIX_FIRST_SLOW_REGISTER_UNBIASED_OFFSET 0
#endif // #ifdef JITTER_DISPATCH_NO_THREADING

/* Expand to the offset of the i-th register of class c in bytes from the Array
   beginning.
   The c argument must be a literal C (one-character) identifier.
   The i argument should always be a compile-time constant for performance, and
   it is in generated code.
   The i-th c-class register must be slow, otherwise the offset will be
   incorrect -- in fact fast registers are, hopefully, not in memory at all.

   Slow registers come in the Array ordered first by index, then by class.  For
   example if there are three classes "r" with 4 fast registers, "f" with 7 fast
   registers and "q" with 2 fast registers, slow registers can be accessed in
   this order:
     r4, f7, q2, r5, r8, q3, r6, r9, q4, and so on.
   This organization is convenient since changing the number of slow registers
   doesn't invalidate any offset computed in the past.

   This relies on macro such as VMPREFIX_REGISTER_CLASS_NO and
   VMPREFIX_REGISTER_?_FAST_REGISTER_NO and , defined below in machine-generated
   code. */
#define VMPREFIX_SLOW_REGISTER_UNBIASED_OFFSET(c, i)                     \
  (VMPREFIX_FIRST_SLOW_REGISTER_UNBIASED_OFFSET                          \
   + (sizeof (union vmprefix_any_register)                               \
      * (VMPREFIX_REGISTER_CLASS_NO                                      \
         * ((i) - JITTER_CONCATENATE_THREE(VMPREFIX_REGISTER_, c,        \
                                           _FAST_REGISTER_NO))           \
         + JITTER_CONCATENATE_THREE(VMPREFIX_REGISTER_, c, _CLASS_ID))))

/* Expand to the offset of the i-th register of class c in bytes from the base,
   keeping the bias into account. */
#define VMPREFIX_SLOW_REGISTER_OFFSET(c, i)                              \
  (VMPREFIX_SLOW_REGISTER_UNBIASED_OFFSET(c, i) - JITTER_BIAS)

/* Expand to the Array size in bytes, assuming the given number of slow
   registers per class.  This is an allocation size, ignoring the bias. */
#define VMPREFIX_ARRAY_SIZE(slow_register_per_class_no)                  \
  (VMPREFIX_FIRST_SLOW_REGISTER_UNBIASED_OFFSET                          \
   + (sizeof (union vmprefix_any_register)                               \
      * VMPREFIX_REGISTER_CLASS_NO                                       \
      * (slow_register_per_class_no)))




/* Program text frontend.
 * ************************************************************************** */

/* Parse from the given file, returning a program or failing fatally in case of
   error.  These are simple wrappers around functions implemented in the Bison
   file. */
struct jitter_program*
vmprefix_parse_file_star (FILE *input_file)
  __attribute__ ((nonnull (1), returns_nonnull));
struct jitter_program*
vmprefix_parse_file (const char *input_file_name)
  __attribute__ ((nonnull (1), returns_nonnull));
struct jitter_program*
vmprefix_parse_file_star_possibly_with_slow_registers_only (FILE *input_file,
                                                            bool slow_only)
  __attribute__ ((nonnull (1), returns_nonnull));
struct jitter_program*
vmprefix_parse_file_possibly_with_slow_registers_only (const char *input_file_name,
                                                       bool slow_only)
  __attribute__ ((nonnull (1), returns_nonnull));




/* Machine-generated data structures.
 * ************************************************************************** */

/* Declare a few machine-generated data structures, which together define a VM. */

/* Threads or pointers to native code blocks of course don't exist with
   switch-dispatching. */
#ifndef JITTER_DISPATCH_SWITCH
/* Every possible thread, indexed by enum jitter_specialized_instruction_opcode .
   This is used at specialization time, and the user shouldn't need to touch
   it. */
extern const jitter_thread *
vmprefix_threads;

/* VM instruction end label.  These are not all reachable at run time, but
   having them in a global array might prevent older GCCs from being too clever
   in reordering blocks. */
extern const jitter_thread *
vmprefix_thread_ends;

/* The size, in chars, of each thread's native code.  The elements are in the
   same order of vmprefix_threads.  Sizes could conceptually be of type size_t ,
   but in order to be defensive I'm storing pointer differences as signed
   values, so that we may catch compilation problems: if any VM instruction end
   *precedes* its VM instruction beginning, then the compiler has reordered
   labels, which would have disastrous effects with replicated code. */
extern const long *
vmprefix_thread_sizes;
#endif // #ifndef JITTER_DISPATCH_SWITCH

/* This is defined in the machine-generated vm/meta-instructions.c . */
extern struct jitter_hash_table
vmprefix_meta_instruction_hash;

/* An array specifying every existing meta-instruction, defined in the order of
   enum vmprefix_meta_instruction_id .  This is defined in vm/meta-instructions.c ,
   which is machine-generated. */
extern const struct jitter_meta_instruction
vmprefix_meta_instructions [];

/* How many residual parameters each specialized instruction has.  The
   actual array definition is machine-generated. */
extern const size_t
vmprefix_specialized_instruction_residual_arities [];

/* An array of bitmasks, one per specialized instruction.  Each bitmask holds
   one bit per residual argument, counting from the least significant (the first
   residual arg maps to element & (1 << 0), the second to element & (1 << 1),
   and so on).
   Each bit is 1 if and only if the corresponding residual argument is a label
   or a fast label.
   Only residual arguments are counted: for example a specialized instruction
   foo_n1_lR_r2 would have a mask with the *first* bit set. */
extern const unsigned long // FIXME: possibly use a shorter type when possible
vmprefix_specialized_instruction_label_bitmasks [];

/* Like vmprefix_specialized_instruction_label_bitmasks , but for fast labels
   only.
   The actual definition is conditionalized so as to appear only when
   needed according to the dispatching model. */
extern const unsigned long // FIXME: possibly use a shorter type when possible
vmprefix_specialized_instruction_fast_label_bitmasks [];

/* An array of booleans in which each element is true iff the specialized
   instruction whose opcode is the index is relocatable. */
extern const bool
vmprefix_specialized_instruction_relocatables [];

/* An array of booleans in which each element is true iff the specialized
   instruction whose opcode is the index is a caller. */
extern const bool
vmprefix_specialized_instruction_callers [];

/* An array of booleans in which each element is true iff the specialized
   instruction whose opcode is the index is a callee. */
extern const bool
vmprefix_specialized_instruction_callees [];

/* This big array of strings contains the name of each specialized instruction,
   in the order of enum vmprefix_specialized_instruction_opcode . */
extern const char* const
vmprefix_specialized_instruction_names [];


/* A pointer to a struct containing const pointers to the structures above, plus
   sizes; there will be only one instance of this per VM, machine-generated.
   Each program data structure contains a pointer to that instance, so that
   VM-independent functions, given a program, will have everything needed to
   work.  The one instance of struct jitter_vm for the vmprefix VM. */
extern const struct jitter_vm * const
vmprefix_vm;

/* A pointer to a struct containing VM-specific parameters set in part when
   calling jitterc and in part when compiling the generated C code, such as the
   dispatching model and the number of fast registers.  The data is fully
   initialized only after a call to vmprefix_initialize . */
extern const
struct jitter_vm_configuration * const
vmprefix_vm_configuration;




/* Compatibility macros.
 * ************************************************************************** */

/* It is convenient, for future extensibility, to expose an interface in which
   some VM-independent functions and data structures actually look as if they
   were specific to the user VM. */

/* What the user refers to as struct vmprefix_program is actually a struct
   jitter_program , whose definition is VM-independent. */
#define vmprefix_program jitter_program

/* Destroy programs (program initialization is actually VM-specific). */
#define vmprefix_destroy_program jitter_destroy_program

/* Program construction API. */
#define vmprefix_append_instruction_name \
  jitter_append_instruction_name
#define vmprefix_append_meta_instruction \
  jitter_append_meta_instruction
#define vmprefix_append_label \
  jitter_append_label
#define vmprefix_append_symbolic_label \
  jitter_append_symbolic_label
#define vmprefix_append_register_parameter \
  jitter_append_register_parameter
#define vmprefix_append_literal_parameter \
  jitter_append_literal_parameter
#define vmprefix_append_signed_literal_parameter \
  jitter_append_signed_literal_parameter
#define vmprefix_append_unsigned_literal_parameter \
  jitter_append_unsigned_literal_parameter
#define vmprefix_append_pointer_literal_parameter \
  jitter_append_pointer_literal_parameter
#define vmprefix_append_label_parameter \
  jitter_append_label_parameter
#define vmprefix_append_symbolic_label_parameter \
  jitter_append_symbolic_label_parameter
#define vmprefix_label \
  jitter_label
#define vmprefix_fresh_label \
  jitter_fresh_label
#define vmprefix_print_program \
  jitter_print_program
#define vmprefix_print_program_possibly_with_slow_registers_only \
  jitter_print_program_possibly_with_slow_registers_only
#define vmprefix_specialize_program \
  jitter_specialize_program
#define vmprefix_disassemble_program \
  jitter_disassemble_program
#define vmprefix_disassemble_program_to \
  jitter_disassemble_program_to
#define vmprefix_print_vm_configuration \
  jitter_print_vm_configuration




/* Register class types.
 * ************************************************************************** */

/* Return a pointer to a statically allocated register class descriptor, given
   the register class character, or NULL if the character does not represent a
   valid register class.

   A constant array indexed by a character would have been more efficient, but
   relying on character ordering is not portable, at least in theory.  A
   non-constant array could be initialized in a portable way, but that would
   probably not be worth the trouble. */
const struct jitter_register_class *
vmprefix_register_class_character_to_register_class (char c)
  __attribute__ ((pure));


/* A constant array of constant pointers to every existing register class
   descriptor, ordered by class id; each pointer within the array refers the
   only existing class descriptor for its class.  The number of elements is
   VMPREFIX_REGISTER_CLASS_NO , but that is not declared because the definition
   of VMPREFIX_REGISTER_CLASS_NO comes later in generated code.

   This is useful when the user code enumerates every existing register class,
   particularly for debugging. */
extern const struct jitter_register_class * const
vmprefix_regiter_classes [];




/* Array re-allocation.
 * ************************************************************************** */

/* Make the Array in the pointed state large enough to accommodate the given
   number of slow reigsters per class, adjusting the Array pointer as needed
   and recording information about the new size in the state; change nothing
   if the array is already large enough.  Return the new base.
   For example passing 3 as the value of slow_register_no would make
   place for three slow registers per register class: if the current VM had two
   classes 'r' and 'f' than the function would ensure that the Array can hold
   three 'r' and three 'f' slow registers, independently from the number
   of fast 'r' or 'f' registers.
   Any new elements allocated in the Array are left uninitialized, but its old
   content remains valid. */
volatile union vmprefix_any_register *
vmprefix_make_place_for_slow_registers (struct vmprefix_state *s,
                                        size_t slow_register_no_per_class)
  __attribute__ ((noinline));




/* **************************************************************************
 * Evrything following this point is for internal use only.
 * ************************************************************************** */




/* Defect tables.
 * ************************************************************************** */

/* It is harmless to declare these unconditionally, even if they only used when
   patch-ins are available.  See jitter/jitter-defect.h .*/

/* The worst-case defect table.  This is a global constant array, having one
   element per specialized instruction. */
extern const jitter_uint
vmprefix_worst_case_defect_table [];

/* The actual defect table, to be filled at initialization time. */
extern jitter_uint
vmprefix_defect_table [];




/* Instruction rewriter.
 * ************************************************************************** */

/* Try to apply each rewrite rule in order and run the first one that matches,
   if any, on the pointed program.  When a rule fires the following ones are not
   checked but if a rule, after removing the last few instructions, adds another
   one, the addition will trigger another rewrite in its turn, and so on until
   no more rewriting is possible.  The rewriting process is inherently
   recursive.

   The implementation of this function is machine-generated, but the user can
   add her own code in the rewriter-c block, which ends up near the beginning of
   this function body, right after JITTTER_REWRITE_FUNCTION_PROLOG_ .  The
   formal argument seen from the body is named jitter_program_p .

   Rationale: the argument is named differently in the body in order to keep
   the namespace conventions and, more importantly, to encourage the user to
   read this comment.

   The user must *not* append labels to the VM programs during rewriting: that
   would break it.  The user is responsible for destroying any instruction she
   removes, including their arguments.  The user can assume that
   jitter_rewritable_instruction_no is strictly greater than zero. */
void
vmprefix_rewrite (struct jitter_program *p);




/* Program points at runtime in specialized programs.
 * ************************************************************************** */

/* The type of a program point at run time in a specialized program.  This is
   the type of object than can be passed to JITTER_BRANCH , and its actual
   definition depends on the dispatching model.  Notice that however, in every
   case, a program point is a pointer-to-constant type and therefore fits in a
   word. */
#ifdef JITTER_DISPATCH_NO_THREADING
  /* With no-threading dispatch a program point is the address of a machine
     instruction -- from C, it's what a goto * statement accepts.  I don't
     need to worry about non-GCC compilers, since no-threading relies on GCC
     extensions. */
  typedef void *
  vmprefix_program_point;
#else
  /* On every other dispatching model a program point is a pointer to a word
     in the specialized program -- in the case of switch dispatching that word
     will contain a specialized opcode, with threading it will contain the
     address of a machine instruction (see the case above) followed by the
     VM instruction arguments. */
  typedef const union jitter_word *
  vmprefix_program_point;
#endif // #ifdef JITTER_DISPATCH_NO_THREADING

/* Expand to an expression evaluating to the program point of the first
   instruction in the pointed vmprefix VM program, as some object which is
   correct to pass to JITTER_BRANCH.  The expression type will be
   vmprefix_program_point.

   This is useful to execute a program without explicitly calling the
   interpreter function, for example from a VM instruction jumping to the
   beginning of another VM program.  The expansion of this macro is guaranteed
   not to contain function calls, and is safe to use from VM instructions
   without C function wrappers.

   This assumes, without checking, that the program is already specialized. */
#ifdef JITTER_DISPATCH_NO_THREADING
  /* In this case the program structure contains a separate pointer to the
     beginning of the executable region for the native program.  Of course
     the first instruction is at the beginning of the region. */
# define VMPREFIX_PROGRAM_BEGINNING(_jitterlisp_program_pointer)  \
    ((_jitterlisp_program_pointer)->native_code)
#else
  /* With switch dispatching or threading the first program point is a pointer
     to the beginning of the specialized program array. */
# define VMPREFIX_PROGRAM_BEGINNING(_jitterlisp_program_pointer)   \
    ((vmprefix_program_point)                                      \
     ((_jitterlisp_program_pointer)->specialized_program.region))
#endif // ifdef JITTER_DISPATCH_NO_THREADING




/* Machine-generated code.
 * ************************************************************************** */

/* What follows could be conceptually split into several generated header files,
   but having too many files would be inconvenient for the user to compile and
   link.  For this reason we generate a single header. */

