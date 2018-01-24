/* Jittery Lisp: interpreter: naïve C version.

   Copyright (C) 2018 Luca Saiu
   Written by Luca Saiu

   This file is part of the Jittery Lisp language implementation, distributed as
   an example along with Jitter under the same license.

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


#include "jitterlisp-eval-vm.h"

#include "jitterlisp.h"
#include "jitterlispvm-vm.h"




/* Jittery evaluation engine.
 * ************************************************************************** */

// FIXME: remove this section.

/* Error out saying that the VM is not yet implemented. */
__attribute__ ((noreturn))
static void
jitterlisp_vm_unimplemented (void)
{
  jitterlisp_error_cloned ("Jittery VM not implemented yet: plase run "
                           "with --no-vm or explicitly use the AST "
                           "interpreter");
}

jitterlisp_object
jitterlisp_eval_globally_vm (jitterlisp_object form)
{
  jitterlisp_vm_unimplemented ();
}

jitterlisp_object
jitterlisp_eval_vm (jitterlisp_object form, jitterlisp_object env)
{
  jitterlisp_vm_unimplemented ();
}

jitterlisp_object
jitterlisp_apply_vm (jitterlisp_object closure_value,
                     jitterlisp_object operands_as_list)
{
  jitterlisp_vm_unimplemented ();
}




/* VM initialization and finalization.
 * ************************************************************************** */

struct jitterlispvm_state
jitterlispvm_state;

void
jitterlisp_reset_vm_state (void)
{
  /* Finalize and then re-initialize the VM state. */
  jitterlispvm_state_finalize (& jitterlispvm_state);
  jitterlispvm_state_initialize (& jitterlispvm_state);
}

void
jitterlisp_initialize_vm (void)
{
  /* Call the Jitter-generated function initializing the VM subsystem. */
  jitterlispvm_initialize ();

  /* Initialize the global VM state. */
  jitterlispvm_state_initialize (& jitterlispvm_state);
}

void
jitterlisp_finalize_vm (void)
{
  /* Finalize the global VM state. */
  jitterlispvm_state_finalize (& jitterlispvm_state);

  /* Call the Jitter-generated function finalizing the VM subsystem. */
  jitterlispvm_initialize ();
}




/* Call into VM code.
 * ************************************************************************** */

jitterlisp_object
jitterlisp_call_compiled (const struct jitterlisp_compiled_closure *cc,
                          const jitterlisp_object *operand_values,
                          size_t operand_value_no)
{
  jitterlisp_error_cloned ("jitterlisp_call_compiled: unimplemented");
}
