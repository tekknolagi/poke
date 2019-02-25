/* JitterLisp: interpreter: naïve C version.

   Copyright (C) 2018 Luca Saiu
   Updated in 2019 by Luca Saiu
   Written by Luca Saiu

   This file is part of the JitterLisp language implementation, distributed as
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

/* The one global VM state. */
struct jitterlispvm_state
jitterlispvm_state;

/* The driver program, used to call compiled closures from the interpreter.
   Jumping from interpreted to compiled code is common and must be fast: we
   keep one driver program, to be reused. */
static struct jitterlispvm_program *
jitterlisp_driver_vm_program;

void
jitterlisp_reset_vm_state (void)
{
  /* Finalize and then re-initialize the VM state. */
  jitterlispvm_state_finalize (& jitterlispvm_state);
  jitterlispvm_state_initialize (& jitterlispvm_state);
}

void
jitterlisp_vm_initialize (void)
{
  /* Call the Jitter-generated function initializing the VM subsystem. */
  jitterlispvm_initialize ();

  /* Initialize the global VM state. */
  jitterlispvm_state_initialize (& jitterlispvm_state);

  /* Register the two stack backings as GC roots. */
  struct jitterlispvm_state_backing *sb
    = & jitterlispvm_state.jitterlispvm_state_backing;
  jitterlisp_push_stack_backing_as_gc_root
     (& sb->jitter_stack_mainstack_backing);
  jitterlisp_push_stack_backing_as_gc_root
     (& sb->jitter_stack_returnstack_backing);

  /* Make the driver program and keep it ready to be use, pre-specialized.
     The driver program consists of exactly two instructions:
        call-from-interpreter
        exitvm
     , of which the second is added implicitly. */
  jitterlisp_driver_vm_program = jitterlispvm_make_program ();
  /* In the particular case of the driver I can keep the default option
     generating a final exitvm instruction at the end.  This will not be
     the same for compiled user code. */
//JITTERLISPVM_APPEND_INSTRUCTION(jitterlisp_driver_vm_program, debug);
  JITTERLISPVM_APPEND_INSTRUCTION(jitterlisp_driver_vm_program,
                                  call_mfrom_mc);
//JITTERLISPVM_APPEND_INSTRUCTION(jitterlisp_driver_vm_program, debug);
//JITTERLISPVM_APPEND_INSTRUCTION(jitterlisp_driver_vm_program, debug);
  jitterlispvm_specialize_program (jitterlisp_driver_vm_program);
}

void
jitterlisp_vm_finalize (void)
{
  /* Destroy the driver program and invalidate its pointer to catch mistakes. */
  jitterlispvm_destroy_program (jitterlisp_driver_vm_program);
  jitterlisp_driver_vm_program = NULL;

  /* Unregister the two stack backings as GC roots. */
  jitterlisp_pop_gc_roots (2);

  /* Finalize the global VM state. */
  jitterlispvm_state_finalize (& jitterlispvm_state);

  /* Call the Jitter-generated function finalizing the VM subsystem. */
  jitterlispvm_finalize ();
}




/* Call into VM code.
 * ************************************************************************** */

/* FIXME: is there a sensible way of making the interpreter->vm interaction a
   tail call? */

/* Jump to the first program point of the driver on the current VM state, whose
   main stack must contain a compiled closure, its evaluated actuals and the
   (unencoded) number of actuals.  When the VM is done pop the result off the
   stack and return it.
   This is the trailing part of both jitterlisp_call_compiled and
   jitterlisp_apply_compiled. */
static inline jitterlisp_object
jitterlisp_jump_to_driver_and_return_result (void)
{
  /*
  static bool d = false;
  if (! d)
    {
      d = true;
      fprintf (stderr, "Driver:\n");
      if (jitterlisp_settings.cross_disassembler)
        jitterlispvm_disassemble_program_to (stderr, jitterlisp_driver_vm_program, true, JITTER_CROSS_OBJDUMP, NULL);
      else
        jitterlispvm_disassemble_program_to (stderr, jitterlisp_driver_vm_program, true, JITTER_OBJDUMP, NULL);
      fprintf (stderr, "\n");
    }
  */
  /*
printf ("E: %p\n", (void*)JITTERLISPVM_TOP_MAINSTACK());
printf ("R: %p\n", (void*)JITTERLISPVM_UNDER_TOP_MAINSTACK());
fputs ("{c", stdout); fflush (stdout);
  */
  /* Run the driver which will immediately call the closure, leaving only
     the result on the stack. */
  jitterlispvm_interpret (jitterlisp_driver_vm_program, & jitterlispvm_state);
  /*
fputs ("} --> ", stdout); fflush (stdout);
  */

  /* Pop the result off the stack and return it. */
  jitterlisp_object res = JITTERLISPVM_TOP_MAINSTACK();
  /*
printf ("%p", (void*) res); fflush (stdout);
fputs ("\n", stdout); fflush (stdout);
  */
  JITTERLISPVM_DROP_MAINSTACK();
  return res;
}

jitterlisp_object
jitterlisp_call_compiled (jitterlisp_object rator_value,
                          const jitterlisp_object *rand_asts,
                          jitter_uint rand_ast_no,
                          jitterlisp_object env)
{
  /* Here I can be sure that operator_value is a compiled closure. */
  const struct jitterlisp_closure *c = JITTERLISP_CLOSURE_DECODE(rator_value);
  /*
printf ("The encoded rator is %p\n", (void*)rator_value);
printf ("The decoded rator is %p\n", c);
printf ("The decoded cc is at %p\n", cc);
printf ("The cc program is at %p\n", cc->vm_program);
printf ("The cc first program point is at %p\n", cc->first_program_point);
  */
  // FIXME: shall I check the arity *before* evaluating actuals or after?
  // Here I do it before, but it's easy to change.
  // In either case compiled code must have the same semantics.  Do whatever
  // is faster on compiled code.  There are two other identical cases below.

  /* Check that the arity matches. */
  if (__builtin_expect ((c->in_arity != rand_ast_no), false))
    jitterlisp_error_cloned ("arity mismatch in call to compiled closure");

  /* Push the compiled closure, tagged, on the VM stack. */
  JITTERLISPVM_PUSH_MAINSTACK(rator_value);

  // FIXME: handle errors without leaving observable changes on the stack:
  // that doesn't need to be done here: error recovery code in the REPL should
  // simply reset the state.

  /* Evaluate actuals and push them on the VM stack. */
  int i;
  for (i = 0; i < rand_ast_no; i ++)
    JITTERLISPVM_PUSH_MAINSTACK(jitterlisp_eval_interpreter_ast (rand_asts [i],
                                                                 env));

  /* Push the number of closure operands plus one, unencoded.  The driver
     programs expects this last operand on the top of the stack, to be able to
     find the closure below. */
  JITTERLISPVM_PUSH_MAINSTACK(rand_ast_no + 1);
  /*
printf ("Q: %p\n", (void*)JITTERLISPVM_TOP_MAINSTACK());
printf ("W: %p\n", (void*)JITTERLISPVM_UNDER_TOP_MAINSTACK());
  */
  /* Pass control to VM code. */
  return jitterlisp_jump_to_driver_and_return_result ();
}

jitterlisp_object
jitterlisp_apply_compiled (jitterlisp_object rator_value,
                           jitterlisp_object rand_value_list)
{
  // FIXME: handle errors without leaving observable changes on the stack:
  // that doesn't need to be done here: error recovery code in the REPL should
  // simply reset the state.

  /* Here I can be sure that operator_value is a compiled closure. */
  const struct jitterlisp_closure *c = JITTERLISP_CLOSURE_DECODE(rator_value);

  /* Push the compiled closure on the VM stack. */
  JITTERLISPVM_PUSH_MAINSTACK(rator_value);

  /* Push the actuals on the VM stack.  No need to check that the arguments
     actually come in a list. */
  jitterlisp_object rest = rand_value_list;
  jitter_uint provided_in_arity = 0;
  while (! JITTERLISP_IS_EMPTY_LIST(rest))
    {
      JITTERLISPVM_PUSH_MAINSTACK(JITTERLISP_EXP_C_A_CAR(rest));
      rest = JITTERLISP_EXP_C_A_CDR(rest);
      provided_in_arity ++;
    }

  /* Check that the arity matches. */
  if (__builtin_expect ((c->in_arity != provided_in_arity), false))
    jitterlisp_error_cloned ("arity mismatch in apply to compiled closure");

  /* Push the number of closure operands, unencoded.  The driver programs
     expects this last operand on the top of the stack, to be able to find
     the closure below. */
  JITTERLISPVM_PUSH_MAINSTACK(provided_in_arity);

  /* Pass control to VM code. */
  return jitterlisp_jump_to_driver_and_return_result ();
}
