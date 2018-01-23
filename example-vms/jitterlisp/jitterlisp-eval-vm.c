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




/* Code generator: C part.
 * ************************************************************************** */

/* FIXME: don't leak on compilation errors, and don't break the closure
   provided by the user. */
void
jitterlisp_compilation_error (char *message)
{
  printf ("compile-time failure\n");
  jitterlisp_error_cloned (message);
}

/* FIXME: handle GC roots. */

/* The following can safely assume that code_as_sexpression is a list, since
   that is checked by the compilation primitive, but nothing more about its
   structure. */

/* Return an upper bound on the number of labels needed in the code.  This
   actually returns the successor of the maximum label identifier, which is
   adequate as long as label identifiers are not too sparse -- which they
   are not. */
static jitter_uint
jitterlisp_label_no (jitterlisp_object code_as_sexpression)
{
  /* Labels as generated by the Lisp code generator are natural numbers, so
     we can use -1 to represent the maximum of an empty set.  */
  jitter_int max = -1;

  /* Scan the instruction list, looking for (label INDEX)
     pseudo-instructions. */
  jitterlisp_object rest;
  for (rest = code_as_sexpression;
       rest != JITTERLISP_EMPTY_LIST;
       rest = JITTERLISP_EXP_C_A_CDR(rest))
    {
      jitterlisp_object element = JITTERLISP_EXP_C_A_CAR(rest);
      printf ("* element: ");
      jitterlisp_print_to_stream (stdout, element);
      printf ("\n");
      if (jitterlisp_is_list_of_length (element, 2)
          && JITTERLISP_EXP_C_A_CAR(element) == jitterlisp_label)
        {
          jitterlisp_object cadr;
          if (! JITTERLISP_IS_FIXNUM(cadr = jitterlisp_cadr (element)))
            jitterlisp_compilation_error ("label followed by non-fixnum");
          jitter_int this_label = JITTERLISP_FIXNUM_DECODE(cadr);
          if (this_label < 0)
            jitterlisp_compilation_error ("negative label");
          if (this_label > max)
            max = this_label;
        }
    }
  return max + 1;
}

/* Given the Lisp encoding of the VM program, generate the Jittery version. */
static struct jitterlispvm_program *
jitterlisp_generate_jittery (jitterlisp_object code_as_sexpression)
{
  struct jitterlispvm_program *res
    = jitterlispvm_make_program ();

  jitter_uint label_no = jitterlisp_label_no (code_as_sexpression);
  jitterlispvm_label *labels // FIXME: don't leak on error.
    = jitter_xmalloc (sizeof (jitterlispvm_label) * label_no);
  int i;
  for (i = 0; i < label_no; i ++)
    labels [i] = jitterlispvm_fresh_label (res);
  printf ("Labels are %i\n", (int) label_no);

  JITTERLISPVM_APPEND_INSTRUCTION(res, drop);
  JITTERLISPVM_APPEND_INSTRUCTION(res, nip);
  JITTERLISPVM_APPEND_INSTRUCTION(res, push_munspecified);
  jitterlispvm_append_label (res, labels [0]);
  JITTERLISPVM_APPEND_INSTRUCTION(res, push_mliteral);
  jitterlispvm_append_unsigned_literal_parameter (res, JITTERLISP_FIXNUM_ENCODE(42));
  JITTERLISPVM_APPEND_INSTRUCTION(res, push_mnil);
  JITTERLISPVM_APPEND_INSTRUCTION(res, push_mzero);
  JITTERLISPVM_APPEND_INSTRUCTION(res, push_mone);
  JITTERLISPVM_APPEND_INSTRUCTION(res, push_mfalse);
  JITTERLISPVM_APPEND_INSTRUCTION(res, push_mnothing);
  JITTERLISPVM_APPEND_INSTRUCTION(res, branch);
  jitterlispvm_append_label_parameter (res, labels [0]);
  JITTERLISPVM_APPEND_INSTRUCTION(res, dup);
  JITTERLISPVM_APPEND_INSTRUCTION(res, branch_mif_mfalse);
  jitterlispvm_append_label_parameter (res, labels [0]);
  JITTERLISPVM_APPEND_INSTRUCTION(res, branch_mif_mtrue);
  jitterlispvm_append_label_parameter (res, labels [0]);

  JITTERLISPVM_APPEND_INSTRUCTION(res, push_mregister);
  JITTERLISPVM_APPEND_REGISTER_PARAMETER (res, r, 0);
  JITTERLISPVM_APPEND_INSTRUCTION(res, pop_mto_mregister);
  JITTERLISPVM_APPEND_REGISTER_PARAMETER (res, r, 0);


  JITTERLISPVM_APPEND_INSTRUCTION(res, copy_mto_mregister);
  JITTERLISPVM_APPEND_REGISTER_PARAMETER (res, r, 0);
  JITTERLISPVM_APPEND_INSTRUCTION(res, copy_mto_mregister);
  JITTERLISPVM_APPEND_REGISTER_PARAMETER (res, r, 1);
  JITTERLISPVM_APPEND_INSTRUCTION(res, copy_mto_mregister);
  JITTERLISPVM_APPEND_REGISTER_PARAMETER (res, r, 2);
  JITTERLISPVM_APPEND_INSTRUCTION(res, copy_mfrom_mregister);
  JITTERLISPVM_APPEND_REGISTER_PARAMETER (res, r, 0);

  JITTERLISPVM_APPEND_INSTRUCTION(res, primitive);
  jitterlispvm_append_pointer_literal_parameter (res, 0);
  jitterlispvm_append_unsigned_literal_parameter (res, 0);

  JITTERLISPVM_APPEND_INSTRUCTION(res, primitive);
  jitterlispvm_append_pointer_literal_parameter (res, 0);
  jitterlispvm_append_unsigned_literal_parameter (res, 1);

  JITTERLISPVM_APPEND_INSTRUCTION(res, primitive);
  jitterlispvm_append_pointer_literal_parameter (res, 0);
  jitterlispvm_append_unsigned_literal_parameter (res, 2);

  JITTERLISPVM_APPEND_INSTRUCTION(res, plus);

  JITTERLISPVM_APPEND_INSTRUCTION(res, one_mplus);
  JITTERLISPVM_APPEND_INSTRUCTION(res, one_mminus);

  JITTERLISPVM_APPEND_INSTRUCTION(res, car);
  JITTERLISPVM_APPEND_INSTRUCTION(res, cdr);
  JITTERLISPVM_APPEND_INSTRUCTION(res, cadddr);

  JITTERLISPVM_APPEND_INSTRUCTION(res, nullp);
  JITTERLISPVM_APPEND_INSTRUCTION(res, nothingp);
  JITTERLISPVM_APPEND_INSTRUCTION(res, fixnump);
  JITTERLISPVM_APPEND_INSTRUCTION(res, characterp);
  JITTERLISPVM_APPEND_INSTRUCTION(res, consp);
  JITTERLISPVM_APPEND_INSTRUCTION(res, non_mconsp);

  JITTERLISPVM_APPEND_INSTRUCTION(res, branch_mif_mnull);
  jitterlispvm_append_label_parameter (res, labels [0]);

  JITTERLISPVM_APPEND_INSTRUCTION(res, canonicalize_mboolean);

  jitterlispvm_specialize_program (res);
  jitterlispvm_print_program (stdout, res);
  printf ("\n");
  jitterlispvm_disassemble_program (res, true, JITTER_CROSS_OBJDUMP, NULL);
  free (labels);
  return res;
}

void
jitterlisp_compile (struct jitterlisp_closure *c,
                    jitter_int in_arity,
                    jitterlisp_object nonlocals,
                    jitterlisp_object code_as_sexpression)
{
  /* Error out if the Jittery VM is disabled. */
  if (! jitterlisp_settings.vm)
    jitterlisp_error_cloned ("can't compile: Jittery VM disabled");

  c->kind = jitterlisp_closure_type_compiled;
  struct jitterlisp_compiled_closure * const cc = & c->compiled;
  cc->in_arity = in_arity;
  cc->nonlocals = nonlocals;
  // FIXME: don't leak.
  cc->code = jitterlisp_generate_jittery (code_as_sexpression);
}
