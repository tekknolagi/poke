/* Jittery structured language example: stack-based code generator.

   Copyright (C) 2017, 2018, 2019 Luca Saiu
   Written by Luca Saiu

   This file is part of the Jitter structured-language example, distributed
   along with Jitter under the same license.

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
#include <jitter/jitter-dynamic-buffer.h>
#include <jitter/jitter-fatal.h>

#include "structuredvm-vm.h"
#include "structured-syntax.h"
#include "structured-code-generator.h"


/* Code generation by recursion over an AST.
 * ************************************************************************** */

/* Add code to translate the pointed expression AST to the pointed Jittery
   program, using the pointed static environment to be looked up and updated.
   We need one forward-declaration here, as structured_translate_expression and
   structured_translate_primitive are mutually recursive. */
static void
structured_translate_expression (struct structuredvm_program *vmp,
                                 struct structured_expression *e,
                                 struct structured_static_environment *env);

/* Add code to translate a primitive-case expression AST to the pointed Jittery
   program, using the pointed static environment to be looked up and updated.
   The primitive is given by its case and its two operands, of which the second
   is ignored if the primitive is unary. */
static void
structured_translate_primitive (struct structuredvm_program *vmp,
                                enum structured_primitive case_,
                                struct structured_expression *operand_0,
                                struct structured_expression *operand_1,
                                struct structured_static_environment *env)
{
  /* Emit code to evaluate the first operand for unary primitives, or code to
     evaluate the first and then second operand for binary primitives. */
  switch (case_)
    {
    case structured_primitive_unary_minus:
    case structured_primitive_logical_not:
      structured_translate_expression (vmp, operand_0, env);
      break;
    default: /* The other primitives are binary. */
      structured_translate_expression (vmp, operand_0, env);
      structured_translate_expression (vmp, operand_1, env);
    }

  /* Emit one VM instruction actually implementing the primitive, working over
     its two actual arguments already evaluated in the top one or two stack
     elements. */
  switch (case_)
    {
    case structured_primitive_plus:
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, plus); break;
    case structured_primitive_minus:
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, minus); break;
    case structured_primitive_times:
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, times); break;
    case structured_primitive_divided:
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, divided); break;
    case structured_primitive_remainder:
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, remainder); break;
    case structured_primitive_unary_minus:
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, uminus); break;
    case structured_primitive_equal:
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, equal); break;
    case structured_primitive_different:
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, different); break;
    case structured_primitive_less:
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, less); break;
    case structured_primitive_less_or_equal:
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, lessorequal); break;
    case structured_primitive_greater:
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, greater); break;
    case structured_primitive_greater_or_equal:
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, greaterorequal); break;
    case structured_primitive_logical_not:
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, logicalnot); break;
    }
}

static void
structured_translate_expression (struct structuredvm_program *vmp,
                                 struct structured_expression *e,
                                 struct structured_static_environment *env)
{
  switch (e->case_)
    {
    case structured_expression_case_literal:
      {
        STRUCTUREDVM_APPEND_INSTRUCTION(vmp, push);
        structuredvm_append_signed_literal_parameter (vmp, e->literal);
        break;
      }
    case structured_expression_case_variable:
      {
        structured_register_index idx
          = structured_static_environment_lookup (env, e->variable);
        STRUCTUREDVM_APPEND_INSTRUCTION(vmp, push);
        STRUCTUREDVM_APPEND_REGISTER_PARAMETER(vmp, r, idx);
        break;
      }
    case structured_expression_case_if_then_else:
      {
        structuredvm_label before_else = structuredvm_fresh_label (vmp);
        structuredvm_label after_else = structuredvm_fresh_label (vmp);
        structured_translate_expression (vmp, e->if_then_else_condition, env);
        STRUCTUREDVM_APPEND_INSTRUCTION(vmp, bf);
        structuredvm_append_label_parameter (vmp, before_else);
        structured_translate_expression (vmp, e->if_then_else_then_branch, env);
        STRUCTUREDVM_APPEND_INSTRUCTION(vmp, b);
        structuredvm_append_label_parameter (vmp, after_else);
        structuredvm_append_label (vmp, before_else);
        structured_translate_expression (vmp, e->if_then_else_else_branch, env);
        structuredvm_append_label (vmp, after_else);
        break;
      }
    case structured_expression_case_primitive:
      {
        structured_translate_primitive (vmp, e->primitive,
                                        e->primitive_operand_0,
                                        e->primitive_operand_1, env);
        break;
      }
    default:
      jitter_fatal ("invalid expression case (bug): %i", (int) e->case_);
    }
}

/* Add code to translate the pointed statement AST to the pointed Jittery
   program, using the pointed static environment to be looked up and updated. */
static void
structured_translate_statement (struct structuredvm_program *vmp,
                                struct structured_statement *s,
                                struct structured_static_environment *env)
{
  switch (s->case_)
    {
    case structured_statement_case_skip:
      {
        break;
      }
    case structured_statement_case_block:
      {
        structured_static_environment_bind (env, s->block_variable);
        structured_translate_statement (vmp, s->block_body, env);
        structured_static_environment_unbind (env);
        break;
      }
    case structured_statement_case_assignment:
      {
        structured_register_index idx
          = structured_static_environment_lookup (env, s->assignment_variable);
        structured_translate_expression (vmp, s->assignment_expression, env);
        STRUCTUREDVM_APPEND_INSTRUCTION(vmp, pop);
        STRUCTUREDVM_APPEND_REGISTER_PARAMETER(vmp, r, idx);
        break;
      }
    case structured_statement_case_print:
      {
        structured_translate_expression (vmp, s->print_expression, env);
        STRUCTUREDVM_APPEND_INSTRUCTION(vmp, print);
        break;
      }
    case structured_statement_case_sequence:
      {
        structured_translate_statement (vmp, s->sequence_statement_0, env);
        structured_translate_statement (vmp, s->sequence_statement_1, env);
        break;
      }
    case structured_statement_case_if_then_else:
      {
        structuredvm_label before_else = structuredvm_fresh_label (vmp);
        structuredvm_label after_else = structuredvm_fresh_label (vmp);
        structured_translate_expression (vmp, s->if_then_else_condition, env);
        STRUCTUREDVM_APPEND_INSTRUCTION(vmp, bf);
        structuredvm_append_label_parameter (vmp, before_else);
        structured_translate_statement (vmp, s->if_then_else_then_branch, env);
        STRUCTUREDVM_APPEND_INSTRUCTION(vmp, b);
        structuredvm_append_label_parameter (vmp, after_else);
        structuredvm_append_label (vmp, before_else);
        structured_translate_statement (vmp, s->if_then_else_else_branch, env);
        structuredvm_append_label (vmp, after_else);
        break;
      }
    case structured_statement_case_repeat_until:
      {
        structuredvm_label before_body = structuredvm_fresh_label (vmp);
        structuredvm_append_label (vmp, before_body);
        structured_translate_statement (vmp, s->repeat_until_body, env);
        structured_translate_expression (vmp, s->repeat_until_guard, env);
        STRUCTUREDVM_APPEND_INSTRUCTION(vmp, bf);
        structuredvm_append_label_parameter (vmp, before_body);
        break;
      }
    default:
      jitter_fatal ("invalid statement case (bug): %i", (int) s->case_);
    }
}

/* Add code to translate the pointed program AST to the pointed Jittery
   program. */
static void
structured_translate_program (struct structuredvm_program *vmp,
                              struct structured_program *p)
{
  struct structured_static_environment *env
    = structured_static_environment_make ();
  structured_translate_statement (vmp, p->main_statement, env);
  structured_static_environment_destroy (env);
}




/* Entry point: translate an AST program to a VM program.
 * ************************************************************************** */

struct structuredvm_program *
structured_make_vm_program_stack (struct structured_program *p)
{
  /* Make an empty Jittery program. */
  struct structuredvm_program *vmp = structuredvm_make_program ();

  /* Translate the AST pointed by p into *vmp.  This of course works by
     recursion. */
  structured_translate_program (vmp, p);

  // FIXME: this is a test
  //STRUCTUREDVM_APPEND_INSTRUCTION(vmp, push_mvariable);
  //structuredvm_append_unsigned_literal_parameter (vmp, 0);
  //structuredvm_append_unsigned_literal_parameter (vmp, 1);

  /* We're done. */
  return vmp;
}
