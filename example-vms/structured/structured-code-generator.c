/* Jittery structured language example: code generator implementation.

   Copyright (C) 2017 Luca Saiu
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


/* Environment data structure for variable lookup.
 * ************************************************************************** */

typedef unsigned structured_register_index;

/* Each instance of this structure holds a <variable, register-index> pair. */
struct structured_variable_register_pair
{
  /* A variable name.  The pointed string is shared with the AST, and doesn't
     need to be freed here. */
  structured_variable variable;

  /* The 0-based index used to keep the variable.  This will be used as the
     index for an r-class register. */
  structured_register_index register_index;
};

/* A static environment structure contains a variable-to-register-index
   mapping.  Rationale: using a hash would have required less code and
   might even have been faster, but this idea is much easier to extend
   to nested scopes. */
struct structured_static_environment
{
  /* A dynamic array of struct structured_variable_register_pair elements. */
  struct jitter_dynamic_buffer pairs;

  /* Index of the lowest register index to be used next.  Indices are allocated
     sequentially, so the first variable will be assigned to 0, the second to 1
     and so on. */
  structured_register_index next_register_index;
};

/* Initialize the pointed static-environment struct. */
static void
structured_static_environment_initialize (struct structured_static_environment *e)
{
  jitter_dynamic_buffer_initialize (& e->pairs);
  e->next_register_index = 0;
}

/* Finalize the pointed static-environment struct, without freeing the struct
   itself. */
static void
structured_static_environment_finalize (struct structured_static_environment *e)
{
  jitter_dynamic_buffer_finalize (& e->pairs);
}

/* Return the register-index associated to the given variable in the pointed
   environment; if no binding for the variable exists, add one. */
static structured_register_index
structured_static_environment_lookup (struct structured_static_environment *e,
                                      const structured_variable v)
{
  struct structured_variable_register_pair *p
    = ((struct structured_variable_register_pair *)
       (e->pairs.region
        + e->pairs.used_size
        - sizeof (struct structured_variable_register_pair)));
  while (p >= (struct structured_variable_register_pair *) e->pairs.region)
    {
      if (! strcmp (p->variable, v))
        return p->register_index;
      p --;
    }
  struct structured_variable_register_pair *new_pair
    = jitter_dynamic_buffer_reserve
         (& e->pairs, sizeof (struct structured_variable_register_pair));
  new_pair->variable = v;
  return new_pair->register_index = e->next_register_index ++;
}




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
    case structured_primitive_logical_and:
      // FIXME: this is currently strict, differently from Pascal.
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, logicaland); break;
    case structured_primitive_logical_or:
      // FIXME: this is currently strict, differently from Pascal.
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, logicalor); break;
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
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, push);
      structuredvm_append_signed_literal_parameter (vmp, e->literal);
      break;
    case structured_expression_case_variable:
      {
        structured_register_index idx
          = structured_static_environment_lookup (env, e->variable);
        STRUCTUREDVM_APPEND_INSTRUCTION(vmp, push);
        STRUCTUREDVM_APPEND_REGISTER_PARAMETER(vmp, r, idx);
        break;
      }
    case structured_expression_case_primitive:
      structured_translate_primitive (vmp, e->primitive, e->primitive_operand_0,
                                      e->primitive_operand_1,
                                      env);
      break;
    default:
      jitter_fatal ("invalid expression case");
    }
}

/* Add code to translate the pointed statement AST to the pointed Jittery
   program, using the pointed static environment to be looked up and updated. */
static void
structured_translate_statement (struct structuredvm_program *vmp,
                                struct structured_statement *s,
                                struct structured_static_environment *env)
{
  /* FIXME: using textual labels like this is certainly correct, but barbaric.
     Even worse the static variable makes the generator non-reentrant when there
     would be no reason.  I have to provide a convenient API to generate fresh
     labels from a Jittery program. */
  char label_0 [100], label_1 [100];
  static long n = 0;
  sprintf (label_0, "L%li", n ++);
  sprintf (label_1, "L%li", n ++);

  switch (s->case_)
    {
    case structured_statement_case_skip:
      break;
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
      structured_translate_expression (vmp, s->print_expression, env);
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, print);
      break;
    case structured_statement_case_sequence:
      structured_translate_statement (vmp, s->sequence_statement_0, env);
      structured_translate_statement (vmp, s->sequence_statement_1, env);
      break;
    case structured_statement_case_if_then_else:
      structured_translate_expression (vmp, s->if_then_else_condition, env);
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, bf);
      structuredvm_append_symbolic_label_parameter (vmp, label_0);
      structured_translate_statement (vmp, s->if_then_else_then_branch, env);
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, b);
      structuredvm_append_symbolic_label_parameter (vmp, label_1);
      structuredvm_append_symbolic_label (vmp, label_0);
      structured_translate_statement (vmp, s->if_then_else_else_branch, env);
      structuredvm_append_symbolic_label (vmp, label_1);
      break;
    case structured_statement_case_if_then:
      structured_translate_expression (vmp, s->if_then_condition, env);
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, bf);
      structuredvm_append_symbolic_label_parameter (vmp, label_0);
      structured_translate_statement (vmp, s->if_then_then_branch, env);
      structuredvm_append_symbolic_label (vmp, label_0);
      break;
    case structured_statement_case_while_do:
      structuredvm_append_symbolic_label (vmp, label_0);
      structured_translate_expression (vmp, s->while_do_guard, env);
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, bf);
      structuredvm_append_symbolic_label_parameter (vmp, label_1);
      structured_translate_statement (vmp, s->while_do_body, env);
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, b);
      structuredvm_append_symbolic_label_parameter (vmp, label_0);
      structuredvm_append_symbolic_label (vmp, label_1);
      break;
    case structured_statement_case_repeat_until:
      structuredvm_append_symbolic_label (vmp, label_0);
      structured_translate_statement (vmp, s->repeat_until_body, env);
      structured_translate_expression (vmp, s->repeat_until_guard, env);
      STRUCTUREDVM_APPEND_INSTRUCTION(vmp, bf);
      structuredvm_append_symbolic_label_parameter (vmp, label_0);
      break;
    default:
      jitter_fatal ("invalid statement case");
    }
}

/* Add code to translate the pointed program AST to the pointed Jittery
   program. */
static void
structured_translate_program (struct structuredvm_program *vmp,
                              struct structured_program *p)
{
  struct structured_static_environment env;
  structured_static_environment_initialize (& env);
  structured_translate_statement (vmp, p->main_statement, & env);

  /* Because of a temporary Jitter limitation/bug, an unspecialized program can
     not end in a label.  It is harmless to unconditionally add one nop
     instruction at the end, just to prevent that situation. */
  STRUCTUREDVM_APPEND_INSTRUCTION(vmp, nop);

  structured_static_environment_finalize (& env);
}




/* Entry point: translate an AST program to a VM program.
 * ************************************************************************** */

struct structuredvm_program *
structured_make_vm_program (struct structured_program *p)
{
  /* Make an empty Jittery program. */
  struct structuredvm_program *vmp = structuredvm_make_program ();

  /* Translate the AST pointed by p into *vmp.  This of course works by
     recursion. */
  structured_translate_program (vmp, p);

  /* We're done. */
  return vmp;
}
