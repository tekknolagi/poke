/* Jittery structured language example: code generator implementation.

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
    case structured_expression_case_primitive:
      {
        structured_translate_primitive (vmp, e->primitive,
                                        e->primitive_operand_0,
                                        e->primitive_operand_1, env);
        break;
      }
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
  switch (s->case_)
    {
    case structured_statement_case_skip:
      {
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
    case structured_statement_case_if_then:
      {
        structuredvm_label after_then = structuredvm_fresh_label (vmp);
        structured_translate_expression (vmp, s->if_then_condition, env);
        STRUCTUREDVM_APPEND_INSTRUCTION(vmp, bf);
        structuredvm_append_label_parameter (vmp, after_then);
        structured_translate_statement (vmp, s->if_then_then_branch, env);
        structuredvm_append_label (vmp, after_then);
        break;
      }
    case structured_statement_case_while_do:
      {
        /* I compile a while..do loop as a do..while loop, with a single
           conditional branch at the end:
                 b $before_guard
              $loop_beginning:
                 [body]
              $before_guard:
                 [guard]
                 bt $loop_beginning */
        structuredvm_label loop_beginning = structuredvm_fresh_label (vmp);
        structuredvm_label before_guard = structuredvm_fresh_label (vmp);
        STRUCTUREDVM_APPEND_INSTRUCTION(vmp, b);
        structuredvm_append_label_parameter (vmp, before_guard);
        structuredvm_append_label (vmp, loop_beginning);
        structured_translate_statement (vmp, s->while_do_body, env);
        structuredvm_append_label (vmp, before_guard);
        structured_translate_expression (vmp, s->while_do_guard, env);
        STRUCTUREDVM_APPEND_INSTRUCTION(vmp, bt);
        structuredvm_append_label_parameter (vmp, loop_beginning);
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
  structured_static_environment_finalize (& env);
}




/* New experimental code generator, using Jitter facilities still to come.
 * ************************************************************************** */

/* This prints instructions to the output instead of generating real VM
   instructions.  I'm using this to convince myself that the new features for
   Jitter are worth the trouble, and are as easy to use as I think.  I'm
   optimistic. */

/* The kind of place where a datum is, as known at compile time. */
enum new_location_case
  {
    /* The object can go anywhere.  This case is used when supplying a location
       to the code generator for compiling an expression, but is never given back
       as the location of code which has been compiled.  The location of compiled
       code will be specific, and use one of the other cases. */
    new_location_case_anywhere,

    /* The object is be on the stack. */
    new_location_case_stack,

    /* The object is in a register. */
    new_location_case_register,

    /* The object is a literal, known at compile time. */
    new_location_case_constant
  };

/* Where a datum is, as known at compile time. */
struct new_location
{
  /* The case for this location.  See the comments within the definition of enum
     new_location_case. */
  enum new_location_case case_;

  /* Other data complementing the location, as needed for some cases. */
  union
  {
    /* A register index, only used when the case is register. */
    structured_register_index register_index;

    /* Notice that there is no additional data needed for the stack case: the
       operand or result will be on the stack at a depth which is always known
       by the code generator. */

    /* The value of the literal, only used when the case is literal. */
    jitter_int literal_value;
  };
};

/* A litreal suitable for initializing a struct new_location object to be an
   location with an anywhere case. */
#define NEW_ANYWHERE_LOCATION  \
  { new_location_case_anywhere }

/* A litreal suitable for initializing a struct new_location object to be the
   stack. */
#define NEW_ANYWHERE_LOCATION_STACK  \
  { new_location_case_stack }

/* A litreal suitable for initializing a struct new_location object to be a
   register, with the given index. */
#define NEW_ANYWHERE_LOCATION_REGISTER(index)  \
  { new_location_case_register, .register_index = (index) }

/* I'm actually printing instructions in a textual form to this stream instead
   of generating them. */
static FILE *new_stream;

/* Emit a VM routine label. */
static void
new_emit_label (struct structuredvm_program *vmp, structuredvm_label label)
{
  fprintf (new_stream, "$L%lu:\n", (unsigned long) label);
}

/* Emit a VM instruction opcode. */
static void
new_emit_opcode (struct structuredvm_program *vmp, const char *opcode_name)
{
  fprintf (new_stream, "\t%s ", opcode_name);
}

static void
new_emit_operand_stack (struct structuredvm_program *vmp, char stack_name)
{
  fprintf (new_stream, "%%%c ", stack_name);
}

static void
new_emit_operand_register (struct structuredvm_program *vmp,
                           char class_name,
                           structured_register_index index)
{
  fprintf (new_stream, "%%%c%i ", class_name, (int) index);
}

static void
new_emit_operand_literal (struct structuredvm_program *vmp,
                          jitter_int value)
{
  fprintf (new_stream, "%" JITTER_PRIi " ", value);
}

__attribute__ ((unused))
static void
new_emit_operand_label (struct structuredvm_program *vmp,
                        structuredvm_label label)
{
  fprintf (new_stream, "$L%lu ", (unsigned long) label);
}

static void
new_emit_operand (struct structuredvm_program *vmp,
                  const struct new_location *l)
{
  switch (l->case_)
    {
    case new_location_case_anywhere:
      jitter_fatal ("invalid instruction operand: anywhere");

    case new_location_case_stack:
      new_emit_operand_stack (vmp, 's');
      break;

    case new_location_case_register:
      new_emit_operand_register (vmp, 'r', l->register_index);
      break;

    case new_location_case_constant:
      new_emit_operand_literal (vmp, l->literal_value);
      break;

    default:
      jitter_fatal ("invalid expression result location: unexpected (bug): %i",
                    (int) l->case_);
    };
}

/* Emit the end of an instruction.  This will not be needed with a real Jittery
   VM. */
static void
new_close_instruction (struct structuredvm_program *vmp)
{
  fprintf (new_stream, "\n");
}

/* Emit code to translate a literal expression with the given literal value.  The
   result of the expression will be stored, in emitted code, in the required
   location, updated here if its case is "anywhere". */
static void
new_translate_expression_literal (struct structuredvm_program *vmp,
                                  struct new_location *rl,
                                  jitter_int literal)
{
  switch (rl->case_)
    {
    case new_location_case_anywhere:
      rl->case_ = new_location_case_constant;
      rl->literal_value = literal;
      break;

    case new_location_case_stack:
      new_emit_opcode (vmp, "push");
      new_emit_operand_literal (vmp, literal);
      new_close_instruction (vmp);
      break;

    case new_location_case_register:
      new_emit_opcode (vmp, "set-r");
      new_emit_operand_literal (vmp, literal);
      new_emit_operand_register (vmp, 'r', rl->register_index);
      new_close_instruction (vmp);
      break;

    case new_location_case_constant:
      jitter_fatal ("invalid expression result location: constant");

    default:
      jitter_fatal ("invalid expression result location: unexpected (bug): %i",
                    (int) rl->case_);
    };
}

/* Emit code to translate a variable expression whose value is held in a register
   with the given index.  The result of the expression will be stored, in
   emitted code, in the required location, updated here if its case is
   "anywhere". */
static void
new_translate_expression_variable (struct structuredvm_program *vmp,
                                   struct new_location *rl,
                                   structured_register_index ri)
{
  switch (rl->case_)
    {
    case new_location_case_anywhere:
      rl->case_ = new_location_case_register;
      rl->register_index = ri;
      break;

    case new_location_case_stack:
      new_emit_opcode (vmp, "push");
      new_emit_operand_register (vmp, 'r', ri);
      new_close_instruction (vmp);
      break;

    case new_location_case_register:
      /* Generate nothing if the assignment is from a variable to itself. */
      if (ri != rl->register_index)
        {
          new_emit_opcode (vmp, "r-to-r");
          new_emit_operand_register (vmp, 'r', ri);
          new_emit_operand_register (vmp, 'r', rl->register_index);
          new_close_instruction (vmp);
        }
      break;

    case new_location_case_constant:
      jitter_fatal ("invalid expression result location: constant");

    default:
      jitter_fatal ("invalid expression result location: unexpected (bug): %i",
                    (int) rl->case_);
    };
}

/* Forward-declaration. */
static void
new_translate_expression (struct structuredvm_program *vmp,
                          struct new_location *rl,
                          struct structured_expression *e,
                          struct structured_static_environment *env);

/* Emit one opcode, without the operands, for the given primitive. */
static void
new_translate_primitive_opcode (struct structuredvm_program *vmp,
                                enum structured_primitive case_)
{
  switch (case_)
    {
    case structured_primitive_plus:
      new_emit_opcode(vmp, "plus"); break;
    case structured_primitive_minus:
      new_emit_opcode(vmp, "minus"); break;
    case structured_primitive_times:
      new_emit_opcode(vmp, "times"); break;
    case structured_primitive_divided:
      new_emit_opcode(vmp, "divided"); break;
    case structured_primitive_remainder:
      new_emit_opcode(vmp, "remainder"); break;
    case structured_primitive_unary_minus:
      new_emit_opcode(vmp, "uminus"); break;
    case structured_primitive_equal:
      new_emit_opcode(vmp, "equal"); break;
    case structured_primitive_different:
      new_emit_opcode(vmp, "different"); break;
    case structured_primitive_less:
      new_emit_opcode(vmp, "less"); break;
    case structured_primitive_less_or_equal:
      new_emit_opcode(vmp, "lessorequal"); break;
    case structured_primitive_greater:
      new_emit_opcode(vmp, "greater"); break;
    case structured_primitive_greater_or_equal:
      new_emit_opcode(vmp, "greaterorequal"); break;
    case structured_primitive_logical_and:
      // FIXME: this is currently strict, differently from Pascal.
      new_emit_opcode(vmp, "logicaland"); break;
    case structured_primitive_logical_or:
      // FIXME: this is currently strict, differently from Pascal.
      new_emit_opcode(vmp, "logicalor"); break;
    case structured_primitive_logical_not:
      new_emit_opcode(vmp, "logicalnot"); break;
    default:
      jitter_fatal ("invalid primitive case: unexpected (bug): %i",
                    (int) case_);
    }
}

/* Emit code to translate a primitive expression with the given case and
   operands, using the pointed static environment to be looked up and updated.
   The result of the expression will be stored, in emitted code, in the required
   location, updated here if its case is "anywhere". */
static void
new_translate_expression_primitive (struct structuredvm_program *vmp,
                                    struct new_location *rl,
                                    enum structured_primitive case_,
                                    struct structured_expression *operand_0,
                                    struct structured_expression *operand_1,
                                    struct structured_static_environment *env)
{
  /* Translate the two operands, or the single operand in case the second is
     NULL (which means that the primitive is unary). */
  struct new_location o0l = NEW_ANYWHERE_LOCATION;
  new_translate_expression (vmp, &o0l, operand_0, env);
  struct new_location o1l = NEW_ANYWHERE_LOCATION;
  if (operand_1 != NULL)
    new_translate_expression (vmp, &o1l, operand_1, env);

  /* The result of the primitive will go to the stack if no specific location
     was requested. */
  if (rl->case_ == new_location_case_anywhere)
    rl->case_ = new_location_case_stack;

  /* Give the two operand results, or the one operand result, as operands of the
     primitive instruction. */
  new_translate_primitive_opcode (vmp, case_);
  new_emit_operand (vmp, & o0l);
  if (operand_1 != NULL)
    new_emit_operand (vmp, & o1l);
  new_emit_operand (vmp, rl);
  new_close_instruction (vmp);
}

/* Emit code to translate the pointed expression AST to the pointed Jittery
   program, using the pointed static environment to be looked up and updated.
   The result of the expression will be stored, in emitted code, in the required
   location, updated here if its case is "anywhere". */
static void
new_translate_expression (struct structuredvm_program *vmp,
                          struct new_location *rl,
                          struct structured_expression *e,
                          struct structured_static_environment *env)
{
  switch (e->case_)
    {
    case structured_expression_case_literal:
      new_translate_expression_literal (vmp, rl, e->literal);
      break;
    case structured_expression_case_variable:
      {
        structured_register_index idx
          = structured_static_environment_lookup (env, e->variable);
        new_translate_expression_variable (vmp, rl, idx);
        break;
      }
    case structured_expression_case_primitive:
      {
        new_translate_expression_primitive (vmp, rl, e->primitive,
                                            e->primitive_operand_0,
                                            e->primitive_operand_1,
                                            env);
        break;
      }
    default:
      jitter_fatal ("invalid expression case");
    }
}

/* Emit code to translate the pointed statement AST to the pointed Jittery
   program, using the pointed static environment to be looked up and updated. */
static void
new_translate_statement (struct structuredvm_program *vmp,
                         struct structured_statement *s,
                         struct structured_static_environment *env)
{
  switch (s->case_)
    {
    case structured_statement_case_skip:
      {
        break;
      }
    case structured_statement_case_assignment:
      {
        structured_register_index idx = structured_static_environment_lookup (env, s->assignment_variable);
        struct new_location vl = NEW_ANYWHERE_LOCATION_REGISTER(idx);
        new_translate_expression (vmp, &vl, s->assignment_expression, env);
        break;
      }
    case structured_statement_case_print:
      {
        struct new_location vl = NEW_ANYWHERE_LOCATION;
        new_translate_expression (vmp, &vl, s->print_expression, env);
        new_emit_opcode (vmp, "print");
        new_emit_operand (vmp, & vl);
        new_close_instruction (vmp);
        break;
      }
    case structured_statement_case_sequence:
      {
        new_translate_statement (vmp, s->sequence_statement_0, env);
        new_translate_statement (vmp, s->sequence_statement_1, env);
        break;
      }
    case structured_statement_case_if_then_else:
      {
        structuredvm_label before_else = structuredvm_fresh_label (vmp);
        structuredvm_label after_else = structuredvm_fresh_label (vmp);
        struct new_location cl = NEW_ANYWHERE_LOCATION;
        new_translate_expression (vmp, &cl, s->if_then_condition, env);
        new_emit_opcode (vmp, "bf");
        STRUCTUREDVM_APPEND_INSTRUCTION(vmp, bf);
        new_emit_operand (vmp, & cl);
        new_emit_operand_label (vmp, before_else);
        new_close_instruction (vmp);
        new_translate_statement (vmp, s->if_then_else_then_branch, env);
        new_emit_opcode (vmp, "b");
        new_emit_operand_label (vmp, after_else);
        new_close_instruction (vmp);
        new_emit_label (vmp, before_else);
        new_translate_statement (vmp, s->if_then_else_else_branch, env);
        new_emit_label (vmp, after_else);
        break;
      }
    case structured_statement_case_if_then:
      {
        structuredvm_label after_then = structuredvm_fresh_label (vmp);
        struct new_location cl = NEW_ANYWHERE_LOCATION;
        new_translate_expression (vmp, &cl, s->if_then_condition, env);
        new_emit_opcode (vmp, "bf");
        STRUCTUREDVM_APPEND_INSTRUCTION(vmp, bf);
        new_emit_operand (vmp, & cl);
        new_emit_operand_label (vmp, after_then);
        new_close_instruction (vmp);
        new_translate_statement (vmp, s->if_then_then_branch, env);
        new_emit_label (vmp, after_then);
        break;
      }
    case structured_statement_case_while_do:
      {
        /* I compile a while..do loop as a do..while loop, with a single
           conditional branch at the end:
                 b $before_guard
              $loop_beginning:
                 [body]
              $before_guard:
                 [guard]
                 bt $loop_beginning */
        structuredvm_label loop_beginning = structuredvm_fresh_label (vmp);
        structuredvm_label before_guard = structuredvm_fresh_label (vmp);
        new_emit_opcode (vmp, "b");
        new_emit_operand_label (vmp, before_guard);
        new_close_instruction (vmp);
        new_emit_label (vmp, loop_beginning);
        new_translate_statement (vmp, s->while_do_body, env);
        new_emit_label (vmp, before_guard);
        struct new_location gl = NEW_ANYWHERE_LOCATION;
        new_translate_expression (vmp, & gl, s->while_do_guard, env);
        new_emit_opcode(vmp, "bt");
        new_emit_operand (vmp, & gl);
        new_emit_operand_label (vmp, loop_beginning);
        new_close_instruction (vmp);
        break;
      }
    case structured_statement_case_repeat_until:
      {
        structuredvm_label before_body = structuredvm_fresh_label (vmp);
        new_emit_label (vmp, before_body);
        new_translate_statement (vmp, s->repeat_until_body, env);
        struct new_location gl = NEW_ANYWHERE_LOCATION;
        new_translate_expression (vmp, & gl, s->repeat_until_guard, env);
        new_emit_opcode (vmp, "bf");
        new_emit_operand (vmp, & gl);
        new_emit_operand_label (vmp, before_body);
        new_close_instruction (vmp);
        break;
      }
    default:
      jitter_fatal ("invalid statement case");
    }
}

static void
new_translate_program (struct structuredvm_program *vmp,
                       struct structured_program *p)
{
  struct structured_static_environment env;
  structured_static_environment_initialize (& env);

  fprintf (new_stream, "==============================\n");
  new_translate_statement (vmp, p->main_statement, & env);
  new_emit_opcode (vmp, "exitvm");
  new_close_instruction (vmp);
  fprintf (new_stream, "==============================\n\n");
  fflush (new_stream);
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

  /* Call the experimental code generator as well. */
  new_stream = stderr;
  struct structuredvm_program *nvmp = structuredvm_make_program ();
  new_translate_program (nvmp, p);

  /* We're done. */
  return vmp;
}
