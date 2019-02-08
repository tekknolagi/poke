/* Jittery structured language example: register-based code generator.

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


/* New experimental code generator, using Jitter facilities still to come.
 * ************************************************************************** */

/* This prints instructions to the output instead of generating real VM
   instructions.  I'm using this to convince myself that the new features for
   Jitter are worth the trouble, and are as easy to use as I think.  I'm
   optimistic. */

/* The kind of place where a datum is, as known at compile time. */
enum structured_location_case
  {
    /* The object can go anywhere.  This case is used when supplying a location
       to the code generator for compiling an expression, but is never given back
       as the location of code which has been compiled.  The location of compiled
       code will be specific, and use one of the other cases. */
    structured_location_case_anywhere,

    /* The object can be anywhere, except that it cannot be a constant.  This is
       needed to compile branches of conditional epxressions, which must resolve
       to the same location which cannot be, in general, *the same* constant. */
    structured_location_case_nonconstant,

    /* The object is be on the stack. */
    structured_location_case_stack,

    /* The object is in a register. */
    structured_location_case_register,

    /* The object is a literal, known at compile time. */
    structured_location_case_constant
  };

/* Where a datum is, as known at compile time. */
struct structured_location
{
  /* The case for this location.  See the comments within the definition of enum
     structured_location_case. */
  enum structured_location_case case_;

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

/* A litreal suitable for initializing a struct structured_location object to be an
   location with an anywhere case. */
#define STRUCTURED_LOCATION_ANYWHERE     \
  { structured_location_case_anywhere }

/* A litreal suitable for initializing a struct structured_location object to be an
   location with a non-constant case. */
#define STRUCTURED_LOCATION_NONCONSTANT     \
  { structured_location_case_nonconstant }

/* A litreal suitable for initializing a struct structured_location object to be the
   stack. */
#define STRUCTURED_LOCATION_STACK     \
  { structured_location_case_stack }

/* A litreal suitable for initializing a struct structured_location object to be a
   register, with the given index. */
#define STRUCTURED_LOCATION_REGISTER(index)                         \
  { structured_location_case_register, .register_index = (index) }

/* I'm actually printing instructions in a textual form to this stream instead
   of generating them. */
static FILE *structured_stream;

/* Emit a VM routine label. */
static void
structured_emit_label (struct structuredvm_program *vmp, structuredvm_label label)
{
  fprintf (structured_stream, "$L%lu:\n", (unsigned long) label);
}

/* Emit a VM instruction opcode. */
static void
structured_emit_opcode (struct structuredvm_program *vmp, const char *opcode_name)
{
  fprintf (structured_stream, "\t%s ", opcode_name);
}

static void
structured_emit_operand_stack (struct structuredvm_program *vmp, char stack_name)
{
  fprintf (structured_stream, "%%%c ", stack_name);
}

static void
structured_emit_operand_register (struct structuredvm_program *vmp,
                                  char class_name,
                                  structured_register_index index)
{
  fprintf (structured_stream, "%%%c%i ", class_name, (int) index);
}

static void
structured_emit_operand_literal (struct structuredvm_program *vmp,
                                 jitter_int value)
{
  fprintf (structured_stream, "%" JITTER_PRIi " ", value);
}

__attribute__ ((unused))
static void
structured_emit_operand_label (struct structuredvm_program *vmp,
                               structuredvm_label label)
{
  fprintf (structured_stream, "$L%lu ", (unsigned long) label);
}

static void
structured_emit_operand (struct structuredvm_program *vmp,
                         const struct structured_location *l)
{
  switch (l->case_)
    {
    case structured_location_case_anywhere:
      jitter_fatal ("invalid instruction operand: anywhere");

    case structured_location_case_nonconstant:
      jitter_fatal ("invalid instruction operand: nonconstant");

    case structured_location_case_stack:
      structured_emit_operand_stack (vmp, 's');
      break;

    case structured_location_case_register:
      structured_emit_operand_register (vmp, 'r', l->register_index);
      break;

    case structured_location_case_constant:
      structured_emit_operand_literal (vmp, l->literal_value);
      break;

    default:
      jitter_fatal ("invalid expression result location: unexpected (bug): %i",
                    (int) l->case_);
    };
}

/* Emit the end of an instruction.  This will not be needed with a real Jittery
   VM. */
static void
structured_close_instruction (struct structuredvm_program *vmp)
{
  fprintf (structured_stream, "\n");
}

/* Emit code to translate a literal expression with the given literal value.  The
   result of the expression will be stored, in emitted code, in the required
   location, updated here if its case is "anywhere". */
static void
structured_translate_expression_literal (struct structuredvm_program *vmp,
                                         struct structured_location *rl,
                                         jitter_int literal)
{
  switch (rl->case_)
    {
    case structured_location_case_anywhere:
      rl->case_ = structured_location_case_constant;
      rl->literal_value = literal;
      break;

    case structured_location_case_stack:
    case structured_location_case_nonconstant:
      structured_emit_opcode (vmp, "push");
      structured_emit_operand_literal (vmp, literal);
      structured_close_instruction (vmp);
      /* In case the requested location was nonconstant, we made it more
         specific.  Update it so that the caller knows the exact location. */
      rl->case_ = structured_location_case_stack;
      break;

    case structured_location_case_register:
      structured_emit_opcode (vmp, "set-r");
      structured_emit_operand_literal (vmp, literal);
      structured_emit_operand_register (vmp, 'r', rl->register_index);
      structured_close_instruction (vmp);
      break;

    case structured_location_case_constant:
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
structured_translate_expression_variable (struct structuredvm_program *vmp,
                                          struct structured_location *rl,
                                          structured_register_index ri)
{
  switch (rl->case_)
    {
    case structured_location_case_anywhere:
      rl->case_ = structured_location_case_register;
      rl->register_index = ri;
      break;

    case structured_location_case_stack:
      structured_emit_opcode (vmp, "push");
      structured_emit_operand_register (vmp, 'r', ri);
      structured_close_instruction (vmp);
      break;

    case structured_location_case_register:
      /* Generate nothing if the assignment is from a variable to itself. */
      if (ri != rl->register_index)
        {
          structured_emit_opcode (vmp, "r-to-r");
          structured_emit_operand_register (vmp, 'r', ri);
          structured_emit_operand_register (vmp, 'r', rl->register_index);
          structured_close_instruction (vmp);
        }
      break;

    case structured_location_case_constant:
      jitter_fatal ("invalid expression result location: constant");

    default:
      jitter_fatal ("invalid expression result location: unexpected (bug): %i",
                    (int) rl->case_);
    };
}

/* Forward-declaration. */
static void
structured_translate_expression (struct structuredvm_program *vmp,
                                 struct structured_location *rl,
                                 struct structured_expression *e,
                                 struct structured_static_environment *env);


/* Emit one opcode, without the operands, for the given primitive. */
static void
structured_translate_primitive_opcode (struct structuredvm_program *vmp,
                                       enum structured_primitive case_)
{
  switch (case_)
    {
    case structured_primitive_plus:
      structured_emit_opcode(vmp, "plus"); break;
    case structured_primitive_minus:
      structured_emit_opcode(vmp, "minus"); break;
    case structured_primitive_times:
      structured_emit_opcode(vmp, "times"); break;
    case structured_primitive_divided:
      structured_emit_opcode(vmp, "divided"); break;
    case structured_primitive_remainder:
      structured_emit_opcode(vmp, "remainder"); break;
    case structured_primitive_unary_minus:
      structured_emit_opcode(vmp, "uminus"); break;
    case structured_primitive_equal:
      structured_emit_opcode(vmp, "equal"); break;
    case structured_primitive_different:
      structured_emit_opcode(vmp, "different"); break;
    case structured_primitive_less:
      structured_emit_opcode(vmp, "less"); break;
    case structured_primitive_less_or_equal:
      structured_emit_opcode(vmp, "lessorequal"); break;
    case structured_primitive_greater:
      structured_emit_opcode(vmp, "greater"); break;
    case structured_primitive_greater_or_equal:
      structured_emit_opcode(vmp, "greaterorequal"); break;
    case structured_primitive_logical_not:
      structured_emit_opcode(vmp, "logicalnot"); break;
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
structured_translate_expression_primitive
   (struct structuredvm_program *vmp,
    struct structured_location *rl,
    enum structured_primitive case_,
    struct structured_expression *operand_0,
    struct structured_expression *operand_1,
    struct structured_static_environment *env)
{
  /* Translate the two operands, or the single operand in case the second is
     NULL (which means that the primitive is unary). */
  struct structured_location o0l = STRUCTURED_LOCATION_ANYWHERE;
  structured_translate_expression (vmp, &o0l, operand_0, env);
  struct structured_location o1l = STRUCTURED_LOCATION_ANYWHERE;
  if (operand_1 != NULL)
    structured_translate_expression (vmp, &o1l, operand_1, env);

  /* The result of the primitive will go to the stack if no specific location
     was requested. */
  if (rl->case_ == structured_location_case_anywhere)
    rl->case_ = structured_location_case_stack;

  /* Give the two operand results, or the one operand result, as operands of the
     primitive instruction. */
  structured_translate_primitive_opcode (vmp, case_);
  structured_emit_operand (vmp, & o0l);
  if (operand_1 != NULL)
    structured_emit_operand (vmp, & o1l);
  structured_emit_operand (vmp, rl);
  structured_close_instruction (vmp);
}

/* Forward-declaration. */
static void
structured_translate_condtitional (struct structuredvm_program *vmp,
                                   struct structured_expression *e,
                                   structuredvm_label label,
                                   bool branch_on_true,
                                   struct structured_static_environment *env);

/* Emit code to translate the pointed expression AST to the pointed Jittery
   program, using the pointed static environment to be looked up and updated.
   The result of the expression will be stored, in emitted code, in the required
   location, updated here if its case is "anywhere". */
static void
structured_translate_expression (struct structuredvm_program *vmp,
                                 struct structured_location *rl,
                                 struct structured_expression *e,
                                 struct structured_static_environment *env)
{
  switch (e->case_)
    {
    case structured_expression_case_literal:
      structured_translate_expression_literal (vmp, rl, e->literal);
      break;
    case structured_expression_case_variable:
      {
        structured_register_index idx
          = structured_static_environment_lookup (env, e->variable);
        structured_translate_expression_variable (vmp, rl, idx);
        break;
      }
    case structured_expression_case_if_then_else:
      {
        /* If * rl is an anywhere location, change it to a different case: here
           * rl cannot be (in the general case) a constant.  But it can be a
           variable.  The problem is that if one of the two branches compiled to
           a constant location then if would only be possible to compile the
           other branch to the *same* constant location, which will not happen
           in any non-trivial conditional.  The result must go to *one*
           non-constant location.  It is not a problem if it goes to a variable
           or to a temporary. */
        if (rl->case_ == structured_location_case_anywhere)
          rl->case_ = structured_location_case_nonconstant;

        structuredvm_label before_else = structuredvm_fresh_label (vmp);
        structuredvm_label after_else = structuredvm_fresh_label (vmp);
        structured_translate_condtitional (vmp, e->if_then_else_condition,
                                           before_else,
                                           false,
                                           env);
        structured_translate_expression (vmp, rl, e->if_then_else_then_branch,
                                         env);
        structured_emit_opcode (vmp, "b");
        structured_emit_operand_label (vmp, after_else);
        structured_close_instruction (vmp);
        structured_emit_label (vmp, before_else);
        structured_translate_expression (vmp, rl, e->if_then_else_else_branch,
                                         env);
        structured_emit_label (vmp, after_else);
        break;
      }
    case structured_expression_case_primitive:
      {
        structured_translate_expression_primitive (vmp, rl, e->primitive,
                                                   e->primitive_operand_0,
                                                   e->primitive_operand_1, env);
        break;
      }
    default:
      jitter_fatal ("invalid expression case (bug): %i", (int) e->case_);
    }
}

/* Given a boolean primitive case, return the name of the VM instruction
   implementing a conditional branch over it.
   Fail if conditional branching is not defined on the given primitive. */
static const char *
structured_boolean_primitive_to_instruction (enum structured_primitive p)
{
  switch (p)
    {
    case structured_primitive_equal:
      return "be";
    case structured_primitive_different:
      return "bne";
    case structured_primitive_less:
      return "bl";
    case structured_primitive_less_or_equal:
      return "ble";
    case structured_primitive_greater:
      return "bg";
    case structured_primitive_greater_or_equal:
      return "bge";
    default:
      jitter_fatal ("boolean (?) primitive not supporting branching: %i",
                    (int) p);
    }
}

/* A helper for structured_translate_condtitional, defined below.  Emit code for a
   conditioanl primitive with the given case and operands, to conditionally
   branch to the pointed label according the the result of the primitive.
   Generate a branch-on-non-zero if branch_on_true is non-false; generate a
   branch-on-zero if branch_on_true is false. */
static void
structured_translate_condtitional_primitive
   (struct structuredvm_program *vmp,
    enum structured_primitive case_,
    struct structured_expression *operand_0,
    struct structured_expression *operand_1,
    structuredvm_label label,
    bool branch_on_true,
    struct structured_static_environment *env)
{
  switch (case_)
    {
    case structured_primitive_equal:
    case structured_primitive_different:
    case structured_primitive_less:
    case structured_primitive_less_or_equal:
    case structured_primitive_greater:
    case structured_primitive_greater_or_equal:
      {
        /* Translate the two operands, the ordinary way. */
        struct structured_location o0l = STRUCTURED_LOCATION_ANYWHERE;
        structured_translate_expression (vmp, &o0l, operand_0, env);
        struct structured_location o1l = STRUCTURED_LOCATION_ANYWHERE;
        structured_translate_expression (vmp, &o1l, operand_1, env);
        /* Generate a conditional branch, with the appropriate opcode for the
           primitive case, reversed if we need to branch on false. */
        enum structured_primitive actual_case;
        if (branch_on_true)
          actual_case = case_;
        else
          actual_case = structured_reverse_boolean_primitive (case_);
        const char *opcode
          = structured_boolean_primitive_to_instruction (actual_case);
        structured_emit_opcode (vmp, opcode);
        structured_emit_operand (vmp, & o0l);
        structured_emit_operand (vmp, & o1l);
        structured_emit_operand_label (vmp, label);
        structured_close_instruction (vmp);
        break;
      }
    case structured_primitive_logical_not:
      /* Translate the not subexpression as an ordinary contitional,
         swapping the on_true and on_false labels. */
      structured_translate_condtitional (vmp, operand_0, label,
                                         ! branch_on_true, env);
      break;
    default:
      {
        /* The primitive is not a comparison primitive producing a boolean from
           non-booleans, or a boolean-composition primitive producing a boolean
           from other booleans.  We have to translate the primitive as an
           ordinary expression, materializing the result, and then conditionally
           branch according to its value. */
        struct structured_location rl = STRUCTURED_LOCATION_ANYWHERE;
        structured_translate_expression_primitive (vmp, & rl, case_, operand_0,
                                                   operand_1, env);
        if (branch_on_true)
          structured_emit_opcode (vmp, "bnz");
        else
          structured_emit_opcode (vmp, "bz");
        structured_emit_operand (vmp, & rl);
        structured_emit_operand_label (vmp, label);
        structured_close_instruction (vmp);
      }
    }
}

/* Emit code for a conditional branch, generating code to find the truth value
   of the given expression and then possibly jumping the given label.  If
   branch_on_true is non-false then branch when the given condition (as an
   expression) would evaluate to a non-zero value; if branch_on_true is false
   branch when the expression would evaluate to zero.
   When the generated code does not branch, it simply falls thru. */
static void
structured_translate_condtitional (struct structuredvm_program *vmp,
                                   struct structured_expression *e,
                                   structuredvm_label label,
                                   bool branch_on_true,
                                   struct structured_static_environment *env)
{
  switch (e->case_)
    {
    case structured_expression_case_literal:
      {
        /* The condition is constant: generate an unconditional branch, or
           nothing. */
        if ((branch_on_true && e->literal != 0)
            || (! branch_on_true && e->literal == 0))
          {
            structured_emit_opcode (vmp, "b");
            structured_emit_operand_label (vmp, label);
            structured_close_instruction (vmp);
          }
        break;
      }
    case structured_expression_case_variable:
      {
        structured_register_index idx
          = structured_static_environment_lookup (env, e->variable);
        if (branch_on_true)
          structured_emit_opcode (vmp, "bnz");
        else
          structured_emit_opcode (vmp, "bz");
        structured_emit_operand_register (vmp, 'r', idx);
        structured_emit_operand_label (vmp, label);
        structured_close_instruction (vmp);
        break;
      }
    case structured_expression_case_primitive:
      {
        structured_translate_condtitional_primitive (vmp, e->primitive,
                                                     e->primitive_operand_0,
                                                     e->primitive_operand_1,
                                                     label, branch_on_true,
                                                     env);
        break;
      }
    case structured_expression_case_if_then_else:
      {
        /* Translate
              branch[-unless] (if C then T else E) $L
           into
                branch-unless C $E
                branch[-unless] T $L
                goto $AFTER
              $E:
                branch[-unless] E $L
              $AFTER: */
        struct structured_expression *c = e->if_then_else_condition;
        struct structured_expression *tb = e->if_then_else_then_branch;
        struct structured_expression *eb = e->if_then_else_else_branch;
        structuredvm_label before_else = structuredvm_fresh_label (vmp);
        structuredvm_label after_else = structuredvm_fresh_label (vmp);
        structured_translate_condtitional (vmp, c, before_else, false, env);
        structured_translate_condtitional (vmp, tb, label, branch_on_true, env);
        structured_emit_opcode (vmp, "b");
        structured_emit_operand_label (vmp, after_else);
        structured_close_instruction (vmp);
        structured_emit_label (vmp, before_else);
        structured_translate_condtitional (vmp, eb, label, branch_on_true, env);
        structured_emit_label (vmp, after_else);
        break;
      }
    default:
      jitter_fatal ("invalid (conditional) expression case: %i",
                    (int) e->case_);
    }
}

/* Emit code to translate the pointed statement AST to the pointed Jittery
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
        struct structured_location vl = STRUCTURED_LOCATION_REGISTER(idx);
        structured_translate_expression (vmp, &vl, s->assignment_expression,
                                         env);
        break;
      }
    case structured_statement_case_print:
      {
        struct structured_location vl = STRUCTURED_LOCATION_ANYWHERE;
        structured_translate_expression (vmp, &vl, s->print_expression, env);
        structured_emit_opcode (vmp, "print");
        structured_emit_operand (vmp, & vl);
        structured_close_instruction (vmp);
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

        structured_translate_condtitional (vmp, s->if_then_else_condition,
                                           before_else,
                                           false,
                                           env);
        structured_translate_statement (vmp, s->if_then_else_then_branch, env);
        structured_emit_opcode (vmp, "b");
        structured_emit_operand_label (vmp, after_else);
        structured_close_instruction (vmp);
        structured_emit_label (vmp, before_else);
        structured_translate_statement (vmp, s->if_then_else_else_branch, env);
        structured_emit_label (vmp, after_else);
        break;
      }
    case structured_statement_case_if_then:
      {
        structuredvm_label after_then = structuredvm_fresh_label (vmp);
        structured_translate_condtitional (vmp, s->if_then_condition,
                                           after_then,
                                           false,
                                           env);
        structured_translate_statement (vmp, s->if_then_then_branch, env);
        structured_emit_label (vmp, after_then);
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
        structured_emit_opcode (vmp, "b");
        structured_emit_operand_label (vmp, before_guard);
        structured_close_instruction (vmp);
        structured_emit_label (vmp, loop_beginning);
        structured_translate_statement (vmp, s->while_do_body, env);
        structured_emit_label (vmp, before_guard);
        structured_translate_condtitional (vmp, s->while_do_guard,
                                           loop_beginning,
                                           true,
                                           env);
        break;
      }
    case structured_statement_case_repeat_until:
      {
        structuredvm_label before_body = structuredvm_fresh_label (vmp);
        structured_emit_label (vmp, before_body);
        structured_translate_statement (vmp, s->repeat_until_body, env);
        structured_translate_condtitional (vmp, s->repeat_until_guard,
                                           before_body,
                                           false,
                                           env);
        break;
      }
    default:
      jitter_fatal ("invalid statement case (bug): %i", (int) s->case_);
    }
}

static void
structured_translate_program (struct structuredvm_program *vmp,
                              struct structured_program *p)
{
  struct structured_static_environment *env
    = structured_static_environment_make ();

  structured_translate_statement (vmp, p->main_statement, env);
  /* Generate a closing exitvm instruction.  I will need to remove this when
     actually generating VM instructions, as the closing instruction will come
     automatically. */
  structured_emit_opcode (vmp, "exitvm");
  structured_close_instruction (vmp);
  fflush (structured_stream);
  structured_static_environment_destroy (env);
}




/* Entry point: translate an AST program to a VM program.
 * ************************************************************************** */

struct structuredvm_program *
structured_make_vm_program_register (struct structured_program *p)
{
  /* Translate the AST pointed by p into *vmp.  This of course works by
     recursion. */
  structured_stream = stderr;
  struct structuredvm_program *vmp = structuredvm_make_program ();
  structured_translate_program (vmp, p);

  /* We're done. */
  return vmp;
}
