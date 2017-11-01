/* Jitter: rewriting rules, only used at VM-generation time.

   Copyright (C) 2017 Luca Saiu
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


/* Include Gnulib functions. */
#include <config.h>

/* Include standard headers. */
#include <stdlib.h>
#include <stdbool.h>

/* We use Gnulib lists and (here arbitrarily: Jitter's version would work just
   as well) xmalloc and friends. */
#include <gl_xlist.h>
#include <gl_array_list.h>
#include <xalloc.h>

/* Include the Jitter main header, generation-time headers, and generation-time
   utilities. */
#include <jitter/jitter.h>
#include <jitter/jitter-fatal.h>
#include "jitterc-rewrite.h"
#include "jitterc-vm.h"
#include "jitterc-utility.h"




/* Finding placeholder occurrences within rule patterns.
 * ************************************************************************** */

/* Add any placeholder occurring in the pointed argument pattern to the given
   unique string list. */
static void
jitterc_add_placeholders_from_argument_pattern
   (gl_list_t placeholders,
    struct jitterc_argument_pattern *ap)
{
  if (ap->placeholder_or_NULL != NULL)
    jitterc_list_add_string_unique (placeholders, ap->placeholder_or_NULL);
}

/* Add any placeholder occurring in the pointed pattern to the given unique
   string list. */
static void
jitterc_add_placeholders_from_pattern (gl_list_t placeholders,
                                       struct jitterc_instruction_pattern *ip)
{
  gl_list_t argument_patterns = ip->argument_patterns;
  size_t length = gl_list_size (argument_patterns);
  int i;
  for (i = 0; i < length; i ++)
    {
      struct jitterc_argument_pattern *ap
        = ((struct jitterc_argument_pattern *)
           gl_list_get_at (argument_patterns, i));
      jitterc_add_placeholders_from_argument_pattern (placeholders, ap);
    }
}

/* Add any placeholder occurring in the given pattern list to the given unique
   string list. */
static void
jitterc_add_placeholders_from_patterns (gl_list_t placeholders,
                                        gl_list_t in_instruction_patterns)
{
  size_t length = gl_list_size (in_instruction_patterns);
  int i;
  for (i = 0; i < length; i ++)
    {
      struct jitterc_instruction_pattern *ip
        = ((struct jitterc_instruction_pattern *)
           gl_list_get_at (in_instruction_patterns, i));
      jitterc_add_placeholders_from_pattern (placeholders, ip);
    }
}




/* Finding placeholder occurrences within rule templates and expressions.
 * ************************************************************************** */

/* Fail fatally if any placeholder occurring in the pointed template expression
   is not in the given list of strings.  This forward-declaration is needed
   because of mutual recursion. */
static void
jitterc_check_placeholders_in_expression
   (gl_list_t placeholders,
    struct jitterc_template_expression *e);

/* Fail fatally if any placeholder occurring in the given template expressions
   is not in the given list of strings. */
static void
jitterc_check_placeholders_in_expressions (gl_list_t placeholders,
                                           gl_list_t template_expressions)
{
  size_t length = gl_list_size (template_expressions);
  int i;
  for (i = 0; i < length; i ++)
    {
      struct jitterc_template_expression *te
        = ((struct jitterc_template_expression *)
           gl_list_get_at (template_expressions, i));
      jitterc_check_placeholders_in_expression (placeholders, te);
    }
}

static void
jitterc_check_placeholders_in_expression (gl_list_t placeholders,
                                          struct jitterc_template_expression *e)
{
  switch (e->case_)
    {
    case jitterc_instruction_argument_expression_case_boolean_constant:
    case jitterc_instruction_argument_expression_case_fixnum_constant:
      return;

    case jitterc_instruction_argument_expression_case_placeholder:
      if (! jitterc_list_has_string (placeholders, e->placeholder))
        jitter_fatal ("???:%i: unbound placeholder %s", e->line_no,
                      e->placeholder);
      break;

    case jitterc_instruction_argument_expression_case_operation:
      jitterc_check_placeholders_in_expressions (placeholders,
                                                 e->operand_expressions);
      break;

    default:
      jitter_fatal ("invalid template expression case");
    }
}

/* Fail fatally if any placeholder occurring in the pointed instruction template
   is not in the given list of strings. */
static void
jitterc_check_placeholders_in_template (gl_list_t placeholders,
                                        struct jitterc_instruction_template *it)
{
  jitterc_check_placeholders_in_expressions (placeholders,
                                             it->argument_expressions);
}

/* Fail fatally if any placeholder occurring in the given instruction templates
   is not in the given list of strings. */
static void
jitterc_check_placeholders_in_templates (gl_list_t placeholders,
                                         gl_list_t out_instruction_templates)
{
  size_t length = gl_list_size (out_instruction_templates);
  int i;
  for (i = 0; i < length; i ++)
    {
      struct jitterc_instruction_template *it
        = ((struct jitterc_instruction_template *)
           gl_list_get_at (out_instruction_templates, i));
      jitterc_check_placeholders_in_template (placeholders, it);
    }
}




/* Rewrite AST-construction API.
 * ************************************************************************** */

struct jitterc_rule*
jitterc_make_rule (gl_list_t in_instruction_patterns,
                   gl_list_t out_instruction_templates,
                   struct jitterc_template_expression *guard,
                   /* The list of placeholders is not given. */
                   char *name,
                   int line_no)
{
  struct jitterc_rule *res = xmalloc (sizeof (struct jitterc_rule));

  res->in_instruction_patterns = in_instruction_patterns;
  res->out_instruction_templates = out_instruction_templates;
  res->guard = guard;

  /* Compute the list of all the occurring placeholders in the pattern part. */
  res->placeholders = jitterc_make_empty_list ();
  jitterc_add_placeholders_from_patterns (res->placeholders,
                                          in_instruction_patterns);

  /* Check that every placeholder in the template and guard occurs in the
     pattern.  Any placeholder not found in the pattern is incorrect. */
  jitterc_check_placeholders_in_expression (res->placeholders,
                                            guard);
  jitterc_check_placeholders_in_templates (res->placeholders,
                                           out_instruction_templates);

  res->name = name;
  res->line_no = line_no;

  return res;
}

struct jitterc_instruction_pattern*
jitterc_make_instruction_pattern (char *instruction_name,
                                  gl_list_t argument_patterns,
                                  int line_no)
{
  struct jitterc_instruction_pattern *res
    = xmalloc (sizeof (struct jitterc_instruction_pattern));

  res->instruction_name = instruction_name;
  res->argument_patterns = argument_patterns;
  res->line_no = line_no;

  return res;
}

struct jitterc_argument_pattern*
jitterc_make_argument_pattern (enum jitterc_instruction_argument_kind kind,
                               bool has_literal,
                               union jitter_word literal,
                               char *placeholder_or_NULL,
                               int line_no)
{
  struct jitterc_argument_pattern *res
    = xmalloc (sizeof (struct jitterc_argument_pattern));

  res->kind = kind;
  res->has_literal = has_literal;
  res->literal = literal;
  res->placeholder_or_NULL = placeholder_or_NULL;
  res->line_no = line_no;

  return res;
}

struct jitterc_instruction_template*
jitterc_make_instruction_template (char *instruction_name,
                                   gl_list_t argument_expressions,
                                   int line_no)
{
  struct jitterc_instruction_template *res
    = xmalloc (sizeof (struct jitterc_instruction_template));

  res->instruction_name = instruction_name;
  res->argument_expressions = argument_expressions;
  res->line_no = line_no;

  return res;
}

/* Return a pointer to a new template expression with the given case, and the
   union part uninitialized.  Argument types match struct field types. */
static struct jitterc_template_expression*
jitterc_make_template_expression
   (enum jitterc_template_expression_case case_,
    int line_no)
{
  struct jitterc_template_expression *res
    = xmalloc (sizeof (struct jitterc_template_expression));

  /* The actually important fields are left uninitialized here. */
  res->case_ = case_;
  res->line_no = line_no;

  return res;
}

struct jitterc_template_expression*
jitterc_make_template_expression_boolean (bool bool_value,
                                          int line_no)
{
  struct jitterc_template_expression *res
    = jitterc_make_template_expression
         (jitterc_instruction_argument_expression_case_boolean_constant,
          line_no);

  res->constant.fixnum = bool_value;

  return res;
}

struct jitterc_template_expression*
jitterc_make_template_expression_fixnum (union jitter_word word,
                                         int line_no)
{
  struct jitterc_template_expression *res
    = jitterc_make_template_expression
         (jitterc_instruction_argument_expression_case_fixnum_constant,
          line_no);

  res->constant = word;

  return res;
}

struct jitterc_template_expression*
jitterc_make_template_expression_placeholder (char *placeholder,
                                              int line_no)
{
  struct jitterc_template_expression *res
    = jitterc_make_template_expression
         (jitterc_instruction_argument_expression_case_placeholder, line_no);

  res->placeholder = placeholder;

  return res;
}

struct jitterc_template_expression*
jitterc_make_template_expression_operation (char *operator_name,
                                            gl_list_t operand_expressions,
                                            int line_no)
{
  struct jitterc_template_expression *res
    = jitterc_make_template_expression
         (jitterc_instruction_argument_expression_case_operation, line_no);

  res->operator_name = operator_name;
  res->operand_expressions = operand_expressions;

  return res;
}




/* Adding rewrite rules to a VM.
 * ************************************************************************** */

void
jitterc_add_rule (struct jitterc_vm *vm,
                  struct jitterc_rule *rule)
{
  gl_list_add_last (vm->rewrite_rules, rule);
}
