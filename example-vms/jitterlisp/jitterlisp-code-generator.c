/* JitterLisp: Jittery VM code generator.

   Copyright (C) 2018 Luca Saiu
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


#include "jitterlisp-code-generator.h"

#include <stdio.h> // FIXME: remove unless needed.
#include <string.h> // FIXME: remove unless needed.

#include "jitterlisp.h"
#include <jitter/jitter-hash.h>

#include "jitterlispvm-vm.h"




/* Code generation: (pseudo-) instruction validation.
 * ************************************************************************** */

/* This section contains internal utility functions to check that VM
   instructions and labels are encoded correctly in s-expressions.  We need to
   validate VM code coming from Lisp, which might come from the user and contain
   mistakes. */

/* FIXME: don't leak on compilation errors, and don't break the closure
   provided by the user. */
static void
jitterlisp_compilation_error (char *message)
{
  printf ("compile-time failure:\n");
  jitterlisp_error_cloned (message);
}

/* First advance the given list l-value, assigning it to the cdr of its current
   value; then assign the given element l-value to be the car of the new list
   value, checking that the car type (given as an uppercase suffix) is as
   required.
   Error out if the type doesn't match of the given list is not actually a list.

   This is meant to be called (usually thru JITTERLISP_ARGUMENT_DECODED ) on a
   Lisp-encoded instruction once per argument, in order, to both extract their
   content and validate them, in a single pass; JITTERLISP_NO_MORE_ARGUMENTS
   should be called at the end to ensure that there are no excess arguments.
   Each macro call will consume the current car (the opcode at the first call),
   which has already been checked before. */
#define JITTERLISP_ARGUMENT(_jitterlisp_element,                               \
                            _jitterlisp_list,                                  \
                            _jitterlisp_uppercase_type)                        \
  JITTER_BEGIN_                                                                \
    if (! JITTERLISP_IS_CONS(_jitterlisp_list))                                \
      jitterlisp_compilation_error ("non-cons VM instruction substructure");   \
    (_jitterlisp_list) = JITTERLISP_EXP_C_A_CDR(_jitterlisp_list);             \
    jitterlisp_object _jitterlisp_car_tmp                                      \
      = JITTERLISP_EXP_C_A_CAR(_jitterlisp_list);                              \
    if (! JITTER_CONCATENATE_TWO(JITTERLISP_IS_, _jitterlisp_uppercase_type)(  \
               (_jitterlisp_car_tmp)))                                         \
      jitterlisp_compilation_error ("non-"                                     \
                                    JITTER_STRINGIFY(                          \
                                       _jitterlisp_uppercase_type)             \
                                    " VM instruction argument");               \
    (_jitterlisp_element) = _jitterlisp_car_tmp;                               \
  JITTER_END_

/* Like JITTERLISP_ARGUMENT but set the element lvalue to a decoded Lisp
   value, by calling the appropriate JITTERLISP_*_DECODE macro. */
#define JITTERLISP_ARGUMENT_DECODED(_jitterlisp_element,                 \
                                    _jitterlisp_list,                    \
                                    _jitterlisp_uppercase_type)          \
  JITTER_BEGIN_                                                          \
    jitterlisp_object _jitterlisp_encoded_element;                       \
    JITTERLISP_ARGUMENT(_jitterlisp_encoded_element,                     \
                        _jitterlisp_list,                                \
                        _jitterlisp_uppercase_type);                     \
    (_jitterlisp_element)                                                \
      = JITTER_CONCATENATE_THREE(JITTERLISP_,                            \
                                 _jitterlisp_uppercase_type,             \
                                 _DECODE)(_jitterlisp_encoded_element);  \
  JITTER_END_

/* Error out if the given argument is not a singleton list.  See the comment
   before JITTERLISP_ARGUMENT for rationale. */
#define JITTERLISP_NO_MORE_ARGUMENTS(_jitterlisp_list_exp)                    \
  JITTER_BEGIN_                                                               \
    jitterlisp_object _jitterlisp_list = (_jitterlisp_list_exp);              \
    if (! JITTERLISP_IS_CONS(_jitterlisp_list))                               \
      jitterlisp_compilation_error ("non-cons VM instruction substructure");  \
    (_jitterlisp_list) = JITTERLISP_EXP_C_A_CDR(_jitterlisp_list);            \
    if (! JITTERLISP_IS_EMPTY_LIST(_jitterlisp_list))                         \
      {                                                                       \
        if (JITTERLISP_IS_CONS(_jitterlisp_list))                             \
          jitterlisp_compilation_error ("excess instruction argument");       \
        else                                                                  \
          jitterlisp_compilation_error ("non-cons in instruction");           \
      }                                                                       \
  JITTER_END_

/* Return the name of the given instruction as a C string, shared with the
   internal symbol representation.  Error out if the instruction is
   ill-formed. */
static const char *
jitterlisp_instruction_name (jitterlisp_object insn)
{
  /* Validate the pseudo-instruction, checking that it's a cons with an interned
     symbol as its car. */
  if (! JITTERLISP_IS_CONS(insn))
    jitterlisp_compilation_error ("non-cons VM instruction");
  jitterlisp_object car = JITTERLISP_EXP_C_A_CAR(insn);
  if (! JITTERLISP_IS_SYMBOL(car))
    jitterlisp_compilation_error ("non-symbol VM instruction car");
  struct jitterlisp_symbol *s = JITTERLISP_SYMBOL_DECODE(car);
  if (s->name_or_NULL == NULL)
    jitterlisp_compilation_error ("uninterned instruction name");

  /* Now we can be sure that the symbol name is a string. */
  return s->name_or_NULL;
}




/* Code generation: label handling.
 * ************************************************************************** */

/* I use a hash to keep a mapping from each fixnum label as occurring in the
   Lisp representation to a jitterlispvm_label . */

/* Reserve a Lisp label identifier not used by the Lisp compiler (which only
   generates natural label identifiers) for error handling.  This will be
   added to the map only if actually used. */
static const jitterlisp_object
jitterlisp_minus_one = JITTERLISP_FIXNUM_ENCODE(-1);

/* Return the Jitter label associated to the given Lisp label, adding a fresh
   binding to the table if the Lisp label was unknown before. */
static jitterlispvm_label
jitterlisp_lookup_label (struct jitterlispvm_program *p,
                         struct jitter_hash_table *map,
                         jitterlisp_object lisp_tagged_label)
{
  if (! JITTERLISP_IS_FIXNUM(lisp_tagged_label))
    jitterlisp_compilation_error ("non-fixnum label");
  jitter_int lisp_label = JITTERLISP_FIXNUM_DECODE(lisp_tagged_label);

  /* If the Lisp label is already known return its value; otherwise add a
     new binding, and return the new value. */
  jitterlispvm_label res;
  if (jitter_word_hash_table_has (map, lisp_label))
    res = jitter_word_hash_table_get (map, lisp_label).fixnum;
  else
    {
      res = jitterlispvm_fresh_label (p);
      union jitter_word value = { .fixnum = res };
      jitter_word_hash_table_add (map, lisp_label, value);
    }
  return res;
}

/* Return the one VM label used for handling errors, binding it in the table (to
   a negative Lisp identifier) if this is its first use for the current VM
   program.  See the initialization code below for rationale. */
static jitterlispvm_label
jitterlisp_error_label (struct jitterlispvm_program *p,
                        struct jitter_hash_table *map)
{
  return jitterlisp_lookup_label (p, map, jitterlisp_minus_one);
}

/* Return non-false iff the given map has an error label.  If that is not the
   case then we don't need error handling in the current VM program. */
static bool
jitterlisp_has_error_label (const struct jitter_hash_table *map)
{
  jitter_int key = JITTERLISP_FIXNUM_DECODE(jitterlisp_minus_one);
  return jitter_word_hash_table_has (map, key);
}




/* Code generation: extracting label pseudo-instruction from the Lisp side.
 * ************************************************************************** */

/* FIXME: handle GC roots. */

/* A helper function for jitterlisp_translate_instruction .  Translate a
   primitive use of the given primitive in the given program. */
static void
jitterlisp_translate_primitive (struct jitterlispvm_program *p,
                                struct jitter_hash_table *map,
                                const struct jitterlisp_primitive *pri)
{
  const char *name = pri->name;
  jitter_uint in_arity = pri->in_arity;

  bool is_cons_setter = false;
  bool can_fail = true;
  if (! strcmp (name, "null?"))
    {
      can_fail = false;
      jitterlispvm_append_instruction_name (p, "primitive-nullp");
    }
  else if (! strcmp (name, "non-null?"))
    {
      can_fail = false;
      jitterlispvm_append_instruction_name (p, "primitive-non-nullp");
    }
  else if (! strcmp (name, "nothing?"))
    {
      can_fail = false;
      jitterlispvm_append_instruction_name (p, "primitive-nothingp");
    }
  else if (! strcmp (name, "cons?"))
    {
      can_fail = false;
      jitterlispvm_append_instruction_name (p, "primitive-consp");
    }
  else if (! strcmp (name, "non-cons?"))
    {
      can_fail = false;
      jitterlispvm_append_instruction_name (p, "primitive-non-consp");
    }
  else if (! strcmp (name, "symbol?"))
    {
      can_fail = false;
      jitterlispvm_append_instruction_name (p, "primitive-symbolp");
    }
  else if (! strcmp (name, "non-symbol?"))
    {
      can_fail = false;
      jitterlispvm_append_instruction_name (p, "primitive-non-symbolp");
    }
  else if (! strcmp (name, "fixnum?"))
    {
      can_fail = false;
      jitterlispvm_append_instruction_name (p, "primitive-fixnump");
    }
  else if (! strcmp (name, "character?"))
    {
      can_fail = false;
      jitterlispvm_append_instruction_name (p, "primitive-characterp");
    }
  else if (! strcmp (name, "unique?"))
    {
      can_fail = false;
      jitterlispvm_append_instruction_name (p, "primitive-uniquep");
    }
  else if (! strcmp (name, "1+"))
    jitterlispvm_append_instruction_name (p, "primitive-one-plus");
  else if (! strcmp (name, "1-"))
    jitterlispvm_append_instruction_name (p, "primitive-one-minus");
  else if (! strcmp (name, "2*"))
    jitterlispvm_append_instruction_name (p, "primitive-two-times");
  else if (! strcmp (name, "2/"))
    jitterlispvm_append_instruction_name (p, "primitive-two-divided");
  else if (! strcmp (name, "2quotient"))
    jitterlispvm_append_instruction_name (p, "primitive-two-quotient");
  else if (! strcmp (name, "2remainder"))
    jitterlispvm_append_instruction_name (p, "primitive-two-remainder");
  else if (! strcmp (name, "primordial-+"))
    jitterlispvm_append_instruction_name (p, "primitive-primordial-plus");
  else if (! strcmp (name, "primordial--"))
    jitterlispvm_append_instruction_name (p, "primitive-primordial-minus");
  else if (! strcmp (name, "primordial-*"))
    jitterlispvm_append_instruction_name (p, "primitive-primordial-times");
  else if (! strcmp (name, "primordial-/-unsafe"))
    jitterlispvm_append_instruction_name (p, "primitive-primordial-divided-unsafe");
  else if (! strcmp (name, "eq?"))
    {
      can_fail = false;
      jitterlispvm_append_instruction_name (p, "primitive-eqp");
    }
  else if (! strcmp (name, "not-eq?"))
    {
      can_fail = false;
      jitterlispvm_append_instruction_name (p, "primitive-not-eqp");
    }
  else if (! strcmp (name, "zero?"))
    jitterlispvm_append_instruction_name (p, "primitive-zerop");
  else if (! strcmp (name, "non-zero?"))
    jitterlispvm_append_instruction_name (p, "primitive-non-zerop");
  else if (! strcmp (name, "positive?"))
    jitterlispvm_append_instruction_name (p, "primitive-positivep");
  else if (! strcmp (name, "non-positive?"))
    jitterlispvm_append_instruction_name (p, "primitive-non-positivep");
  else if (! strcmp (name, "negative?"))
    jitterlispvm_append_instruction_name (p, "primitive-negativep");
  else if (! strcmp (name, "non-negative?"))
    jitterlispvm_append_instruction_name (p, "primitive-non-negativep");
  else if (! strcmp (name, "<"))
    jitterlispvm_append_instruction_name (p, "primitive-lessp");
  else if (! strcmp (name, ">"))
    jitterlispvm_append_instruction_name (p, "primitive-greaterp");
  else if (! strcmp (name, "<="))
    jitterlispvm_append_instruction_name (p, "primitive-not-greaterp");
  else if (! strcmp (name, ">="))
    jitterlispvm_append_instruction_name (p, "primitive-not-lessp");
  else if (! strcmp (name, "="))
    jitterlispvm_append_instruction_name (p, "primitive-fixnum-eqp");
  else if (! strcmp (name, "<>"))
    jitterlispvm_append_instruction_name (p, "primitive-fixnum-not-eqp");
  else if (! strcmp (name, "cons")
           || ! strcmp (name, "boolean-canonicalize")
           || ! strcmp (name, "not")
           )
    {
      /* These primitives cannot fail and have a regular name. */
      can_fail = false;
      char *full_name = jitter_xmalloc (strlen (name) + 100);
      sprintf (full_name, "primitive-%s", name);
      jitterlispvm_append_instruction_name (p, full_name);
      free (full_name);
    }
  else if (! strcmp (name, "car")
           || ! strcmp (name, "cdr")
           || ! strcmp (name, "negate")
           || ! strcmp (name, "quotient-unsafe")
           || ! strcmp (name, "remainder-unsafe")
           )
    {
      /* These primitives have a regular name but can fail, so we can't treat
         them along with the previous group.  I guess I should factor. */
      char *full_name = jitter_xmalloc (strlen (name) + 100);
      sprintf (full_name, "primitive-%s", name);
      jitterlispvm_append_instruction_name (p, full_name);
      free (full_name);
    }
  else if (! strcmp (name, "set-car!")
           || ! strcmp (name, "set-cdr!"))
    {
      /* These two are compiled in a special way, using one VM instruction which
         doesn't drop any of the two operands, followed by a nip instruction to
         remove the undertop and a copy-from-literal instruction to set the TOS
         to #<nothing>.  The last two instructions can often be rewritten away. */
      is_cons_setter = true;
      if (! strcmp (name, "set-car!"))
        JITTERLISPVM_APPEND_INSTRUCTION (p, primitive_mset_mcarb_mspecial);
      else
        JITTERLISPVM_APPEND_INSTRUCTION (p, primitive_mset_mcdrb_mspecial);
      jitterlispvm_label error_label = jitterlisp_error_label (p, map);
      jitterlispvm_append_label_parameter (p, error_label);
      JITTERLISPVM_APPEND_INSTRUCTION (p, nip);
      JITTERLISPVM_APPEND_INSTRUCTION (p, copy_mfrom_mliteral);
      jitterlispvm_append_unsigned_literal_parameter (p, JITTERLISP_NOTHING);
    }
  else
    {
      /* Generic fallback case. */
      jitterlisp_primitive_function f = pri->function;
      JITTERLISPVM_APPEND_INSTRUCTION(p, primitive);
      jitterlispvm_append_pointer_literal_parameter (p, f);
      jitterlispvm_append_unsigned_literal_parameter (p, in_arity);
    }

  /* In every case except for the primitives that never fail and set-car! and
     set-cdr! (compiled into more than one VM instructions) we still need to
     append the error label as the last argument.  The primitive instruction
     will jump there used in case some argument type doesn't match.  */
  if (can_fail && ! is_cons_setter)
    {
      jitterlispvm_label error_label = jitterlisp_error_label (p, map);
      jitterlispvm_append_label_parameter (p, error_label);
    }
}

/* Add the given pseudo-instruction translated from its Lisp encoding into the
   pointed Jittery VM program, validating it in the process.  Use the pointed
   map associating Lisp labels to Jitter labels. */
static void
jitterlisp_translate_instruction (struct jitterlispvm_program *p,
                                  struct jitter_hash_table *map,
                                  jitterlisp_object insn)
{
  const char *name = jitterlisp_instruction_name (insn);
  jitterlisp_object label_arg, literal_arg, symbol_arg;
  jitter_uint register_arg, in_arity_arg, depth_arg;
  struct jitterlisp_primitive *primitive_arg;
  jitterlispvm_label label;
  if (! strcmp (name, "label"))
    {
      JITTERLISP_ARGUMENT(label_arg, insn, FIXNUM);
      JITTERLISP_NO_MORE_ARGUMENTS(insn);
      label = jitterlisp_lookup_label (p, map, label_arg);
      jitterlispvm_append_label (p, label);
    }
  else if (! strcmp (name, "drop")
           || ! strcmp (name, "nip")
           || ! strcmp (name, "procedure-prolog")
           || ! strcmp (name, "return"))
    {
      JITTERLISP_NO_MORE_ARGUMENTS(insn);
      jitterlispvm_append_instruction_name (p, name);
    }
  else if (! strcmp (name, "push-literal"))
    {
      JITTERLISP_ARGUMENT(literal_arg, insn, ANYTHING);
      JITTERLISP_NO_MORE_ARGUMENTS(insn);
      JITTERLISPVM_APPEND_INSTRUCTION(p, push_mliteral);
      jitterlispvm_append_unsigned_literal_parameter (p, literal_arg);
    }
  else if (! strcmp (name, "push-global")
           || ! strcmp (name, "pop-to-global"))
    {
      JITTERLISP_ARGUMENT(symbol_arg, insn, SYMBOL);
      JITTERLISP_NO_MORE_ARGUMENTS(insn);
      jitterlispvm_append_instruction_name (p, name);
      jitterlispvm_append_unsigned_literal_parameter (p, symbol_arg);
    }
  else if (! strcmp (name, "push-register")
           || ! strcmp (name, "pop-to-register")
           || ! strcmp (name, "copy-to-register")
           || ! strcmp (name, "save-register")
           || ! strcmp (name, "restore-register"))
    {
      JITTERLISP_ARGUMENT_DECODED(register_arg, insn, FIXNUM);
      JITTERLISP_NO_MORE_ARGUMENTS(insn);
      jitterlispvm_append_instruction_name (p, name);
      JITTERLISPVM_APPEND_REGISTER_PARAMETER (p, r, register_arg);
    }
  else if (! strcmp (name, "at-depth-to-register"))
    {
      JITTERLISP_ARGUMENT_DECODED(depth_arg, insn, FIXNUM);
      JITTERLISP_ARGUMENT_DECODED(register_arg, insn, FIXNUM);
      JITTERLISP_NO_MORE_ARGUMENTS(insn);
      jitterlispvm_append_instruction_name (p, name);
      jitterlispvm_append_unsigned_literal_parameter (p, depth_arg);
      JITTERLISPVM_APPEND_REGISTER_PARAMETER (p, r, register_arg);
    }
  else if (! strcmp (name, "check-in-arity"))
    {
      JITTERLISP_ARGUMENT_DECODED(in_arity_arg, insn, FIXNUM);
      JITTERLISP_NO_MORE_ARGUMENTS(insn);
      jitterlispvm_append_instruction_name (p, name);
      jitterlispvm_append_unsigned_literal_parameter (p, in_arity_arg);
      jitterlispvm_label error_label = jitterlisp_error_label (p, map);
      jitterlispvm_append_label_parameter (p, error_label);
    }
  else if (! strcmp (name, "check-global-defined"))
    {
      JITTERLISP_ARGUMENT(symbol_arg, insn, SYMBOL);
      JITTERLISP_NO_MORE_ARGUMENTS(insn);
      jitterlispvm_append_instruction_name (p, name);
      jitterlispvm_append_unsigned_literal_parameter (p, symbol_arg);
      jitterlispvm_label error_label = jitterlisp_error_label (p, map);
      jitterlispvm_append_label_parameter (p, error_label);
    }
  else if (! strcmp (name, "branch")
           || ! strcmp (name, "branch-if-true")
           || ! strcmp (name, "branch-if-false"))
    {
      JITTERLISP_ARGUMENT(label_arg, insn, FIXNUM);
      JITTERLISP_NO_MORE_ARGUMENTS(insn);
      label = jitterlisp_lookup_label (p, map, label_arg);
      jitterlispvm_append_instruction_name (p, name);
      jitterlispvm_append_label_parameter (p, label);
    }
  else if (! strcmp (name, "check-closure"))
    {
      JITTERLISP_NO_MORE_ARGUMENTS(insn);
      jitterlispvm_append_instruction_name (p, name);
      jitterlispvm_label error_label = jitterlisp_error_label (p, map);
      jitterlispvm_append_label_parameter (p, error_label);
    }
  else if (! strcmp (name, "primitive"))
    {
      JITTERLISP_ARGUMENT_DECODED(primitive_arg, insn, PRIMITIVE);
      JITTERLISP_NO_MORE_ARGUMENTS(insn);

      /* Primitives are important: use a helper procedure optimizing the
         critical cases. */
      jitterlisp_translate_primitive (p, map, primitive_arg);
    }
  else if (! strcmp (name, "call")
           || ! strcmp (name, "tail-call")
           || ! strcmp (name, "call-compiled")
           || ! strcmp (name, "tail-call-compiled"))
    {
      JITTERLISP_ARGUMENT_DECODED(in_arity_arg, insn, FIXNUM);
      JITTERLISP_NO_MORE_ARGUMENTS(insn);
      jitterlispvm_append_instruction_name (p, name);
      jitterlispvm_append_unsigned_literal_parameter (p, in_arity_arg);
    }
  else
    {
      printf ("About the Lisp instruction ");
      jitterlisp_print_to_stream (stdout, insn);
      printf ("\n");
      printf ("WARNING: unknown instruction named %s:\n", name);
      //printf ("About an instruction named %s:\n", name);
      //jitterlisp_compilation_error ("unknown instruction");
    }

  //JITTERLISPVM_APPEND_INSTRUCTION(p, debug);
}

/* Add each pseudo-instruction translated from its Lisp encoding into the
   pointed Jittery VM program, validating it in the process.  Use the pointed
   map associating Lisp labels to Jitter labels.
   The given Lisp instructions are assumed to be a Lisp list, as this function
   is called after a primitive has already validated its arguments. */
static void
jitterlisp_translate_instructions (struct jitterlispvm_program *p,
                                   struct jitter_hash_table *map,
                                   jitterlisp_object insns)
{
  jitterlisp_object rest;
  for (rest = insns;
       rest != JITTERLISP_EMPTY_LIST;
       rest = JITTERLISP_EXP_C_A_CDR(rest))
    jitterlisp_translate_instruction (p, map, JITTERLISP_EXP_C_A_CAR(rest));

  /* Generate the error-handling routine after the other instructions, if we
     have used it in the translation of the instructions before; if we haven't
     then the current VM program doesn't need error handling. */
  if (jitterlisp_has_error_label (map))
    {
      jitterlispvm_append_label (p, jitterlisp_error_label (p, map));
      JITTERLISPVM_APPEND_INSTRUCTION(p, fail);
    }
}

/* Given the Lisp encoding of the VM program, generate the Jittery version. */
static struct jitterlispvm_program *
jitterlisp_generate_jittery (jitterlisp_object code_as_sexpression)
{
  struct jitterlispvm_program *res
    = jitterlispvm_make_program ();

  /* Make the label hash table. */
  struct jitter_hash_table map;
  jitter_hash_initialize (& map);

  /* Translate instructions. */
  jitterlisp_translate_instructions (res, & map, code_as_sexpression);

  /* We're done with the hash table. */
  jitter_word_hash_finalize (& map, jitter_do_nothing_on_word);

  /* Specialize the VM program immediately.  We want it to be ready to be
     executed at any time. */
  jitterlispvm_specialize_program (res);
  return res;
}

void
jitterlisp_compile (struct jitterlisp_closure *c,
                    jitter_int in_arity,
                    jitterlisp_object nonlocals,
                    jitterlisp_object code_as_sexpression)
{
  c->kind = jitterlisp_closure_type_compiled;
  struct jitterlisp_compiled_closure * const cc = & c->compiled;
  cc->nonlocals = nonlocals;
  cc->nonlocal_no = jitterlisp_length (nonlocals);
  // FIXME: don't leak.
  cc->vm_program = jitterlisp_generate_jittery (code_as_sexpression);
  cc->first_program_point = JITTERLISPVM_PROGRAM_BEGINNING(cc->vm_program);
  /*
    printf ("codegen: first program point at %p\n", cc->first_program_point);
  */
}




/* Code generation debugging.
 * ************************************************************************** */

void
jitterlisp_print_compiled_closure (struct jitterlisp_compiled_closure *cc)
{
  jitterlispvm_print_program (stdout, cc->vm_program);
}

/* Disassemble native code from the given compiled closure. */
void
jitterlisp_disassemble_compiled_closure (struct jitterlisp_compiled_closure *cc)
{
  struct jitter_program *p = cc->vm_program;
  if (jitterlisp_settings.cross_disassembler)
    jitterlispvm_disassemble_program (p, true, JITTER_CROSS_OBJDUMP, NULL);
  else
    jitterlispvm_disassemble_program (p, true, JITTER_OBJDUMP, NULL);
}
