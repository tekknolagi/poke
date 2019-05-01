/* Jitter: generator implementation.

   Copyright (C) 2017, 2018, 2019 Luca Saiu
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


#include <config.h>

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h> /* for unlink and rmdir */
#include <string.h>
#include <sys/stat.h> /* For mkdir and permission bit macros. */
#include <errno.h>

#include <xalloc.h>
#include <gl_array_list.h>
#include <gl_xlist.h>

#include "jitterc-generate.h"
#include "jitterc-utility.h"
#include "jitterc-vm.h"
#include "jitterc-rewrite.h"
#include "jitterc-mangle.h"

#include <jitter/jitter-fatal.h>
#include <jitter/jitter-string.h>

/* This contains fixed opcodes for special specialized instructions. */
#include <jitter/jitter-specialize.h>


/* Preliminary definitions.
 * ************************************************************************** */

/* VM prefixes as occurring in templates and generated files before
   replacement. */
#define INPUT_LOWER_CASE_PREFIX  "vmprefix"
#define INPUT_UPPER_CASE_PREFIX  "VMPREFIX"

/* The temporary subdirectory basename.  This directory contains a temporary
   copy of the generated files, to be copied to the actual output directory
   at the end if everything succeeds. */
#define TMP "tmp-vm-generator"




/* Code generation machinery.
 * ************************************************************************** */

/* Perform a fprintf call exiting fatally in case of error. */
#define EMIT_TO(...)                                      \
  do                                                      \
    {                                                     \
      if (fprintf (__VA_ARGS__) < 0)                      \
        jitter_fatal ("could not write to output file");  \
    }                                                     \
  while (false)

/* Perform a fprintf call exiting fatally in case of error and using as output
   the variable f, jitter it is in the current scope. */
#define EMIT(...)         \
  EMIT_TO(f, __VA_ARGS__)

static FILE *
jitterc_fopen_pathname (const char *pathname, const char *opentype)
{
  FILE *res = fopen (pathname, opentype);
  if (res == NULL)
    jitter_fatal ("could not open file %s in mode %s", pathname, opentype);
  return res;
}

/* Return a new file stream open for reading, or fail fatally in case of error;
   the file full pathname is given as an argument. */
static FILE *
jitterc_fopen_r_pathname (const char *pathname)
{
  return jitterc_fopen_pathname (pathname, "r");
}

/* Return a new file stream open for writing, or fail fatally in case of error;
   the file full pathname is given as an argument. */
static FILE *
jitterc_fopen_w_pathname (const char *pathname)
{
  return jitterc_fopen_pathname (pathname, "w");
}

/* Return a new file stream open for appending, or fail fatally in case of
   error; the file full pathname is given as an argument. */
static FILE *
jitterc_fopen_a_pathname (const char *pathname)
{
  return jitterc_fopen_pathname (pathname, "a");
}

/* Return a malloc-allocated string containing the full pathname of the given
   basename, within the temporary directory of the pointed VM. */
__attribute__ ((returns_nonnull, nonnull (1, 2)))
static char *
jitterc_pathname (const struct jitterc_vm *vm,
                  const char *basename)
{
  size_t pathname_size
    = strlen (vm->tmp_directory) + 1 + strlen (basename) + 1;
  char *pathname = xmalloc (pathname_size);
  sprintf (pathname, "%s/%s", vm->tmp_directory, basename);
  return pathname;
}

/* Return a new file stream open for appending in the temporary directory of the
   pointed VM, or fail fatally in case of error; the file basename is given as
   an argument. */
static FILE *
jitterc_fopen_a_basename (const struct jitterc_vm *vm,
                          const char *basename)
{
  char *pathname = jitterc_pathname (vm, basename);
  FILE *res = jitterc_fopen_a_pathname (pathname);
  free (pathname);
  return res;
}

/* Return a new file stream open for writing; the given file basename is
   appended to the temporary output directory of the VM whose pointer is given,
   and the file basename is copied to the written_file_names list in the VM, so
   that the written file can be moved later to the actual output directory if
   everything succeeds. */
static FILE *
jitterc_fopen_w_and_remember_basename (const struct jitterc_vm *vm,
                                       const char *basename)
{
  gl_list_add_last (vm->written_file_names,
                    jitter_clone_string (basename));
  char *pathname = jitterc_pathname (vm, basename);
  FILE *res = jitterc_fopen_w_pathname (pathname);
  free (pathname);
  return res;
}

/* Close the given stream, failing fatally in case of errors.  This works for
   both input and output streams.*/
void
jitterc_fclose (FILE *f)
{
  if (fclose (f) != 0)
    jitter_fatal ("could not close file");
}

/* Make the given directory, exiting fatally in case of error. */
static void
jitterc_mkdir (const char *pathname)
{
  if (mkdir (pathname, S_IRWXU) != 0 && errno != EEXIST)
    jitter_fatal ("could not make directory %s", (pathname));
}

/* From this point on any use of fopen, fclose or fprintf or mkdir is almost
   certainly a mistake.  Poison the identifiers in question. */
#pragma GCC poison \
  mkdir fclose fprintf fopen

#define FOR_LIST(VARIABLE, COMMA, LIST)                                  \
  for (VARIABLE = 0                                                      \
         , COMMA = ((VARIABLE == gl_list_size (LIST) - 1) ? "" : ",");   \
       VARIABLE < gl_list_size (LIST);                                   \
       VARIABLE ++                                                       \
         , COMMA = ((VARIABLE == gl_list_size (LIST) - 1) ? "" : ","))

#define FOR_LIST_DOWN(VARIABLE, COMMA, LIST)      \
  for (VARIABLE = gl_list_size (LIST) - 1         \
         , COMMA = ((VARIABLE == 0) ? "" : ",");  \
       VARIABLE >= 0;                             \
       VARIABLE --                                \
         , COMMA = ((VARIABLE == 0) ? "" : ","))



/* Simple source generation. */

/* Emit user-specified code.  FIXME: use this everywhere and find some way of
   handling #line directives out of user code. */
static void
jitterc_emit_user_c_code_to_stream (const struct jitterc_vm *vm,
                                    FILE *f,
                                    const char *code,
                                    char *description)
{
  EMIT("/* User-specified code, %s part: beginning. */\n", description);
  EMIT("%s", code);
  EMIT("\n/* User-specified code, %s part: end */\n", description);
  EMIT("\n");
}

/* Emit user-specified code.  FIXME: use this everywhere and find some way of
   handling #line directives out of user code. */
static void
jitterc_emit_user_c_code (const struct jitterc_vm *vm,
                          const char *file_basename,
                          const char *code,
                          char *description)
{
  FILE *f = jitterc_fopen_a_basename (vm, file_basename);
  jitterc_emit_user_c_code_to_stream (vm, f, code, description);
  jitterc_fclose (f);
}

/* Emit a macro definition holding the VM name.  This is mostly useful for
   vm-main.c . */
static void
jitterc_emit_vm_name (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm.h");
  EMIT("/* A nice human-readable name for this VM. */\n");
  char *name = vm->name;
  if (name == NULL)
    {
      name = jitter_clone_string (vm->lower_case_prefix);
      if (strlen (name) > 0)
        name [0] = toupper (name [0]);
    }
  EMIT("#define VMPREFIX_VM_NAME JITTER_STRINGIFY(%s)\n", name);
  EMIT("\n");
  jitterc_fclose (f);
}

/* Emit the early part of the user-specified code for the header. */
static void
jitterc_emit_early_header_c (const struct jitterc_vm *vm)
{
  jitterc_emit_user_c_code (vm, "vm.h", vm->early_header_c_code,
                            "early header");
}

/* Emit the late part of the user-specified code for the header. */
static void
jitterc_emit_late_header_c (const struct jitterc_vm *vm)
{
  jitterc_emit_user_c_code (vm, "vm.h", vm->late_header_c_code,
                            "late header");
}

/* Emit the late part of the user-specified code for the header. */
static void
jitterc_emit_header_closing (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm.h");
  EMIT("\n");
  EMIT("/* Close the multiple-inclusion guard opened in the template. */\n");
  EMIT("#endif // #ifndef VMPREFIX_VM_H_\n");
  jitterc_fclose (f);
}

static void
jitterc_emit_meta_instructions_h (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm.h");
  EMIT("#ifndef VMPREFIX_META_INSTRUCTIONS_H_\n#define VMPREFIX_META_INSTRUCTIONS_H_\n\n");
  EMIT("enum vmprefix_meta_instruction_id\n");
  EMIT("  {\n");
  int i; char *comma;
  FOR_LIST(i, comma, vm->instructions)
    EMIT("    vmprefix_meta_instruction_id_%s = %i%s\n",
         (((const struct jitterc_instruction*)
           gl_list_get_at (vm->instructions, i))->mangled_name),
         i,
         comma);
  EMIT("  };\n");
  EMIT("\n#define VMPREFIX_META_INSTRUCTION_NO %i\n\n", i);

  EMIT("/* The longest meta-instruction name length, not mangled, without\n");
  EMIT("   counting the final '\\0' character. */\n");
  EMIT("#define VMPREFIX_MAX_META_INSTRUCTION_NAME_LENGTH %u\n\n",
       (unsigned) vm->max_instruction_name_length);
  EMIT("#endif // #ifndef VMPREFIX_META_INSTRUCTIONS_H_\n");
  jitterc_fclose (f);
}

/* Emit user C code for literal argument printing.  This is called at the
   appropriate time to be visible in the relevant part of the generated code
   without forward-declarations. */
static void
jitterc_emit_printer_c  (const struct jitterc_vm *vm)
{
  jitterc_emit_user_c_code (vm, "vm1.c", vm->printer_c_code,
                            "printer");
}

/* The generated file also includes what was in the old generated
   vm/meta-instruction-parameter-types.c . */
static void
jitterc_emit_meta_instructions (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm1.c");
  int i; char *comma __attribute__ ((unused));
  EMIT("//#include <stdbool.h>\n\n");
  EMIT("//#include <jitter/jitter.h>\n");
  EMIT("//#include <jitter/jitter-instruction.h>\n\n");
  EMIT("//#include \"vmprefix-meta-instructions.h\"\n");
  EMIT("\n");
  EMIT("// FIXME: comment.\n");
  EMIT("struct jitter_hash_table\n");
  EMIT("vmprefix_meta_instruction_hash;\n");
  EMIT("\n\n");
  FOR_LIST(i, comma, vm->instructions)
    {
      const struct jitterc_instruction *in
        = (const struct jitterc_instruction*)
          (gl_list_get_at (vm->instructions, i));

      int in_arity = gl_list_size (in->arguments);
      if (in_arity == 0)
        continue;

      EMIT("static const /*FIXME: use enum jitterc_instruction_argument_kind instead*/struct jitter_meta_instruction_parameter_type vmprefix_%s_meta_instruction_parameter_types [%i] =\n  {",
           in->mangled_name,
           in_arity);
      int j; char *inner_comma;
      FOR_LIST(j, inner_comma, in->arguments)
      //for (j = 0; j < in_arity; j ++)
        {
          //if (j == (in_arity - 1))
          //  inner_comma = "";
          /* FIXME: this is a temporary compatibility hack.  I should do away
             with enum jitter_meta_instruction_parameter_type and use enum
             jitterc_instruction_argument_kind instead .*/
          const struct jitterc_instruction_argument *arg
            = (const struct jitterc_instruction_argument *)
              (gl_list_get_at (in->arguments, j));
          char *kind;
          bool includes_register = false;
          switch ((int) arg->kind)
            {
            case   jitterc_instruction_argument_kind_register:
              kind = "jitter_meta_instruction_parameter_kind_register";
              includes_register = true;
              break;
            case   jitterc_instruction_argument_kind_literal:
              kind = "jitter_meta_instruction_parameter_kind_literal_fixnum";
              break;
            case   jitterc_instruction_argument_kind_label:
            case   jitterc_instruction_argument_kind_fast_label:
              kind = "jitter_meta_instruction_parameter_kind_literal_label";
              break;
            case   jitterc_instruction_argument_kind_register
                 | jitterc_instruction_argument_kind_literal:
              kind = "jitter_meta_instruction_parameter_kind_register_or_literal_fixnum";
              includes_register = true;
              break;
            case   jitterc_instruction_argument_kind_register
                 | jitterc_instruction_argument_kind_label:
              kind = "jitter_meta_instruction_parameter_kind_register_or_literal_label";
              includes_register = true;
              break;
            case   jitterc_instruction_argument_kind_literal
                 | jitterc_instruction_argument_kind_label:
              kind = "jitter_meta_instruction_parameter_kind_literal_fixnum_or_literal_label";
              includes_register = false;
              break;
            case   jitterc_instruction_argument_kind_register
                 | jitterc_instruction_argument_kind_literal
                 | jitterc_instruction_argument_kind_label:
              kind = "jitter_meta_instruction_parameter_kind_register_or_literal_fixnum_or_literal_label";
              includes_register = true;
              break;
            default:
              jitter_fatal ("Unsupported enum jitterc_instruction_argument_kind case: %i\n",
                             (int) arg->kind);
            }

          /* Get the name of the literal printer, of a default if none was
             given. */
          char *literal_printer_name;
          if (arg->c_literal_printer_name != NULL)
            literal_printer_name = arg->c_literal_printer_name;
          else
            literal_printer_name = "jitter_default_literal_parameter_printer";

          if (includes_register)
            EMIT(" { %s, & vmprefix_register_class_%c, %s }%s", kind,
                 arg->register_class_character, literal_printer_name,
                 inner_comma);
          else
            EMIT(" { %s, NULL, %s }%s", kind, literal_printer_name,
                 inner_comma);
        }
      EMIT(" };\n\n");
    }
  //EMIT("  };\n");
  EMIT("\n");
  EMIT("const struct jitter_meta_instruction\n");
  EMIT("vmprefix_meta_instructions [VMPREFIX_META_INSTRUCTION_NO]\n");
  EMIT("  = {\n");
  FOR_LIST(i, comma, vm->instructions)
    {
      const struct jitterc_instruction *in
        = (const struct jitterc_instruction*)
          (gl_list_get_at (vm->instructions, i));
      int in_arity = gl_list_size (in->arguments);
      EMIT("      { %i, \"%s\", %i, %s, %s, %s, ",
           i, in->name, in_arity,
           ((in->callerness == jitterc_callerness_caller)
            ? "true" : "false"),
           ((in->calleeness == jitterc_calleeness_callee)
            ? "true" : "false"),
           ((in->relocatability == jitterc_relocatability_relocatable)
            ? "true /* FIXME: this may be wrong with replacements. */"
            : "false  /* FIXME: this may be wrong with replacements. */"));
      if (in_arity == 0)
        EMIT("NULL }%s\n", comma);
      else
        EMIT("vmprefix_%s_meta_instruction_parameter_types }%s\n",
             in->mangled_name, comma);
    }
  EMIT("    };\n");
  jitterc_fclose (f);
}

static void
jitterc_emit_specialized_instructions_h (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm.h");
  EMIT("#ifndef VMPREFIX_SPECIALIZED_INSTRUCTIONS_H_\n#define VMPREFIX_SPECIALIZED_INSTRUCTIONS_H_\n\n");
  EMIT("enum vmprefix_specialized_instruction_opcode\n");
  EMIT("  {\n");
  int i; char *comma;
  FOR_LIST(i, comma, vm->specialized_instructions)
    EMIT("    vmprefix_specialized_instruction_opcode_%s = %i%s\n",
         (((const struct jitterc_specialized_instruction*)
           gl_list_get_at (vm->specialized_instructions, i))->mangled_name),
         i,
         comma);
  EMIT("  };\n");
  EMIT("\n#define VMPREFIX_SPECIALIZED_INSTRUCTION_NO %i\n\n", i);
  EMIT("#endif // #ifndef VMPREFIX_SPECIALIZED_INSTRUCTIONS_H_\n");
  jitterc_fclose (f);
}

static void
jitterc_emit_specialized_instruction_names (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm1.c");
  EMIT("//#include \"vmprefix-specialized-instructions.h\"\n");
  EMIT("\n");
  EMIT("const char * const\n");
  EMIT("vmprefix_specialized_instruction_names [VMPREFIX_SPECIALIZED_INSTRUCTION_NO]\n");
  EMIT("  = {\n");
  int i; char *comma;
  FOR_LIST(i, comma, vm->specialized_instructions)
    EMIT("      \"%s\"%s\n",
         (((const struct jitterc_specialized_instruction*)
           gl_list_get_at (vm->specialized_instructions, i))->name),
         comma);
  EMIT("    };\n");
  jitterc_fclose (f);
}

static void
jitterc_emit_specialized_instruction_residual_arities
   (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm1.c");
  EMIT("// #include <stdlib.h>\n\n");
  EMIT("// #include \"vmprefix-specialized-instructions.h\"\n");
  EMIT("const size_t\n");
  EMIT("vmprefix_specialized_instruction_residual_arities [VMPREFIX_SPECIALIZED_INSTRUCTION_NO]\n");
  EMIT("  = {\n");
  int i; char *comma;
  FOR_LIST(i, comma, vm->specialized_instructions)
    {
      const struct jitterc_specialized_instruction* sins
        = ((const struct jitterc_specialized_instruction*)
           gl_list_get_at (vm->specialized_instructions, i));
      size_t residual_arity = 0;
      int j;
      for (j = 0; j < gl_list_size (sins->specialized_arguments); j ++)
        if (((const struct jitterc_specialized_argument*)
             gl_list_get_at (sins->specialized_arguments, j))->residual)
          residual_arity ++;
      EMIT("      %i%s /* %s */\n", (int)residual_arity, comma, sins->name);
    }
  EMIT("    };\n");
  jitterc_fclose (f);
}

/* This factors the implementation of
   jitterc_emit_specialized_instruction_label_bitmasks and
   jitterc_emit_specialized_instruction_fast_label_bitmasks . */
static void
jitterc_emit_specialized_instruction_label_bitmasks_possibly_fast
   (const struct jitterc_vm *vm,
    bool fast)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm1.c");
  if (fast)
    EMIT("#ifdef JITTER_HAVE_PATCH_IN\n");
  EMIT("const unsigned long // FIXME: shall I use a shorter type when possible?\n");
  EMIT("vmprefix_specialized_instruction_%slabel_bitmasks [VMPREFIX_SPECIALIZED_INSTRUCTION_NO]\n",
       fast ? "fast_" : "");
  EMIT("  = {\n");
  EMIT("      /* It's important that !BEGINBASICBLOCK has a zero here: it does not need residual patching. */\n");
  int i; char *comma;
  FOR_LIST(i, comma, vm->specialized_instructions)
    {
      const struct jitterc_specialized_instruction* sins
        = ((const struct jitterc_specialized_instruction*)
           gl_list_get_at (vm->specialized_instructions, i));
      EMIT("      0");
      int j;
      int residual_counter = 0;
      for (j = 0; j < gl_list_size (sins->specialized_arguments); j ++)
        {
          const struct jitterc_specialized_argument *sarg
            = ((const struct jitterc_specialized_argument*)
               gl_list_get_at (sins->specialized_arguments, j));
          if (! sarg->residual)
            continue;
          bool has_a_one;
          if (fast)
            has_a_one
              = sarg->kind == jitterc_instruction_argument_kind_fast_label;
          else
            has_a_one
              =     sarg->kind == jitterc_instruction_argument_kind_label
                 || sarg->kind == jitterc_instruction_argument_kind_fast_label;
          if (has_a_one)
            EMIT(" | (1UL << %i)", residual_counter);

          residual_counter ++;
        }
      EMIT("%s /* %s */\n", comma, sins->name);
    }
  EMIT("    };\n");
  if (fast)
    EMIT("#endif // #ifdef JITTER_HAVE_PATCH_IN\n\n");
  jitterc_fclose (f);
}
static void
jitterc_emit_specialized_instruction_label_bitmasks (const struct jitterc_vm *vm)
{
  jitterc_emit_specialized_instruction_label_bitmasks_possibly_fast (vm, false);
}
static void
jitterc_emit_specialized_instruction_fast_label_bitmasks (const struct jitterc_vm *vm)
{
  jitterc_emit_specialized_instruction_label_bitmasks_possibly_fast (vm, true);
}




/* Code generation utility.
 * ************************************************************************** */

/* Emit a #line directive referring the Jitter VM specification source file,
   unless #line-generation was disabled. */
static void
jitterc_emit_hash_line (FILE *f, const struct jitterc_vm *vm, int line_no)
{
  if (vm->generate_line)                                                   \
    EMIT("#line %i \"%s\"\n", line_no, vm->source_file_name);
}




/* More complex code generation.
 * ************************************************************************** */

static void
jitterc_emit_specialized_instruction_relocatables
   (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm1.c");

  EMIT("// FIXME: I may want to conditionalize this.\n");
  EMIT("const bool\n");
  EMIT("vmprefix_specialized_instruction_relocatables [VMPREFIX_SPECIALIZED_INSTRUCTION_NO]\n");
  EMIT("  = {\n");
  int i; char *comma;
  FOR_LIST(i, comma, vm->specialized_instructions)
    {
      const struct jitterc_specialized_instruction* sins
        = ((const struct jitterc_specialized_instruction*)
           gl_list_get_at (vm->specialized_instructions, i));
      bool relocatable = (sins->relocatability
                          == jitterc_relocatability_relocatable);
      EMIT("      %s%s // %s\n",
           relocatable ? "true" : "false",
           comma,
           sins->name);
    }
  EMIT("    };\n\n");
  jitterc_fclose (f);
}

/* Emit the definition of a bool vector, one element per specialized instruction,
   each element being true iff the corresponding instruction is a caller/callee. */
static void
jitterc_emit_specialized_instruction_callers_or_callees
  (const struct jitterc_vm *vm,
   bool callers)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm1.c");
  char *name = callers ? "callers" : "callees";

  EMIT("// FIXME: this is not currently accessed, and in fact may be useless.\n");
  EMIT("const bool\n");
  EMIT("vmprefix_specialized_instruction_%s [VMPREFIX_SPECIALIZED_INSTRUCTION_NO]\n",
       name);
  EMIT("  = {\n");
  int i; char *comma;
  FOR_LIST(i, comma, vm->specialized_instructions)
    {
      const struct jitterc_specialized_instruction* sins
        = ((const struct jitterc_specialized_instruction*)
           gl_list_get_at (vm->specialized_instructions, i));
      bool value;
      if (sins->instruction == NULL)
        value = false;
      else if (callers)
        value = (sins->instruction->callerness == jitterc_callerness_caller);
      else
        value = (sins->instruction->calleeness == jitterc_calleeness_callee);
      EMIT("      %s%s // %s\n", value ? "true" : "false", comma, sins->name);
    }
  EMIT("    };\n\n");
  jitterc_fclose (f);
}

/* Emit the definition of a bool vector, one element per specialized instruction,
   each element being true iff the corresponding instruction is a caller. */
static void
jitterc_emit_specialized_instruction_callers
   (const struct jitterc_vm *vm)
{
  jitterc_emit_specialized_instruction_callers_or_callees (vm, true);
}

/* Emit the definition of a bool vector, one element per specialized instruction,
   each element being true iff the corresponding instruction is a callee. */
static void
jitterc_emit_specialized_instruction_callees
   (const struct jitterc_vm *vm)
{
  jitterc_emit_specialized_instruction_callers_or_callees (vm, false);
}

/* Emit the worst-case defect table for the pointed VM. */
static void
jitterc_emit_worst_case_defect_table (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm1.c");
  EMIT("#ifdef JITTER_HAVE_PATCH_IN\n");
  EMIT("/* Worst-case defect table. */\n");
  EMIT("const jitter_uint\n");
  EMIT("vmprefix_worst_case_defect_table [] =\n");
  EMIT("  {\n");
  int i; char *comma;
  FOR_LIST(i, comma, vm->specialized_instructions)
    {
      const struct jitterc_specialized_instruction* sins
        = ((const struct jitterc_specialized_instruction*)
           gl_list_get_at (vm->specialized_instructions, i));
      if (sins->has_as_replacement == NULL)
        EMIT("    vmprefix_specialized_instruction_opcode_%s%s /* NOT potentially defective. */\n",
             sins->mangled_name, comma);
      else
        EMIT("    /*vmprefix_specialized_instruction_opcode__eINVALID*/vmprefix_specialized_instruction_opcode_%s%s /* POTENTIALLY DEFECTIVE. */\n",
             sins->has_as_replacement->mangled_name, comma);
    }
  EMIT("  };\n");
  EMIT("#endif // #ifdef JITTER_HAVE_PATCH_IN\n");
  EMIT("\n\n");
  jitterc_fclose (f);
}




/* Rewriter generation.
 * ************************************************************************** */

/* Emit code for the pointed template expression to the pointed stream for the
   pointed VM; the generated C code evaluates to a literal if
   evaluate_to_literal is true, otherwise it evaluates to an instruction
   argument.  This is to be used both in rules bodies for instantiating template
   expression and within a condition for evaluating a rule guard -- but the
   output is not a condition. */
static void
jitterc_emit_rewrite_rule_template_expression
   (FILE *f,
    const struct jitterc_vm *vm,
    const struct jitterc_template_expression *te,
    bool evaluate_to_literal)
{
  /* Generate a #line directive for the template expression, indepdendently from
     its shape. */
  jitterc_emit_hash_line(f, vm, te->line_no);

  /* Generate different code according to the AST case. */
  switch (te->case_)
    {
    case jitterc_instruction_argument_expression_case_boolean_constant:
      if (! evaluate_to_literal)
        jitter_fatal ("template expression: unexpected boolean constant");
      EMIT("      %s\n", te->constant.fixnum ? "true" : "false");
      break;

    case jitterc_instruction_argument_expression_case_fixnum_constant:
      if (! evaluate_to_literal)
        jitter_fatal ("template expression: unexpected fixnum constant");
      EMIT("      %"JITTER_PRIi"\n", te->constant.fixnum);
      break;

    case jitterc_instruction_argument_expression_case_placeholder:
      if (evaluate_to_literal)
        EMIT("      JITTER_RULE_LITERAL_FIELD(JITTER_PLACEHOLDER_NAME(%s))\n",
             te->placeholder);
      else
        EMIT("      JITTER_PLACEHOLDER_NAME(%s)\n", te->placeholder);
      break;

    case jitterc_instruction_argument_expression_case_operation:
      {
        int i; char *comma;
        EMIT("#warning: operators (here \"%s\") not really implemented yet\n",
             te->operator->name);
        /* Convert the operator name to upper case, to emit it as part of a C
           macro name. */
        char operator_name_uppercase [1000];
        for (i = 0; te->operator->name [i] != '\0'; i ++)
          operator_name_uppercase [i] = toupper (te->operator->name [i]);
        operator_name_uppercase [i] = '\0';

        EMIT("      JITTER_RULE_EXPRESSION_%s(\n", operator_name_uppercase);
        FOR_LIST(i, comma, te->operand_expressions)
          {
            const struct jitterc_template_expression *oe
              = gl_list_get_at (te->operand_expressions, i);
            bool literal_expected = true; // FIXME: compute it for real.
            jitterc_emit_rewrite_rule_template_expression (f, vm, oe,
                                                           literal_expected);
            EMIT("                                %s\n", comma);
          }
        EMIT("                               )\n");
        break;
      }

    default:
      jitter_fatal ("impossible template expression case");
    }
}

/* Emit a condition matching the argument_idx-th argument of the
   instruction_idx-th instruction (both 0-based) of the candidate instructions,
   to the pointed stream for the pointed VM.
   The generated code assumes that the opcode have already been matched, so the
   arity is correct. */
static void
jitterc_emit_rewrite_rule_argument_condition
   (FILE *f,
    const struct jitterc_vm *vm,
    int instruction_idx,
    int argument_idx,
    const struct jitterc_argument_pattern *ap)
{
  /* Generate a #line directive for the argument pattern, indepdendently from
     its shape. */
  jitterc_emit_hash_line(f, vm, ap->line_no);

  /* If the argument pattern specifies a literal, check that it matches.  This
     is a good check to make first, as it will fail frequently.  This check also
     currently includes a check on the kind (required literal), subsumed by the
     kind checks below; anyway GCC should have no problem merging them into one
     conditional, since there are no side effects in between.  */
  if (ap->has_literal)
    EMIT("    JITTER_RULE_CONDITION_MATCH_LITERAL_ARGUMENT(%i, %i, %"
         JITTER_PRIi ")\n",
         instruction_idx, argument_idx, ap->literal.fixnum);

  /* If a kind bitmask is specified, check it. */
  if (ap->kind != jitterc_instruction_argument_kind_unspecified)
    {
      /* A kind is a bitmask, and we accept any one match with a bit.  This
         means that the alternatives are in logical or.  Using C's infix || is
         more convenient here than our non-variadic prefix macros. */
      EMIT("    JITTER_RULE_CONDITION(false\n");
      if (ap->kind & jitterc_instruction_argument_kind_register)
        EMIT("                          || JITTER_RULE_ARGUMENT_IS_A_REGISTER(%i, %i)\n",
             instruction_idx, argument_idx);
      if (ap->kind & jitterc_instruction_argument_kind_literal)
        EMIT("                          || JITTER_RULE_ARGUMENT_IS_A_LITERAL(%i, %i)\n",
             instruction_idx, argument_idx);
      if (ap->kind & jitterc_instruction_argument_kind_label)
        EMIT("                          || JITTER_RULE_ARGUMENT_IS_A_LABEL(%i, %i)\n",
             instruction_idx, argument_idx);
      /* Close the logical or. */
      EMIT("                         )\n");
    }

  /* Match against a placeholder (destructively), if a placeholder name is
     given. */
  if (ap->placeholder_or_NULL != NULL)
    EMIT("    JITTER_RULE_CONDITION_MATCH_PLACEHOLDER(%i, %i, %s)\n",
         instruction_idx, argument_idx, ap->placeholder_or_NULL);
}

/* Generate content for the condition section of the pointed rewrite rule for
   the pointed VM to the pointed stream. */
static void
jitterc_emit_rewrite_rule_condition (FILE *f, const struct jitterc_vm *vm,
                                     const struct jitterc_rule *rule)
{
  int i, j; char *comma __attribute__ ((unused));

  /* Check that the opcode of every candidate instruction matches its
     pattern. */
  EMIT("    /* Check opcodes first: they are likely not to match, and in */\n");
  EMIT("    /* that case we want to fail as early as possible. */\n");
  FOR_LIST(i, comma, rule->in_instruction_patterns)
    {
      const struct jitterc_instruction_pattern *ip
        = gl_list_get_at (rule->in_instruction_patterns, i);
      char *opcode = ip->instruction_name;
      char *mangled_opcode = jitterc_mangle (opcode);
      jitterc_emit_hash_line(f, vm, ip->line_no);
      EMIT("    JITTER_RULE_CONDITION_MATCH_OPCODE(%i, %s)\n",
           i, mangled_opcode);
      free (mangled_opcode);
    }

  /* Then check instruction arguments against the template, binding placeholders
     in the process. */
  EMIT("    /* Check arguments, binding placeholders.  We don't have to worry */\n");
  EMIT("    /* about arity, since the opcodes match if we're here. */\n");
  FOR_LIST(i, comma, rule->in_instruction_patterns)
    {
      const struct jitterc_instruction_pattern *ip
        = gl_list_get_at (rule->in_instruction_patterns, i);
      FOR_LIST(j, comma, ip->argument_patterns)
        {
          const struct jitterc_argument_pattern *ap
            = gl_list_get_at (ip->argument_patterns, j);
          jitterc_emit_rewrite_rule_argument_condition (f, vm, i, j, ap);
        }
    }

  /* Emit the guard at the end, as it may use any placeholder.  If that succeeds
     as well the condition is satisfied. */
  EMIT("    /* Rule guard. */\n");
  EMIT("    JITTER_RULE_CONDITION(\n");
  jitterc_emit_rewrite_rule_template_expression (f, vm, rule->guard, true);
  EMIT("                         )\n");
}

/* Generate code for the pointed instruction template for the pointed VM to the
   pointed stream.  This is to be used within the body section of rules. */
void
jitterc_emit_rewrite_rule_instruction_template
   (FILE *f,
    const struct jitterc_vm *vm,
    const struct jitterc_instruction_template *it)
{
  /* Emit a #line directive for the instruction template. */
  jitterc_emit_hash_line(f, vm, it->line_no);

  /* Emit code to add the opcode. */
  EMIT("    //fprintf (stderr, \"    rewrite: adding instruction %s\\n\");\n",
       it->instruction_name);
  char *mangled_opcode = jitterc_mangle (it->instruction_name);
  EMIT("    JITTER_RULE_APPEND_INSTRUCTION_(%s);\n", mangled_opcode);
  free (mangled_opcode);

  /* Emit code to add the instantiation of every argument template. */
  int i; char *comma __attribute__ ((unused));
  FOR_LIST(i, comma, it->argument_expressions)
    {
      const struct jitterc_template_expression *ae
        = gl_list_get_at (it->argument_expressions, i);
      EMIT("    //fprintf (stderr, \"    instantiating the %i-th argument of %s\\n\");\n",
           i, it->instruction_name);

      // FIXME: make a rewriting-specific macro instead of using
      // jitter_append_parameter_copy ?
      EMIT("    jitter_append_parameter_copy (jitter_routine_p,\n");
      jitterc_emit_rewrite_rule_template_expression (f, vm, ae, false);
      EMIT("                                 );\n");
    }
}

/* Generate code for the pointed rewrite rule for the pointed VM to the pointed
   stream. */
static void
jitterc_emit_rewrite_rule (FILE *f, const struct jitterc_vm *vm,
                           const struct jitterc_rule *rule)
{
  int i; char *comma __attribute__ ((unused));

  EMIT("/* Rewrite rule \"%s\" */\n", rule->name);
  int head_size = gl_list_size (rule->in_instruction_patterns);

  /* Open the rule section. */
  jitterc_emit_hash_line(f, vm, rule->line_no);
  EMIT("JITTER_RULE_BEGIN(%i)\n", head_size);

  /* Emit the placeholder declaration section. */
  EMIT("  JITTER_RULE_BEGIN_PLACEHOLDER_DECLARATIONS\n");
  FOR_LIST(i, comma, rule->placeholders)
    {
      const char *placeholder = gl_list_get_at (rule->placeholders, i);
      EMIT("    JITTER_RULE_DECLARE_PLACEHOLDER_(%s);\n",
           placeholder);
    }
  EMIT("  JITTER_RULE_END_PLACEHOLDER_DECLARATIONS\n");

  /* Emit the placeholder declaration section. */
  EMIT("  JITTER_RULE_BEGIN_CONDITIONS\n");
  jitterc_emit_rewrite_rule_condition (f, vm, rule);
  EMIT("  JITTER_RULE_END_CONDITIONS\n");

  /* Emit the placeholder cloning section. */
  EMIT("  JITTER_RULE_BEGIN_PLACEHOLDER_CLONING\n");
  FOR_LIST(i, comma, rule->placeholders)
    {
      const char *placeholder = gl_list_get_at (rule->placeholders, i);
      EMIT("    JITTER_RULE_CLONE_PLACEHOLDER_(%s);\n",
           placeholder);
    }
  EMIT("  JITTER_RULE_END_PLACEHOLDER_CLONING\n");

  /* Emit the rule body, by compiling instruction templates one after the
     other. */
  EMIT("  JITTER_RULE_BEGIN_BODY\n");
  EMIT("  //fprintf (stderr, \"* The rule %s (line %i) fires...\\n\");\n",
       rule->name, rule->line_no);
  FOR_LIST(i, comma, rule->out_instruction_templates)
    {
      const struct jitterc_instruction_template *it
        = gl_list_get_at (rule->out_instruction_templates, i);
      jitterc_emit_rewrite_rule_instruction_template (f, vm, it);
    }
  EMIT("    //fprintf (stderr, \"  ...End of the rule %s\\n\");\n",
       rule->name);
  EMIT("  JITTER_RULE_END_BODY\n");

  /* Emit the placeholder destruction section. */
  EMIT("  JITTER_RULE_BEGIN_PLACEHOLDER_DESTRUCTION\n");
  FOR_LIST(i, comma, rule->placeholders)
    {
      const char *placeholder = gl_list_get_at (rule->placeholders, i);
      EMIT("    JITTER_RULE_DESTROY_PLACEHOLDER_(%s);\n",
           placeholder);
    }
  EMIT("  JITTER_RULE_END_PLACEHOLDER_DESTRUCTION\n");

  /* Close the rule section, and we're done. */
  EMIT("JITTER_RULE_END\n");
  EMIT("\n");
}

static void
jitterc_emit_rewriter (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm1.c");

  EMIT("void\n");
  EMIT("vmprefix_rewrite (struct jitter_routine *jitter_routine_p)\n");
  EMIT("{\n");

  /* Add the common prolog, defining variables to be visible to the entire
     function body. */
  EMIT("  JITTTER_REWRITE_FUNCTION_PROLOG_;\n");
  EMIT("\n");

  /* Add user-specified code for the rewriter. */
  jitterc_emit_user_c_code_to_stream (vm, f, vm->rewriter_c_code, "rewriter");
  EMIT("\n");

  /* Generate code for the rules. */
  int i; char *comma __attribute__ ((unused));
  FOR_LIST(i, comma, vm->rewrite_rules)
    {
      const struct jitterc_rule *rule
        = ((const struct jitterc_rule*)
           gl_list_get_at (vm->rewrite_rules, i));
      EMIT("//asm volatile (\"\\n# checking %s\");\n", rule->name);
      EMIT("//fprintf (stderr, \"Trying rule %i of %i, \\\"%s\\\" (line %i)\\n\");\n",
           i + 1, (int) gl_list_size (vm->rewrite_rules),
           rule->name,
           rule->line_no);
      jitterc_emit_rewrite_rule (f, vm, rule);
    }
  EMIT("//fprintf (stderr, \"No more rules to try\\n\");\n");

  EMIT("}\n");
  EMIT("\n\n");
  jitterc_fclose (f);
}




/* Specializer generation.
 * ************************************************************************** */

static void
jitterc_emit_specializer_recognizer_prototypes
   (FILE *f,
    const struct jitterc_specialized_instruction_tree* tree)
{
  EMIT("inline static enum vmprefix_specialized_instruction_opcode\n");
  EMIT("vmprefix_recognize_specialized_instruction_%s (struct jitter_parameter ** const ps,\n",
       tree->prefix_mangled_name);
  EMIT("                                               bool enable_fast_literals)\n");
  EMIT("  __attribute__ ((pure));\n");
  int i;
  for (i = 0; i < gl_list_size (tree->children); i ++)
    {
      const struct jitterc_specialized_instruction_tree_child *sarg_and_child
        = ((const struct jitterc_specialized_instruction_tree_child *)
           gl_list_get_at (tree->children, i));
      jitterc_emit_specializer_recognizer_prototypes (f, sarg_and_child->child);
    }
}

static void
jitterc_emit_specializer_recognizers
   (FILE *f,
    const struct jitterc_vm *vm,
    const struct jitterc_specialized_instruction_tree* tree)
{
  EMIT("inline static enum vmprefix_specialized_instruction_opcode\n");
  EMIT("vmprefix_recognize_specialized_instruction_%s (struct jitter_parameter ** const ps,\n",
       tree->prefix_mangled_name);
  EMIT("                                               bool enable_fast_literals)\n");
  EMIT("{\n");
  if (gl_list_size (tree->children) == 0)
    {
      EMIT("  /* The prefix is a full specialized instruction.  We're done recognizing it. */\n");
      EMIT("  return vmprefix_specialized_instruction_opcode_%s;\n",
           tree->specialized_instruction->mangled_name);
      EMIT("}\n\n");
      /* There's nothing more in this subtree. */
      return;
    }

  EMIT("  enum vmprefix_specialized_instruction_opcode res = vmprefix_specialized_instruction_opcode_%s;\n",
       jitterc_mangle ("!INVALID"));
  int i;
  for (i = 0; i < gl_list_size (tree->children); i ++)
    {
      const struct jitterc_specialized_instruction_tree_child *sarg_and_child
        = ((const struct jitterc_specialized_instruction_tree_child *)
           gl_list_get_at (tree->children, i));
      const struct jitterc_specialized_argument *sarg
        = sarg_and_child->specialized_argument;
      const struct jitterc_specialized_instruction_tree *child
        = sarg_and_child->child;
      EMIT("  if ((");
      switch (sarg->kind)
        {
        case jitterc_instruction_argument_kind_literal:
          EMIT("(* ps)->type == jitter_parameter_type_literal");
          if (! sarg->residual)
            // FIXME: this will need generatilzation with more literal types.
            EMIT(" && (* ps)->literal.fixnum == %li && enable_fast_literals",
                 (long) sarg->nonresidual_literal->value.fixnum);
          break;
        case jitterc_instruction_argument_kind_register:
          EMIT("(* ps)->type == jitter_parameter_type_register_id");
          // FIXME: this will need generatilzation with more register classes; or, more likely, not.
          if (! sarg->residual)
            EMIT(" && (* ps)->register_index == %u",
                 (unsigned) sarg->nonresidual_register->index);
          break;
        case jitterc_instruction_argument_kind_label:
        case jitterc_instruction_argument_kind_fast_label:
          EMIT("(* ps)->type == jitter_parameter_type_label");
          if (! sarg->residual)
            jitter_fatal ("non-residual label: this should not happen");
          break;
        default:
          jitter_fatal ("jitterc_emit_specializer_recognizers: unhandled kind");
        }
      EMIT(")\n");
      EMIT("      && (res = vmprefix_recognize_specialized_instruction_%s (ps + 1, enable_fast_literals)))\n",
           child->prefix_mangled_name);
      EMIT("    goto done;\n");
    }
  EMIT("done:\n");
  EMIT("  return res;\n");
  EMIT("}\n\n");

  /* Generate definitions for every subtree. */
  for (i = 0; i < gl_list_size (tree->children); i ++)
    {
      const struct jitterc_specialized_instruction_tree_child *sarg_and_child
        = ((const struct jitterc_specialized_instruction_tree_child *)
           gl_list_get_at (tree->children, i));
      jitterc_emit_specializer_recognizers (f, vm, sarg_and_child->child);
    }
}

static void
jitterc_emit_specializer (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm1.c");
  EMIT("//#include <jitter/jitter-fatal.h>\n");
  EMIT("\n");
  EMIT("//#include <jitter/jitter.h>\n");
  EMIT("//#include <jitter/jitter-instruction.h>\n");
  EMIT("//#include <jitter/jitter-specialize.h>\n");
  EMIT("\n");
  EMIT("//#include \"vmprefix-vm.h\"\n");
  EMIT("//#include \"vmprefix-meta-instructions.h\"\n");
  EMIT("//#include \"vmprefix-specialized-instructions.h\"\n");
  EMIT("\n\n");
  EMIT("/* Recognizer function prototypes. */\n");
  int i; char *comma __attribute__ ((unused));
#define LET_TREE                                               \
  const struct jitterc_specialized_instruction_tree *tree    \
    = ((const struct jitterc_specialized_instruction_tree*)  \
       gl_list_get_at (vm->specialized_instruction_forest, i))
  /* First generate a function prototype per specialized instruction prefix
     recognizer.  I want to declare them all before the first definition, to
     be able to call the functions in any order. */
  FOR_LIST(i, comma, vm->specialized_instruction_forest)
    {
      LET_TREE;
      jitterc_emit_specializer_recognizer_prototypes (f, tree);
    }
  EMIT("\n\n");

  /* Generate the actual definitions. */
  EMIT("/* Recognizer function definitions. */\n");
  FOR_LIST(i, comma, vm->specialized_instruction_forest)
    {
      LET_TREE;
      jitterc_emit_specializer_recognizers (f, vm, tree);
    }
#undef LET_TREE
  EMIT("\n\n");

  /* Generate the main recognizer function. */
  EMIT("/* Recognizer entry point. */\n");
  EMIT("static enum vmprefix_specialized_instruction_opcode\n");
  EMIT("vmprefix_recognize_specialized_instruction (struct jitter_routine *p,\n");
  EMIT("                                            const struct jitter_instruction *ins)\n");
  EMIT("{\n");
  EMIT("  bool fl = ! p->options.slow_literals_only;\n");
  EMIT("  const struct jitter_meta_instruction *mi = ins->meta_instruction;\n");
  EMIT("  switch (mi->id)\n");
  EMIT("    {\n");
  FOR_LIST(i, comma, vm->instructions)
    {
      const struct jitterc_instruction* ins
        = ((const struct jitterc_instruction*)
           gl_list_get_at (vm->instructions, i));
      EMIT("    case vmprefix_meta_instruction_id_%s:\n", ins->mangled_name);
      EMIT("      return vmprefix_recognize_specialized_instruction_%s (ins->parameters, fl);\n",
           ins->mangled_name);
    }
  EMIT("    default:\n");
  EMIT("      jitter_fatal (\"invalid meta-instruction id %%i\", (int)mi->id);\n");
  EMIT("    }\n");
  EMIT("  __builtin_unreachable ();\n");
  EMIT("}\n\n");

  /* Generate the specializer function. */
  EMIT("/* Specializer entry point: the only non-static function here. */\n");
  EMIT("int\n");
  EMIT("vmprefix_specialize_instruction (struct jitter_routine *p,\n");
  EMIT("                                 const struct jitter_instruction *ins)\n");
  EMIT("{\n");
  EMIT("  enum vmprefix_specialized_instruction_opcode opcode\n");
  EMIT("    = vmprefix_recognize_specialized_instruction (p, ins);\n");
  EMIT("  if (opcode == vmprefix_specialized_instruction_opcode_%s)\n",
       jitterc_mangle ("!INVALID"));
  EMIT("    jitter_fatal (\"specialization failed\");\n");
  EMIT("\n");
  EMIT("#ifdef JITTER_HAVE_PATCH_IN\n");
  EMIT("  /* Replace the opcode with its non-defective counterpart. */\n");
  EMIT("  opcode = vmprefix_defect_table [opcode];\n");
  EMIT("#endif // #ifdef JITTER_HAVE_PATCH_IN\n");
  EMIT("\n");
  EMIT("  jitter_add_specialized_instruction_opcode (p, opcode);\n");
  EMIT("\n");
  EMIT("\n");
  EMIT("  /* FIXME: in the old shell-based generator I grouped specialized instructions by\n");
  EMIT("     their \"residual parameter map\", yielding a switch with a lot of different\n");
  EMIT("     specialized instructions mapping to the same case.  I should redo that here. */\n");
  EMIT("  switch (opcode)\n");
  EMIT("    {\n");
  FOR_LIST(i, comma, vm->specialized_instructions)
    {
      const struct jitterc_specialized_instruction* sins
        = ((const struct jitterc_specialized_instruction*)
           gl_list_get_at (vm->specialized_instructions, i));
      EMIT("    case vmprefix_specialized_instruction_opcode_%s:\n", sins->mangled_name);
      const struct jitterc_instruction* uins = sins->instruction;

      /* Emit code to add residual residual arguments to the threads, except for
         the last arguments of non-relocatable and caller instructions, which
         are special. */
      size_t residual_no = gl_list_size (sins->specialized_arguments);
      bool is_non_relocatable
        = (   uins != NULL
           && sins->relocatability == jitterc_relocatability_non_relocatable);
      bool is_caller
        = (   uins != NULL
           && uins->callerness == jitterc_callerness_caller);
      if (is_non_relocatable || is_caller)
        residual_no --;
      int j;
      for (j = 0; j < residual_no; j ++)
        {
          const struct jitterc_specialized_argument* sarg
            = ((const struct jitterc_specialized_argument*)
               gl_list_get_at (sins->specialized_arguments, j));
          if (! sarg->residual)
            continue;
          switch (sarg->kind)
            {
            case jitterc_instruction_argument_kind_register:
              EMIT("      /* A slow register is passed as a residual literal offset. */");
              EMIT("      jitter_add_specialized_instruction_literal (p, VMPREFIX_SLOW_REGISTER_OFFSET(%c, ins->parameters[%i]->register_index));\n",
                   sarg->unspecialized->register_class_character, j);
              break;
            case jitterc_instruction_argument_kind_literal:
              EMIT("      jitter_add_specialized_instruction_literal (p, ins->parameters[%i]->literal.ufixnum);\n", j);
              break;
            case jitterc_instruction_argument_kind_label:
            case jitterc_instruction_argument_kind_fast_label:
              EMIT("      jitter_add_specialized_instruction_label_index (p, ins->parameters[%i]->label_as_index);\n", j);
              break;
            default:
              jitter_fatal ("jitterc_emit_specializer: unhandled kind");
            }
        }

      /* Add one more residual argument in case of a non-relocatable
         instruction, as a placeholder for the return label. */
      if (is_non_relocatable)
        {
          EMIT("      /* Non-relocatable instruction: make place for the return label,\n");
          EMIT("         whose correct value will be patched in at specialization time. */\n");
          EMIT("      jitter_add_specialized_instruction_literal (p, -1);\n");
        }

      /* Add one more residual argument in case of a caller instruction, as a
         placeholder for the return label. */
      if (is_caller)
        {
          EMIT("      /* Caller instruction: make place for the return address,\n");
          EMIT("         whose correct value will be patched in at specialization time. */\n");
          EMIT("      jitter_add_specialized_instruction_literal (p, -1);\n");
        }

      /* Done handling sins . */
      EMIT("      break;\n\n");
    }
  EMIT("    default:\n");
  EMIT("      jitter_fatal (\"invalid specialized instruction opcode %%i\", (int)opcode);\n");
  EMIT("    }\n");
  EMIT("  return 1; // FIXME: I should rethink this return value.\n");
  EMIT("}\n\n");

  jitterc_fclose (f);
}

static void
jitterc_emit_configuration (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm1.c");
  EMIT("\n\n/* Initialize the VM configuration set by jitterc and CPP options. */\n");
  EMIT("void\n");
  EMIT("vmprefix_initialize_vm_configuration (struct jitter_vm_configuration *c)\n");
  EMIT("{\n");
  EMIT("  c->lower_case_prefix = \"%s\";\n", vm->lower_case_prefix);
  EMIT("  c->upper_case_prefix = \"%s\";\n", vm->upper_case_prefix);

  EMIT("  c->max_fast_register_no_per_class = %i;\n",
       (int) vm->max_fast_register_no_per_class);
  EMIT("  c->max_nonresidual_literal_no = %i;\n",
       (int)vm->max_nonresidual_literal_no);
  EMIT("  c->dispatch = JITTER_DISPATCH_NAME_STRING;\n");
  EMIT("}\n\n");
  jitterc_fclose (f);
}




/* VM stack support.
 * ************************************************************************** */

/* Emit an upper-case conversion of the given lower-case string. */
static void
jitterc_emit_upper_case (FILE *f, const char *lower_case_string)
{
  const char *p;
  for (p = lower_case_string; *p != '\0'; p ++)
    EMIT("%c", toupper (* p));
}

/* Emit the CPP definition of a stack operation, to be called by user code
   within instructions.  The generated macro is a wrapper around a stack
   operation defined in jitter/jitter-stack.h . */
static void
jitterc_emit_stack_operation_definition (FILE *f,
                                         const struct jitterc_stack *stack,
                                         const char *lower_case_operation_name,
                                         size_t arity)
{
  EMIT("/* Wrapper definition of the %s operation for the %s stack \"%s\". */\n",
       lower_case_operation_name,
       ((stack->optimization == jitterc_stack_optimization_tos)
        ? "TOS-optimized" : "non-TOS-optimized"),
       stack->lower_case_name);
  EMIT("#define JITTER_");
  jitterc_emit_upper_case (f, lower_case_operation_name);
  EMIT("_%s(", stack->upper_case_name);
  int i;
  for (i = 0; i < arity; i ++)
    EMIT("x%i%s", i, i != (arity - 1) ? ", ": "");
  const char *optimization_suffix
    = (stack->optimization == jitterc_stack_optimization_tos) ? "TOS" : "NTOS";
  EMIT(")  \\\n");
  EMIT("  JITTER_STACK_%s_", optimization_suffix);
  jitterc_emit_upper_case (f, lower_case_operation_name);
  EMIT("(%s, jitter_state_runtime. , %s", stack->c_element_type,
       stack->lower_case_name);
  for (i = 0; i < arity; i ++)
    EMIT(", x%i", i);
  EMIT(")\n\n");
}

/* Emit CPP definitions for stack operations, for every stack of the pointed
   VM. */
static void
jitterc_emit_stack_operation_definitions (FILE *f, const struct jitterc_vm *vm)
{
  int i; char *comma __attribute__ ((unused));
  FOR_LIST(i, comma, vm->stacks)
    {
      const struct jitterc_stack *stack = gl_list_get_at (vm->stacks, i);
      jitterc_emit_stack_operation_definition (f, stack, "top", 0);
      jitterc_emit_stack_operation_definition (f, stack, "under_top", 0);
      jitterc_emit_stack_operation_definition (f, stack, "at_depth", 1);
      jitterc_emit_stack_operation_definition (f, stack, "at_nonzero_depth", 1);
      jitterc_emit_stack_operation_definition (f, stack, "set_at_depth", 2);
      jitterc_emit_stack_operation_definition (f, stack, "set_at_nonzero_depth", 2);
      jitterc_emit_stack_operation_definition (f, stack, "push_unspecified", 0);
      jitterc_emit_stack_operation_definition (f, stack, "push", 1);

      jitterc_emit_stack_operation_definition (f, stack, "drop", 0);
      jitterc_emit_stack_operation_definition (f, stack, "dup", 0);
      jitterc_emit_stack_operation_definition (f, stack, "swap", 0);
      jitterc_emit_stack_operation_definition (f, stack, "over", 0);
      jitterc_emit_stack_operation_definition (f, stack, "nip", 0);
      jitterc_emit_stack_operation_definition (f, stack, "rot", 0);
      jitterc_emit_stack_operation_definition (f, stack, "mrot", 0);
      jitterc_emit_stack_operation_definition (f, stack, "roll", 1);
      jitterc_emit_stack_operation_definition (f, stack, "mroll", 1);

      jitterc_emit_stack_operation_definition (f, stack, "height", 0);
      jitterc_emit_stack_operation_definition (f, stack, "set_height", 1);

      jitterc_emit_stack_operation_definition (f, stack, "reverse", 1);

      jitterc_emit_stack_operation_definition (f, stack, "unary", 1);
      jitterc_emit_stack_operation_definition (f, stack, "binary", 1); // Not a mistake.
    }
}

/* Emit data structure declarations for VM stack backings.  This generates code
   within the state backing struct containined within the VM state struct. */
static void
jitterc_emit_stack_backing_declarations (FILE *f, const struct jitterc_vm *vm)
{
  EMIT("  /* Stack backing data structures. */\n");
  int i; char *comma __attribute__ ((unused));
  FOR_LIST(i, comma, vm->stacks)
    {
      const struct jitterc_stack *stack = gl_list_get_at (vm->stacks, i);

      EMIT("  struct jitter_stack_backing jitter_stack_%s_backing;\n", stack->lower_case_name);
    }
  EMIT("\n");
}

/* Emit data structure declarations for VM stacks.  This generates code within
   the state runtime struct containined within the VM state struct. */
static void
jitterc_emit_stack_runtime_declarations (FILE *f, const struct jitterc_vm *vm)
{
  EMIT("  /* Stack runtime data structures. */\n");
  int i; char *comma __attribute__ ((unused));
  FOR_LIST(i, comma, vm->stacks)
    {
      const struct jitterc_stack *stack = gl_list_get_at (vm->stacks, i);

      const char *optimization_suffix
        = ((stack->optimization == jitterc_stack_optimization_tos)
           ? "TOS" : "NTOS");
      EMIT("  JITTER_STACK_%s_DECLARATION(%s, %s);\n",
           optimization_suffix,
           stack->c_element_type,
           stack->lower_case_name);
    }
  EMIT("\n");
}

/* Emit initialization code for VM stacks.  This generates code within the VM
   state initialization function. */
static void
jitterc_emit_stack_initializations (FILE *f, const struct jitterc_vm *vm)
{
  EMIT("  /* Initialize stack backings and stack runtime data structures, pointing\n");
  EMIT("     to memory from the backings. */\n");
  int i; char *comma __attribute__ ((unused));
  FOR_LIST(i, comma, vm->stacks)
    {
      const struct jitterc_stack *stack = gl_list_get_at (vm->stacks, i);

      const char *optimization_lower_case_suffix
        = ((stack->optimization == jitterc_stack_optimization_tos)
           ? "tos" : "ntos");
      const char *optimization_upper_case_suffix
        = ((stack->optimization == jitterc_stack_optimization_tos)
           ? "TOS" : "NTOS");
      const unsigned long element_no = 4096; // FIXME: make this customizable.

      EMIT("  jitter_stack_initialize_%s_backing(& jitter_state_backing->jitter_stack_%s_backing,\n",
           optimization_lower_case_suffix, stack->lower_case_name);
      EMIT("                                      sizeof (%s),\n",
           stack->c_element_type);
      EMIT("                                      %lu);\n", element_no);
      EMIT("  JITTER_STACK_%s_INITIALIZE(%s, jitter_state_runtime-> ,\n",
           optimization_upper_case_suffix, stack->c_element_type);
      EMIT("                              %s, jitter_state_backing->jitter_stack_%s_backing);\n",
           stack->lower_case_name, stack->lower_case_name);
    }
  EMIT("\n");
}

/* Emit finalization code for VM stacks.  This generates code within the VM
   state finalization function. */
static void
jitterc_emit_stack_finalizations (FILE *f, const struct jitterc_vm *vm)
{
  EMIT("  /* Finalize stack backings -- There is no need to finalize the stack\n");
  EMIT("     runtime data structures, as they hold no heap data of their own. */\n");
  int i; char *comma __attribute__ ((unused));
  FOR_LIST(i, comma, vm->stacks)
    {
      const struct jitterc_stack *stack = gl_list_get_at (vm->stacks, i);

      EMIT("  jitter_stack_finalize_backing (& jitter_state_backing->jitter_stack_%s_backing);\n",
           stack->lower_case_name);
    }
  EMIT("\n");
}





/* VM state.
 * ************************************************************************** */

static void
jitterc_emit_state_h (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm.h");
  EMIT("#ifndef VMPREFIX_STATE_H_\n");
  EMIT("#define VMPREFIX_STATE_H_\n\n");
  EMIT("//#include <config.h>\n");
  EMIT("//#include <jitter/jitter.h>\n\n");

  /* Insert C code from the user.  This is supposed to come in before the struct
     definition. */
  EMIT("/* Early C code from the user for the state definition. */\n");
  EMIT("%s", vm->state_early_c_code);
  EMIT("/* End of the early C code from the user for the state definition. */\n\n");

  EMIT("/* The VM state backing. */\n");
  EMIT("struct vmprefix_state_backing\n");
  EMIT("{\n");
  EMIT("  /* The Array.  This initial pointer is kept in the backing, since it is\n");
  EMIT("     not normally needed at run time.  By subtracting JITTER_ARRAY_BIAS from\n");
  EMIT("     it (as a pointer to char) we get the base pointer. */\n");
  EMIT("  char *jitter_array;\n");
  EMIT("\n");
  EMIT("  /* How many slow registers per class the Array can hold, without being\n");
  EMIT("     reallocated.  This number is always the same for evey class. */\n");
  EMIT("  size_t jitter_slow_register_no_per_class;\n");
  EMIT("\n");

  /* Emit declarations for stack backing data structures. */
  jitterc_emit_stack_backing_declarations (f, vm);

  EMIT("  /* State backing fields added in C by the user. */\n");
  EMIT("%s", vm->state_backing_struct_c_code);
  EMIT("\n  /* End of the state backing fields added in C by the user. */\n");
  EMIT("};\n");
  EMIT("\n");

  EMIT("/* The VM state runtime data structure, using memory from the VM state backing. */\n");
  EMIT("struct vmprefix_state_runtime\n");
  EMIT("{\n");
  EMIT("#if    defined(JITTER_DISPATCH_SWITCH)                   \\\n");
  EMIT("    || defined(JITTER_DISPATCH_DIRECT_THREADING)         \\\n");
  EMIT("    || defined(JITTER_DISPATCH_MINIMAL_THREADING)        \\\n");
  EMIT("    || (   defined(JITTER_DISPATCH_NO_THREADING)         \\\n");
  EMIT("        && ! defined(JITTER_MACHINE_SUPPORTS_PROCEDURE))\n");
  EMIT("  /* A link register for branch-and-link operations.  This field must *not*\n");
  EMIT("     be accessed from user code, as it may not exist on all dispatching\n");
  EMIT("     models.  It is only used internally for JITTER_PROCEDURE_PROLOG. */\n");
  EMIT("  const union jitter_word *_jitter_link;\n");
  EMIT("#endif\n");
  EMIT("\n");
  EMIT("  /* With recent GCC versions (as of Summer 2017) the *last* declared fields\n");
  EMIT("     are the most likely to be allocated in registers; this is why VM registers\n");
  EMIT("     are in reverse order here.  The first few fast registers will be the \"fastest\"\n");
  EMIT("     ones, allocated in hardware registers; they may be followed by other fast\n");
  EMIT("     fast allocated on the stack at known offsets, with intermediate performance; then\n");
  EMIT("     come the slow registers.  In critical code the users should prefer a register with as\n");
  EMIT("     small an index as possible for best performance. */\n");
  int i; char *comma __attribute__ ((unused));
  FOR_LIST(i, comma, vm->register_classes)
    {
      const struct jitterc_register_class *c
        = (gl_list_get_at (vm->register_classes, i));
      int j;
      for (j = c->fast_register_no - 1; j >= 0; j --)
        EMIT("  vmprefix_register_%c jitter_fast_register_%c_%i;\n",
             c->character, c->character, j);
    }
  EMIT("\n");

  /* Emit declarations for stack runtime data structures. */
  jitterc_emit_stack_runtime_declarations (f, vm);

  /* Insert C code from the user.  This is supposed to contain struct fields. */
  EMIT("  /* State runtime fields added in C by the user. */\n");
  EMIT("%s", vm->state_runtime_struct_c_code);
  EMIT("\n  /* End of the state runtime fields added in C by the user. */\n");

  EMIT("};\n");
  EMIT("\n");

  EMIT("/* A struct holding both the backing and the runtime part of the VM state. */\n");
  EMIT("struct vmprefix_state\n");
  EMIT("{\n");
  EMIT("  /* Each state data structure contains its backing. */\n");
  EMIT("  struct vmprefix_state_backing vmprefix_state_backing;\n");
  EMIT("\n");
  EMIT("  /* Each state data structure contains its runtime data structures,\n");
  EMIT("     to be allocated to registers as long as possible, and using\n");
  EMIT("     memory from the backing. */\n");
  EMIT("  struct vmprefix_state_runtime vmprefix_state_runtime;\n");
  EMIT("};\n");

  EMIT("#endif // #ifndef VMPREFIX_STATE_H_\n");
  jitterc_fclose (f);
}

static void
jitterc_emit_state (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm1.c");

  EMIT("void\n");
  EMIT("vmprefix_state_initialize (struct vmprefix_state *jitter_state)\n");
  EMIT("{\n");
  EMIT("  struct vmprefix_state_backing * const jitter_state_backing\n");
  EMIT("    __attribute__ ((unused))\n");
  EMIT("    = & jitter_state->vmprefix_state_backing;\n");
  EMIT("  struct vmprefix_state_runtime * const jitter_state_runtime\n");
  EMIT("    __attribute__ ((unused))\n");
  EMIT("    = & jitter_state->vmprefix_state_runtime;\n");
  EMIT("\n");

  EMIT("  /* Initialize the Array. */\n");
  EMIT("  jitter_state_backing->jitter_slow_register_no_per_class = 0; // FIXME: raise?\n");
  EMIT("  jitter_state_backing->jitter_array\n");
  EMIT("    = jitter_xmalloc (VMPREFIX_ARRAY_SIZE(jitter_state_backing\n");
  EMIT("                         ->jitter_slow_register_no_per_class));\n");
  EMIT("\n");

  /* Emit initialization for stacks. */
  jitterc_emit_stack_initializations (f, vm);

  EMIT("  /* User code for state initialization. */\n");
  EMIT("%s\n", vm->state_initialization_c_code);
  EMIT("  /* End of the user code for state initialization. */\n");
  EMIT("}\n");
  EMIT("\n");

  EMIT("void\n");
  EMIT("vmprefix_state_finalize (struct vmprefix_state *jitter_state)\n");
  EMIT("{\n");
  EMIT("  struct vmprefix_state_backing * const jitter_state_backing\n");
  EMIT("    __attribute__ ((unused))\n");
  EMIT("    = & jitter_state->vmprefix_state_backing;\n");
  EMIT("  struct vmprefix_state_runtime * const jitter_state_runtime\n");
  EMIT("    __attribute__ ((unused))\n");
  EMIT("    = & jitter_state->vmprefix_state_runtime;\n");
  EMIT("\n");

  /* Emit finalization for stacks. */
  jitterc_emit_stack_finalizations (f, vm);
  EMIT("\n");

  EMIT("  /* User code for state finalization. */\n");
  EMIT("%s\n", vm->state_finalization_c_code);
  EMIT("  /* End of the user code for state finalization. */\n");
  EMIT("\n");
  EMIT("  /* Finalize the Array. */\n");
  EMIT("  free ((void *) jitter_state_backing->jitter_array);\n");
  EMIT("\n");
  EMIT("}\n\n");

  jitterc_fclose (f);
}




/* VM register classes.
 * ************************************************************************** */

/* Emit header code for register classes. */
static void
jitterc_emit_register_classes_h (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm.h");

  EMIT("\n");
  EMIT("/* For each register class define the register type, a unique index, and the\n");
  EMIT("   number of fast registers.  Indices are useful for computing slow register\n");
  EMIT("   offsets.  For each register class declare a global register class\n");
  EMIT("   descriptor, convenient to use when generating unspecialized instructions\n");
  EMIT("   from the C API.*/\n");
  int i; char *comma __attribute__ ((unused));
  FOR_LIST(i, comma, vm->register_classes)
    {
      const struct jitterc_register_class *c
        = (gl_list_get_at (vm->register_classes, i));
      EMIT("typedef\n%s vmprefix_register_%c;\n", c->c_type, c->character);
      EMIT("#define VMPREFIX_REGISTER_%c_CLASS_ID %i\n", c->character, i);
      EMIT("#define VMPREFIX_REGISTER_%c_FAST_REGISTER_NO %i\n", c->character,
           (int) c->fast_register_no);
      EMIT("extern const struct jitter_register_class\n");
      EMIT("vmprefix_register_class_%c;\n", c->character);
    }
  EMIT("\n");

  EMIT("/* How many register classes we have. */\n");
  EMIT("#define VMPREFIX_REGISTER_CLASS_NO  %i\n",
       (int) gl_list_size (vm->register_classes));
  EMIT("\n");

  EMIT("/* A union large enough to hold a register of any class, or a machine word. */\n");
  EMIT("union vmprefix_any_register\n");
  EMIT("{\n");
  EMIT("  /* In any case the union must be at least as large as a machine word. */\n");
  EMIT("  jitter_int jitter_unused_field;\n\n");
  FOR_LIST(i, comma, vm->register_classes)
    {
      const struct jitterc_register_class *c
        = (gl_list_get_at (vm->register_classes, i));
      EMIT("  vmprefix_register_%c %c /* A %c-class register */;\n",
           c->character, c->character, c->character);
    }
  EMIT("};\n");
  EMIT("\n");

  EMIT("/* An enumeration of all vmprefix register classes. */\n");
  EMIT("enum vmprefix_register_class_id\n");
  EMIT("  {\n");
  FOR_LIST(i, comma, vm->register_classes)
    {
      const struct jitterc_register_class *c
        = (gl_list_get_at (vm->register_classes, i));
      EMIT("    vmprefix_register_class_id_%c = VMPREFIX_REGISTER_%c_CLASS_ID,\n",
           c->character, c->character);
    }
  EMIT("\n");
  EMIT("    /* The number of register class ids, not valid as a class id itself. */\n");
  EMIT("    vmprefix_register_class_id_no = VMPREFIX_REGISTER_CLASS_NO\n");
  EMIT("  };\n");

  EMIT("\n");
  jitterc_fclose (f);
}

/* Emit implementation code for register classes. */
static void
jitterc_emit_register_classes (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm1.c");
  EMIT("\n");

  /* Emit definitions for global register class descriptors. */
  int i; char *comma;
  FOR_LIST(i, comma, vm->register_classes)
    {
      const struct jitterc_register_class *c
        = (gl_list_get_at (vm->register_classes, i));
      EMIT("/* The register class descriptor for %c registers. */\n", c->character);
      EMIT("const struct jitter_register_class\n");
      EMIT("vmprefix_register_class_%c\n", c->character);
      EMIT("  = {\n");
      EMIT("      '%c',\n", c->character);
      EMIT("      vmprefix_register_class_id_%c,\n", c->character);
      EMIT("      VMPREFIX_REGISTER_%c_FAST_REGISTER_NO\n", c->character);
      EMIT("    };\n\n");
    }
  EMIT("\n");

  /* Group register class descriptors into a constant pointer constant array. */
  EMIT("/* A pointer to every existing register class descriptor. */\n");
  EMIT("const struct jitter_register_class * const\n");
  EMIT("vmprefix_regiter_classes []\n");
  EMIT("  = {\n");
  FOR_LIST(i, comma, vm->register_classes)
    {
      const struct jitterc_register_class *c
        = (gl_list_get_at (vm->register_classes, i));
      EMIT("      & vmprefix_register_class_%c%s\n", c->character, comma);
    }
  EMIT("    };\n");
  EMIT("\n");

  /* Emit the definition of vmprefix_register_class_character_to_register_class
     , returning a pointer to one of the structures above. */
  EMIT("const struct jitter_register_class *\n");
  EMIT("vmprefix_register_class_character_to_register_class (char c)\n");
  EMIT("{\n");
  EMIT("  switch (c)\n");
  EMIT("    {\n");
  FOR_LIST(i, comma, vm->register_classes)
    {
      const struct jitterc_register_class *c
        = (gl_list_get_at (vm->register_classes, i));
      EMIT("    case '%c': return & vmprefix_register_class_%c;\n",
           c->character, c->character);
    }
  EMIT("    default:  return NULL;\n");
  EMIT("    }\n");
  EMIT("}\n");
  EMIT("\n");

  jitterc_fclose (f);
}




/* VM register access.
 * ************************************************************************** */

/* Emit macro definitions for accessing slow registers.  These go into the
   VM header, since they are useful both in specialization, for computing
   offsets from the base, and in the executor. */
static void
jitterc_emit_register_access_macros_h (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm.h");

  EMIT("/* How many residuals we can have at most.  This, with some dispatching models,\n");
  EMIT("   is needed to compute a slow register offset from the base. */\n");
  EMIT("#define VMPREFIX_MAX_RESIDUAL_ARITY  %i\n\n", (int)vm->max_residual_arity);

  jitterc_fclose (f);
}




/* Executor generation.
 * ************************************************************************** */

/* Emit macro definitions for accessing registers as lvalues, available to user
   meta-instruction code (mostly for implied operands) and for the generated
   code as well (for defining instruction register operands).  There is one
   zero-argument macro per fast register, plus another for slow register, having
   the register index as the argument.

   The slow-register access macro yields an array access with an index known at
   compile time.

   FIXME: this will need generalization when I introduce register classes. */
static void
jitterc_emit_executor_register_access_macros (FILE *f,
                                                 const struct jitterc_vm *vm)
{
  EMIT("/* Expand to the i-th fast register as an lvalue.  This is used internally,\n");
  EMIT("   always with a literal index . */\n");
  EMIT("#define JITTER_FAST_REGISTER(class, index)                             \\\n");
  EMIT("  (JITTER_CONCATENATE_FOUR(jitter_state_runtime.jitter_fast_register_, \\\n");
  EMIT("                           class, _, index))\n");
  EMIT("\n");
  int i, j; char *comma __attribute__ ((unused));
  FOR_LIST(i, comma, vm->register_classes)
    {
      const struct jitterc_register_class *c
        = (gl_list_get_at (vm->register_classes, i));
      for (j = 0; j < c->fast_register_no; j ++)
        {
          EMIT("/* Expand to the %i-th fast %c-register as an lvalue. */\n",
               j, c->character);
          EMIT("#define JITTER_FAST_REGISTER_%c_%i JITTER_FAST_REGISTER(%c, %i)\n\n",
               c->character, j, c->character, j);
        }
    }
  EMIT("/* Expand to a slow register lvalue, given an offset in bytes from the base. */\n");
  EMIT("#define JITTER_SLOW_REGISTER_FROM_OFFSET(c, offset)               \\\n");
  EMIT("  (* ((JITTER_CONCATENATE_TWO(vmprefix_register_, c) * restrict)  \\\n");
  EMIT("      (((char *) jitter_array_base) + offset)))\n");
  EMIT("\n");
  const int vmprefix_slow_register_with_access_macro_no = 32;
  EMIT("/* Expand to the i-th register, which must be a slow register, as an lvalue.\n");
  EMIT("   The given index must be a register index counting from 0 and including fast\n");
  EMIT("   regusters as well, if there are any.  For example if an r class had 3 fast\n");
  EMIT("   registers then the first slow register would be %%r3, to be accessed as\n");
  EMIT("   JITTER_SLOW_REGISTER(r, 3).  It would be invalid to access %%r0, %%r1 and\n");
  EMIT("   %%r2 which this macro, as %%r0, %%r1 and %%r2 would be fast. */\n");
  EMIT("#define JITTER_SLOW_REGISTER(c, i)                                          \\\n");
  EMIT("  JITTER_SLOW_REGISTER_FROM_OFFSET(c, VMPREFIX_SLOW_REGISTER_OFFSET(c, i))\n");
  EMIT("\n");
  EMIT("/* It's not possible to have a single macro JITTER_REGISTER taking an index and\n");
  EMIT("   expanding to either a fast or a slow register lvalue, due to CPP conditional\n");
  EMIT("   limitations.  This restriction is unfortunate, but we have to live with it\n");
  EMIT("   as long as we don't switch to a different preprocessor.\n");
  EMIT("   What we can have is a set of zero-argument macros each expanding to a register\n");
  EMIT("   lvalue, for *a limited number* of registers.  Here we define access macros for\n");
  EMIT("   every fast register plus a reasonable number (currently %i) of slow registers,\n",
       vmprefix_slow_register_with_access_macro_no);
  EMIT("   per class. */\n");
  FOR_LIST(i, comma, vm->register_classes)
    {
      const struct jitterc_register_class *c
        = (gl_list_get_at (vm->register_classes, i));
      for (j = 0; j < c->fast_register_no; j ++)
        EMIT("#define JITTER_REGISTER_%c_%-3i  JITTER_FAST_REGISTER(%c, %i)\n",
             c->character, j, c->character, j);
      for (;
           j < (c->fast_register_no
                + vmprefix_slow_register_with_access_macro_no);
           j ++)
        EMIT("#define JITTER_REGISTER_%c_%-3i  JITTER_SLOW_REGISTER(%c, %i)\n",
             c->character, j, c->character, j);
    }
  EMIT("\n");
  EMIT("\n");
}

  static void
jitterc_emit_executor_reserve_registers (FILE *f,
                                            const struct jitterc_vm *vm)
{
  /* We don't need to reserve global registers even with no-threading dispatch
     if this machine needs no residaul arguments at all. */
  if (vm->max_residual_arity == 0)
    return;

  EMIT("#ifdef JITTER_DISPATCH_NO_THREADING\n\n");

  EMIT("/* Reserve the scratch register, if any. */\n");
  EMIT("#ifdef JITTER_SCRATCH_REGISTER\n");
  EMIT("  register union jitter_word\n");
  EMIT("  jitter_residual_argument_scratch_register_variable asm (JITTER_STRINGIFY(JITTER_SCRATCH_REGISTER));\n");
  EMIT("#endif // #ifdef JITTER_SCRATCH_REGISTER\n\n");

  EMIT("/* Reserve The Array base register. */\n");
  EMIT("#ifndef JITTER_BASE_REGISTER\n");
  EMIT("# error \"the machine does not define JITTER_BASE_REGISTER\"\n");
  EMIT("#else\n");
  EMIT("register char * restrict\n");
  EMIT("vmprefix_array_base_register_variable asm (JITTER_STRINGIFY(JITTER_BASE_REGISTER));\n");
  EMIT("#endif // #ifndef JITTER_BASE_REGISTER\n\n");

  EMIT("/* Reserve registers for our %i residual arguments.  If this particular VM doesn't\n",
       (int) vm->max_residual_arity);
  EMIT("   need all of those supported by the assembly machine then reserve only the first\n");
  EMIT("   ones.  If, on the other hand, we need more residual arguments than we have\n");
  EMIT("   available registers, use CPP macros to map the remaining residual arguments\n");
  EMIT("   to memory locations relative to the base, with constant offsets. */\n\n");

  int i;
  for (i = 0; i < vm->max_residual_arity; i ++)
    {
      EMIT("/* Define a macro for the %i-th residual as a register, or as a residual\n", i);
      EMIT("   memory slot.  Also define a macro expanding to inline asm code with\n");
      EMIT("   output constraints on the appropriate register or memory, to let GCC\n");
      EMIT("   know that its value has been changed by unknown code. */\n");
      EMIT("#if (%i < JITTER_RESIDUAL_REGISTER_NO)\n", i);
      EMIT("  register union jitter_word\n");
      EMIT("  jitter_residual_argument_%i_register_variable asm (JITTER_STRINGIFY(JITTER_RESIDUAL_REGISTER_%i));\n", i, i);
      EMIT("# define JITTER_RESIDUAL_ARGUMENT_%i               \\\n", i);
      EMIT("    jitter_residual_argument_%i_register_variable\n", i);
      EMIT("# define JITTER_MARK_RESIDUAL_%i_AS_SET_BY_ASSEMBLY                 \\\n", i);
      EMIT("    JITTER_MARK_REGISTER_AS_SET_BY_ASSEMBLY(jitter_residual_argument_%i_register_variable)\n", i);
      EMIT("#else\n");
      EMIT("# define JITTER_RESIDUAL_ARGUMENT_%i  \\\n", i);
      EMIT("    (* (union jitter_word *)                 \\\n");
      EMIT("       (jitter_array_base + VMPREFIX_RESIDUAL_OFFSET(%i)))\n", i);
      EMIT("# define JITTER_MARK_RESIDUAL_%i_AS_SET_BY_ASSEMBLY                    \\\n", i);
      EMIT("    JITTER_MARK_MEMORY_AS_SET_BY_ASSEMBLY(JITTER_RESIDUAL_ARGUMENT_%i)\n", i);
      EMIT("#endif // #if (%i < JITTER_RESIDUAL_REGISTER_NO)\n\n", i);
    }

  EMIT("/* The global register values we reserve in this compilation unit are\n");
  EMIT("   callee-save: the called function is supposed to save them before\n");
  EMIT("   setting them, and restore them to their previous value before\n");
  EMIT("   returning to the caller.  Of course this is not done automatically\n");
  EMIT("   in this compilation unit, so we have to do it by hand.  Notice that\n");
  EMIT("   every variable allocated to a register by GCC will not use the\n");
  EMIT("   registers we reserved, so we can be sure that, if we save our\n");
  EMIT("   global register variables before setting them for the first time,\n");
  EMIT("   their values will be the ones we want to retain. */\n");
  EMIT("\n");
  EMIT("/* The buffer where I keep the original register values needs to hold\n");
  EMIT("   every residual register, plus possibly the scratch register and the\n");
  EMIT("   residual base; those two registers are not always used, but allocating\n");
  EMIT("   just two words more costs essentially nothing and lets me simplify\n");
  EMIT("   the code a little.  The two words are not written or read unless\n");
  EMIT("   needed. */\n");
  EMIT("#define VMPREFIX_REGISTER_BUFFER_ELEMENT_NO (JITTER_RESIDUAL_REGISTER_NO + 2)\n");
  EMIT("__attribute__ ((noinline, cold))\n");
  EMIT("\n");

  EMIT("static void\n");
  EMIT("vmprefix_save_registers (union jitter_word *buffer)\n");
  EMIT("{\n");
  EMIT("  buffer [0].pointer = (union jitter_word*) vmprefix_array_base_register_variable;\n");
  EMIT("#ifdef JITTER_SCRATCH_REGISTER\n");
  EMIT("  buffer [1] = jitter_residual_argument_scratch_register_variable;\n");
  EMIT("#endif // #ifdef JITTER_SCRATCH_REGISTER\n");
  for (i = 0; i < vm->max_residual_arity; i ++)
    {
      EMIT("#if (%i < JITTER_RESIDUAL_REGISTER_NO)\n", i);
      EMIT("  buffer [%i + 2] = JITTER_RESIDUAL_ARGUMENT_%i;\n", i, i);
      EMIT("#endif // #if (%i < JITTER_RESIDUAL_REGISTER_NO)\n", i);
    }
  EMIT("}\n");
  EMIT("\n");

  EMIT("__attribute__ ((noinline, cold))\n");
  EMIT("static void\n");
  EMIT("vmprefix_restore_registers (const union jitter_word *buffer)\n");
  EMIT("{\n");
  EMIT("  vmprefix_array_base_register_variable = (char *) buffer [0].pointer;\n");
  EMIT("#ifdef JITTER_SCRATCH_REGISTER\n");
  EMIT("  jitter_residual_argument_scratch_register_variable = buffer [1];\n");
  EMIT("#endif // #ifdef JITTER_SCRATCH_REGISTER\n");
  for (i = 0; i < vm->max_residual_arity; i ++)
    {
      EMIT("#if (%i < JITTER_RESIDUAL_REGISTER_NO)\n", i);
      EMIT("  JITTER_RESIDUAL_ARGUMENT_%i = buffer [%i + 2];\n", i, i);
      EMIT("#endif // #if (%i < JITTER_RESIDUAL_REGISTER_NO)\n", i);
    }
  EMIT("}\n");
  EMIT("\n");

  EMIT("#endif // #ifdef JITTER_DISPATCH_NO_THREADING\n\n\n");
}

static void
jitterc_emit_executor_global_wrappers
   (FILE *f, const struct jitterc_vm *vm)
{
  EMIT("/* Selectively suppress suprious -Wmaybe-uninitialized .\n");
  EMIT("   The indirect jump hack I use in profiling mode in order to\n");
  EMIT("   have a large gap inside a function introduced by assembler without\n");
  EMIT("   being restricted by jump offset limits (intentionally) tricks GCC\n");
  EMIT("   into believing that the indirect jump may reach any instruction label;\n");
  EMIT("   GCC would then warn that some locals might be used uninitialized,\n");
  EMIT("   by skipping over their initialization.  This however is not possible,\n");
  EMIT("   and I want to selectively silence the warning for the variables in\n");
  EMIT("   question. */\n");
  EMIT("#pragma GCC diagnostic push\n");
  EMIT("#pragma GCC diagnostic ignored \"-Wmaybe-uninitialized\"\n");
  EMIT("\n");
  EMIT("  /* Wrap functions and globals used within VM instructions, if needed.\n");
  EMIT("     This is a trick to keep instructions readable while avoiding PC-relative\n");
  EMIT("     addressing, which would mess up replicated code. */\n");
  EMIT("#ifdef JITTER_REPLICATE\n\n");

  /* When using replication we have to wrap C functions called from VM
     instructions. */
  EMIT("  /* Protect the C globals used in VM instructions so that they are always\n");
  EMIT("     referred thru a pointer (from a register or the stack) set in the\n");
  EMIT("     non-replicated part.  This is necessary on architectures where I can't\n");
  EMIT("     force global references to pass thru a GOT.\n");
  EMIT("     [FIXME: possibly don't do this on architectures that don't need it.] */\n\n");
  int i; char *comma __attribute__ ((unused));
  FOR_LIST(i, comma, vm->wrapped_globals)
    {
      const char *name
        = ((const char*) gl_list_get_at (vm->wrapped_globals, i));
      EMIT("  typeof (%s) * volatile _my_volatile_pointer_to_%s = & %s;\n",
           name, name, name);
      EMIT("  typeof (%s) * const restrict _my_pointer_to_%s __attribute__ ((unused))\n",
           name, name);
      EMIT("     = _my_volatile_pointer_to_%s;\n", name);
      EMIT("# undef %s\n", name);
      EMIT("# define %s (* _my_pointer_to_%s)\n\n", name, name);
    }

  EMIT("  /* Similarly, wrap the C functions used in VM instructions, so that they are\n");
  EMIT("     always called thru a function pointer.  This is necessary on architectures\n");
  EMIT("     where call instructions represent the callee as a PC-relative address.\n");
  EMIT("     Unfortunately C has a special quirky syntax for function pointers, so I\n");
  EMIT("     can't just use the code above.  [FIXME: don't do this on architectures\n");
  EMIT("     that don't need it.] */\n");
  FOR_LIST(i, comma, vm->wrapped_functions)
    {
      const char *name
        = ((const char*) gl_list_get_at (vm->wrapped_functions, i));
      EMIT("  typeof (%s) * volatile _my_volatile_pointer_to_%s = & %s;\n",
           name, name, name);
      EMIT("  typeof (%s) * const _my_%s __attribute__ ((unused))\n",
           name, name);
      EMIT("     = * _my_volatile_pointer_to_%s;\n", name);
      EMIT("# undef %s\n", name);
      EMIT("# define %s _my_%s\n\n", name, name);
    }
  EMIT("/* See the comment above about spurious -Wmaybe-uninitialized warnings. */\n");
  EMIT("#pragma GCC diagnostic pop\n");
  EMIT("#endif // #ifdef JITTER_REPLICATE\n\n");
  EMIT("\n");
}

/* Emit macro definitions (and possibly inline asm statements) for the given
   specialized argument, occurring in the given 0-based position among all the
   arguments and in the given (still 0-based) position among the residual
   arguments of a specialized instruction.  Non-fast-label residuals and
   fast-label "residuals" have independent counters: for example the five sargs
   of a specialized instruction foo_r0_nR_fR_nR_fR would have residual indices:
   - 0 (non-residual)
   - 0 (first residual non-fast-label)
   - 0 (first "residual" fast label)
   - 1 (second residual non-fast-label)
   - 1 (second "residual" fast label). */
static void
jitterc_emit_executor_sarg_definition
   (FILE *f, int index, int residual_index,
    const struct jitterc_specialized_argument* sarg,
    bool have_fast_labels)
{
  switch (sarg->kind)
    {
    case jitterc_instruction_argument_kind_register:
      EMIT("    /* The %ith argument is a %s\n        register. */\n",
           index,
           sarg->residual ? "slow (therefore residual, passed as an offset)"
           : "fast");
      if (sarg->residual)
        {
          EMIT("  /* Define a macro expanding to the slow register offset. */\n");
          EMIT("#if defined(JITTER_DISPATCH_NO_THREADING)\n");
          EMIT("# define JITTER_SLOW_REGISTER_OFFSET%i (JITTER_RESIDUAL_ARGUMENT_%i.fixnum)\n", index, residual_index);
          EMIT("#elif defined (JITTER_DISPATCH_MINIMAL_THREADING)\n");
          EMIT("# define JITTER_SLOW_REGISTER_OFFSET%i ((((union jitter_word*)jitter_ip)[%i]).fixnum)\n", index, residual_index);
          EMIT("#else\n");
          EMIT("# define JITTER_SLOW_REGISTER_OFFSET%i ((((union jitter_word*)jitter_ip)[%i]).fixnum)\n", index, residual_index + 1);
          EMIT("#endif // #if defined(JITTER_DISPATCH_NO_THREADING)\n");

          EMIT("  /* Define a macro expanding to an l-value for the VM register content. */\n");
          EMIT("#   define JITTER_ARG%i  JITTER_SLOW_REGISTER_FROM_OFFSET(%c, JITTER_SLOW_REGISTER_OFFSET%i)\n",
               index, sarg->unspecialized->register_class_character, index);
        }
      else
        EMIT("#   define JITTER_ARG%i  JITTER_FAST_REGISTER(%c, %i)\n",
             index, sarg->unspecialized->register_class_character,
             (int) sarg->nonresidual_register->index);
      break;

    case jitterc_instruction_argument_kind_literal:
      EMIT("    /* The %ith argument is a %sresidual literal. */\n",
           index, sarg->residual ? "" : "non");
      if (sarg->residual)
        goto residual_label_or_literal;
      else
        /* FIXME: this will need generalization with more literal kinds. */
        EMIT("#   define JITTER_ARG%i  ((const union jitter_word){.fixnum = %liL})\n",
             index, (long)sarg->nonresidual_literal->value.fixnum);
      break;

    case jitterc_instruction_argument_kind_label:
      EMIT("    /* The %ith argument is a %sresidual label. */\n",
           index, sarg->residual ? "" : "non");
      if (! sarg->residual)
        jitter_fatal ("nonresidual label");
    residual_label_or_literal:
      EMIT("#if defined(JITTER_DISPATCH_NO_THREADING)\n");
      EMIT("#   define JITTER_ARG%i  JITTER_RESIDUAL_ARGUMENT_%i\n", index, residual_index);
      EMIT("    JITTER_MARK_RESIDUAL_%i_AS_SET_BY_ASSEMBLY;\n", residual_index);
      EMIT("#elif defined (JITTER_REPLICATE)\n");
      EMIT("#   define JITTER_ARG%i  (((union jitter_word*)jitter_ip)[%i])\n", index, residual_index);
      EMIT("#else\n");
      EMIT("#   define JITTER_ARG%i  (((union jitter_word*)jitter_ip)[%i])\n", index, residual_index + 1);
      EMIT("#endif // #if defined(JITTER_DISPATCH_NO_THREADING)\n");
      break;

    case jitterc_instruction_argument_kind_fast_label:
      if (! sarg->residual)
        jitter_fatal ("nonresidual fast label");

      if (have_fast_labels)
        {
          EMIT("    /* The %ith argument is a \"residual\" fast label.  Define its\n", index);
          EMIT("       _ARGF macro as the residual *index* (counting only fast labels, 0-based),\n");
          EMIT("       so that at replication time we know what instruction address to patch in. */\n");
          EMIT("#   define JITTER_ARGF%i %i\n", index, residual_index);
          EMIT("    /* JITTER_ARG%i is intentionally not defined for a fast label. */\n\n", index);
        }
      else
        goto residual_label_or_literal;

      break;

    default:
      jitter_fatal ("jitterc_emit_executor_sarg_definition: invalid argument kind %i", (int) sarg->kind);
    }
  EMIT("#   define JITTER_ARGN%i (JITTER_ARG%i.fixnum)\n", index, index);
  EMIT("#   define JITTER_ARGU%i (JITTER_ARG%i.ufixnum)\n", index, index);
  EMIT("#   define JITTER_ARGP%i (JITTER_ARG%i.pointer)\n", index, index);
  if ((have_fast_labels && sarg->replacement) || ! have_fast_labels)
    {
      EMIT("#   define JITTER_ARGF%i JITTER_ARGP%i\n", index, index);
    }
  /*
  else if (! have_fast_labels)
    EMIT("#   define JITTER_ARGF%i JITTER_ARGP%i\n", index, index);
  */
  EMIT("\n");
}

/* An internal function factoring code run twice in
   jitterc_emit_sarg_definitions .  This emits definitions for user-visible
   argument-access macros, assuming fast labels are enabled or not, as per the
   given argument. */
static void
jitterc_emit_sarg_definitions_internal
   (FILE *f, const struct jitterc_specialized_instruction *sins,
    bool have_fast_labels)
{
  size_t residual_arity = 0;
  size_t residual_label_index = 0;
  int j; char *comma __attribute__ ((unused));
  FOR_LIST(j, comma, sins->specialized_arguments)
    {
      const struct jitterc_specialized_argument* sarg
        = ((const struct jitterc_specialized_argument*)
           gl_list_get_at (sins->specialized_arguments, j));
      if (   have_fast_labels
          && sarg->kind == jitterc_instruction_argument_kind_fast_label)
        {
          jitterc_emit_executor_sarg_definition
            (f, j, residual_label_index, sarg, have_fast_labels);
          residual_label_index ++;
        }
      else
        jitterc_emit_executor_sarg_definition
          (f, j, residual_arity, sarg, have_fast_labels);

      if (   sarg->residual
          && (   sarg->kind != jitterc_instruction_argument_kind_fast_label
              || ! have_fast_labels))
        residual_arity ++;
    }
}

/* Emit definitions for the user-visible argument-access macros of the given
   instruction.  The generated code is conditionalized on fast label support, if
   needed. */
static void
jitterc_emit_sarg_definitions
   (FILE *f, const struct jitterc_specialized_instruction *sins)
{
  /* Emit a conditionalized argument definition for when fast labels are used, ending
     in an #else case for the non-fast-label case. */
  if (sins->instruction->has_fast_labels)
    {
      EMIT("  /* Define argument-access macros for %s . */\n", sins->name);
      EMIT("#ifdef JITTER_HAVE_PATCH_IN\n");
      EMIT("  /* Define argument-access macros assuming that fast branches are enabled. */\n");
      jitterc_emit_sarg_definitions_internal (f, sins, true);
      EMIT("#else\n");
      EMIT("  /* Define argument-access macros assuming that fast branches are disabled. */\n");
    }

  /* Emit the non-fast-label case, which is always there. */
  jitterc_emit_sarg_definitions_internal (f, sins, false);

  /* Close the conditional we opened if there were fast labels. */
  if (sins->instruction->has_fast_labels)
    EMIT("# endif // #ifdef JITTER_HAVE_PATCH_IN\n");
}

static void
jitterc_emit_specialized_instruction_residual_arity_definition
   (FILE *f, const struct jitterc_specialized_instruction *sins)
{
  /* How many residual arguments we have in total, including fast labels if
     any. */
  size_t residual_argument_no = 0;

  /* The number of non-fast-label residual arguments, including both non-fast
     labels and non-label residuals. */
  size_t non_fast_label_residual_argument_no = 0;

  /* Count non-fast-labels and all residuals. */
  int j; char *comma __attribute__ ((unused));
  FOR_LIST(j, comma, sins->specialized_arguments)
    {
      const struct jitterc_specialized_argument* sarg
        = ((const struct jitterc_specialized_argument*)
           gl_list_get_at (sins->specialized_arguments, j));
      if (sarg->residual)
        {
          residual_argument_no ++;
          if (sarg->kind != jitterc_instruction_argument_kind_fast_label)
            non_fast_label_residual_argument_no ++;
        }
    }

  /* Emit the residual arity definition, making it conditional only if needed. */
  if (non_fast_label_residual_argument_no == residual_argument_no)
    {
      EMIT("  /* The residual arity for this instruction does not depend on fast labels. */\n");
      EMIT("  #define JITTER_SPECIALIZED_INSTRUCTION_RESIDUAL_ARITY %i\n",
           (int) non_fast_label_residual_argument_no);
    }
  else
    {
      EMIT("  /* The residual arity varies depending on whether we have fast labels. */\n");
      EMIT("#ifdef JITTER_HAVE_PATCH_IN\n");
      EMIT("  #define JITTER_SPECIALIZED_INSTRUCTION_RESIDUAL_ARITY %i\n",
           (int) non_fast_label_residual_argument_no);
      EMIT("#else\n");
      EMIT("  #define JITTER_SPECIALIZED_INSTRUCTION_RESIDUAL_ARITY %i\n",
           (int) residual_argument_no);
      EMIT("#endif // #ifdef JITTER_HAVE_PATCH_IN\n");
    }
}

/* Names for fast-branching macros, without the prefix "JITTER_BRANCH". */
static const char *
jitter_fast_branch_macros []
  = {
      "",
      "_IF_ZERO",
      "_IF_NONZERO",
      "_IF_POSITIVE",
      "_IF_NONPOSITIVE",
      "_IF_NEGATIVE",
      "_IF_NONNEGATIVE",
      "_IF_EQUAL",
      "_IF_NOTEQUAL",
      "_IF_LESS_SIGNED",
      "_IF_LESS_UNSIGNED",
      "_IF_NOTLESS_SIGNED",
      "_IF_NOTLESS_UNSIGNED",
      "_IF_GREATER_SIGNED",
      "_IF_GREATER_UNSIGNED",
      "_IF_NOTGREATER_SIGNED",
      "_IF_NOTGREATER_UNSIGNED",
      /*
// FIXME: I *think* I only use these internally.
      "_IF_NEVER_UNARY",
      "_IF_ALWAYS_UNARY",
      */
      /* Here the underscore is intentional: even the name with (one) initial
         underscore is defined conditionally, only in caller instructions. */
      "_AND_LINK_INTERNAL"
    };

/* How many strings jitter_fast_branch_macros has. */
static const size_t
jitter_fast_branch_macro_no
= sizeof (jitter_fast_branch_macros) / sizeof (jitter_fast_branch_macros [0]);

/* Emit macro definitions for fast branching.  These are defined in a different
   way for replacement and non-replacement specialized instructions. */
static void
jitterc_emit_executor_fast_branch_definitions
  (FILE *f, const struct jitterc_vm *vm,
   const struct jitterc_specialized_instruction* sins)
{
  bool is_replacement = (sins->is_replacement_of != NULL);

  if (is_replacement)
    EMIT("    /* This specialized instruction is a replacement. */\n");
  else
    EMIT("    /* This specialized instruction is not a replacement. */\n");
  int i;
  for (i = 0; i < jitter_fast_branch_macro_no; i ++)
    {
      const char *macro_name = jitter_fast_branch_macros [i];
      EMIT("#   undef JITTER_BRANCH_FAST%s\n", macro_name);
      if (is_replacement)
        EMIT("#   define JITTER_BRANCH_FAST%s JITTER_BRANCH%s\n", macro_name, macro_name);
      else
        EMIT("#   define JITTER_BRANCH_FAST%s _JITTER_BRANCH_FAST%s\n", macro_name, macro_name);
    }
}

static void
jitterc_emit_executor_ordinary_specialized_instructions
   (FILE *f, const struct jitterc_vm *vm)
{
  int i; char *comma __attribute__ ((unused));

  /* Generate code for each ordinary specialized instruction. */
  EMIT("  /* Ordinary specialized instructions. */\n");
  FOR_LIST(i, comma, vm->specialized_instructions)
    {
      const struct jitterc_specialized_instruction* sins
        = ((const struct jitterc_specialized_instruction*)
           gl_list_get_at (vm->specialized_instructions, i));
      /* Ignore special specialized instructions: we have already dealt with
         them. */
      if (sins->instruction == NULL)
        continue;

      const struct jitterc_instruction* uins = sins->instruction;
      bool is_relocatable
        = (sins->relocatability == jitterc_relocatability_relocatable);
      bool is_caller = (uins->callerness == jitterc_callerness_caller);
      bool is_callee = (uins->calleeness == jitterc_calleeness_callee);

      EMIT("  JITTER_INSTRUCTION_PROLOG_(%s, %s, %s)\n",
           sins->name, sins->mangled_name,
           (sins->hotness == jitterc_hotness_hot)
           ? "hot"
           : "cold");
      EMIT("  {\n");

      /* Emit definitions for fast-branch macros.  The definitions will be
         different for replacement and non-replacement instructions. */
      jitterc_emit_executor_fast_branch_definitions (f, vm, sins);

      if (! is_relocatable)
        {
          EMIT("    /* This specialized instruction is non-relocatable.\n");
          EMIT("       Its %i-th argument, a literal, is the return address where to jump\n",
               (int) (gl_list_size (sins->specialized_arguments) - 1));
          EMIT("       at the end, back to relocated code. */\n\n");
        }

      if (is_caller)
        {
          EMIT("    /* This specialized instruction is a caller.\n");
          EMIT("       Its %i-th argument, a literal, is the return address where to jump\n",
               (int) (gl_list_size (sins->specialized_arguments) - 1));
          EMIT("       back after the procedure returns.  Branch-and-link\n");
          EMIT("       functionality is enabled for this instruction. */\n");
          EMIT("#   define JITTER_BRANCH_AND_LINK      JITTER_BRANCH_AND_LINK_INTERNAL\n");
          EMIT("#   define JITTER_BRANCH_FAST_AND_LINK JITTER_BRANCH_FAST_AND_LINK_INTERNAL\n\n");
        }

      /* Define the specialized instruction opcode and name as macros, to be
         used in the body and, in case of fast labels, in the arguments. */
      EMIT("#   define JITTER_SPECIALIZED_INSTRUCTION_OPCODE       %i\n", i);
      EMIT("#   define JITTER_SPECIALIZED_INSTRUCTION_NAME         %s\n\n",
           sins->name);
      EMIT("#   define JITTER_SPECIALIZED_INSTRUCTION_MANGLED_NAME %s\n\n",
           sins->mangled_name);

      /* Emit a macro definition for the specialized instruction residual arity. */
      jitterc_emit_specialized_instruction_residual_arity_definition (f, sins);
      EMIT("\n");

      /* Emit macro definitions for specialized arguments, to be used in the body. */
      jitterc_emit_sarg_definitions (f, sins);
      EMIT("\n");

      if (is_callee)
        {
          EMIT("  /* This specialized instruction is a callee.  Set the link\n");
          EMIT("     pointer if needed... */\n");
          EMIT("  const union jitter_word *_jitter_the_link;\n");
          EMIT("  _JITTER_PROCEDURE_PROLOG(_jitter_the_link);\n");
          EMIT("  /* ...And make it accessible to this instruction thru a macro. */\n");
          EMIT("  #define JITTER_LINK ((const union jitter_word * const) _jitter_the_link)\n");
          EMIT("\n");
        }

      /* If this is a non-relocatable instruction and replication is enabled,
         the actual code to replicate is trivial: just a jump; then comes the
         epilog.  After the epilog we can put the label where relocated code
         jumps to, which is where control flows to in the other cases.  The
         actual user-specified code for the VM instruction comes after the
         label. */
      if (! is_relocatable)
        {
          EMIT("#ifdef JITTER_REPLICATE\n");
          EMIT("    /* Pretend to modify the non-relocatable code pointer, to force GCC to keep\n");
          EMIT("       it on the stack rather than in read-only memory.  I had to do this to prevent\n");
          EMIT("       a GCC 8 snapshot on SH from being too clever. */\n");
          EMIT("    //JITTER_MARK_MEMORY_AS_SET_BY_ASSEMBLY(JITTER_SPECIALIZED_INSTRUCTION_NON_RELOCATABLE_CODE_VARIABLE);\n");
          EMIT("    /* FIXME: no, it's not enough.  GCC 8 on SH keeps the stack pointer *offset*\n");
          EMIT("       in memory, as a 16-bit constant; and since it reads it with a PC-relative\n");
          EMIT("       load the relocated part crashes.\n");
          EMIT("                 mov.w     .L1667,r0\n");
          EMIT("                 mov.l     @(r0,r15),r1\n");
          EMIT("                 jmp       @r1\n");
          EMIT("       r15 is the stack pointer.  The constant at .L1667 is\n");
          EMIT("          .L1667:\n");
          EMIT("                 .short    232\n");
          EMIT("       and this explains everything: 232 doesn't fit in a byte sign-extended, so it\n");
          EMIT("       can't work as an immediate.  Shall I keep these code pointers as a single array?\n");
          EMIT("       I don't know.  I'll switch to GNU C nested functions for non-relocatable code,\n");
          EMIT("       but the problem will be the same. */\n");
          EMIT("    /* Jump to non-relocatable code. */\n");
          EMIT("    JITTER_COMPUTED_GOTO(JITTER_SPECIALIZED_INSTRUCTION_NON_RELOCATABLE_CODE_VARIABLE);\n");
          EMIT("\n");
          EMIT("    /* Here the residual arity is given as zero: it's too late to\n");
          EMIT("       skip residuals, since we've already jumped and this code is\n");
          EMIT("       unreachable.  The instruction pointer, if any, is advanced\n");
          EMIT("       in the non-relocatable code. */\n");
          EMIT("    JITTER_INSTRUCTION_EPILOG_(%s, %s, 0)\n", sins->name, sins->mangled_name);
          EMIT("\n");
          EMIT("    /* Relocated code will jump to this label in non-relocated code. */\n");
          EMIT("  JITTER_SPECIALIZED_INSTRUCTION_NON_RELOCATABLE_CODE_LABEL:\n");
          EMIT("    JITTER_COMMENT_IN_ASM_(\"%s non-relocatable code\");\n", sins->name);
          EMIT("#endif // #ifdef JITTER_REPLICATE\n");
        }

      if (is_caller)
        {
          EMIT("#if    defined(JITTER_DISPATCH_NO_THREADING)         \\\n");
          EMIT("    && ! defined(JITTER_MACHINE_SUPPORTS_PROCEDURE)\n");
          EMIT("  /* We use the implicit atgument at the end of the calling.\n");
          EMIT("     instruction to discover the procedure return address. */\n");
          EMIT("  const void * _jitter_return_pointer = JITTER_ARGP%i;\n",
               (int) (gl_list_size (sins->specialized_arguments) - 1));
          EMIT("  /* And make it accessible to the user (who will usually call \n");
          EMIT("     JITTER_BRANCH_AND_LINK) thru a nice macro. */\n");
          EMIT("# define JITTER_RETURN_ADDRESS _jitter_return_pointer\n");
          EMIT("#endif\n\n");
        }

      /* Emit the user C code for the beginning of every instruction, if any. */
      jitterc_emit_user_c_code_to_stream
         (vm, f, vm->instruction_beginning_c_code, "instruction-beginning-c");

      /* Emit user-specified code for the instruction.  We have already opened a brace, so
         another pair is not needed. */
      EMIT("\n    /* User code for %s . */\n", sins->name);
      EMIT("%s\n", uins->code);
      EMIT("    /* End of the user code for %s . */\n\n", sins->name);

      /* Emit the user C code for the end of every instruction, if any.  Notice
         that the code is not always reachable. */
      jitterc_emit_user_c_code_to_stream (vm, f, vm->instruction_end_c_code,
                                          "instruction-end-c");

      if (! is_relocatable)
        {
          EMIT("#ifdef JITTER_REPLICATE\n");
          EMIT("    /* Advance the instruction pointer, if any, to skip residuals;\n");
          EMIT("       then jump back to replicated code. */\n");
          EMIT("    const void *_jitter_back_to_replicated_code_pointer = JITTER_ARGP%i;\n",
               (int) (gl_list_size (sins->specialized_arguments) - 1));
          EMIT("    JITTER_SKIP_RESIDUALS_;\n");
          EMIT("    goto * _jitter_back_to_replicated_code_pointer;\n");
          EMIT("#endif // #ifdef JITTER_REPLICATE\n\n");
        }

      /* Undefine macros only visible in caller instructions. */
      if (is_caller)
        {
          EMIT("    /* Undefine macros only visible in caller instructions. */\n");
          EMIT("#   undef JITTER_BRANCH_AND_LINK\n");
          EMIT("#   undef JITTER_BRANCH_FAST_AND_LINK\n\n");
        }

      /* Undefine argument macros.  Those will be redefined before the next
         instruction as needed; it would be dangerous to leave previous
         definitions active, because some instruction body coming after this
         might reuse some old definition by mistake in case the new instruction
         doesn't override it. */
      EMIT("    /* Undefine the %s argument macros so they can't be used\n",
           sins->name);
      EMIT("       by mistake in the instruction body coming next. */\n");
      int j; char *comma __attribute__ ((unused));
      FOR_LIST(j, comma, sins->specialized_arguments)
        {
          EMIT("#   undef JITTER_SLOW_REGISTER_OFFSET%i\n", j);
          EMIT("#   undef JITTER_ARG%i\n", j);
          EMIT("#   undef JITTER_ARGN%i\n", j);
          EMIT("#   undef JITTER_ARGU%i\n", j);
          EMIT("#   undef JITTER_ARGP%i\n", j);
          EMIT("#   undef JITTER_ARGF%i\n", j);
        }

      /* Undefine the specialized instruction opcode and name. */
      EMIT("\n");
      EMIT("#   undef JITTER_SPECIALIZED_INSTRUCTION_OPCODE\n");
      EMIT("#   undef JITTER_SPECIALIZED_INSTRUCTION_NAME\n");
      EMIT("#   undef JITTER_SPECIALIZED_INSTRUCTION_MANGLED_NAME\n\n");

      EMIT("  }\n");

      /* If we have defined a link, undefine it: it is only visible in its
         instruction. */
      if (is_callee)
        {
          EMIT("  /* Undefine the link macro. */\n");
          EMIT("#   undef JITTER_LINK\n\n");
        }
      if (is_caller)
        {
          EMIT("#if    defined(JITTER_DISPATCH_NO_THREADING)         \\\n");
          EMIT("    && ! defined(JITTER_MACHINE_SUPPORTS_PROCEDURE)\n");
          EMIT("# undef JITTER_RETURN_ADDRESS\n");
          EMIT("#endif\n\n");
        }

      /* This is the instruction epilog only for relocatable instructions, and
         when replication is disabled. */
      if (! is_relocatable)
        EMIT("#ifndef JITTER_REPLICATE\n");
      EMIT(" JITTER_INSTRUCTION_EPILOG_(%s, %s, JITTER_SPECIALIZED_INSTRUCTION_RESIDUAL_ARITY)\n",
           sins->name, sins->mangled_name);
      if (! is_relocatable)
        EMIT("#endif // #ifndef JITTER_REPLICATE\n");
      EMIT("#   undef JITTER_SPECIALIZED_INSTRUCTION_RESIDUAL_ARITY\n");
      EMIT("\n");
   }
  EMIT("  /* End of the ordinary specialized instructions. */\n\n");
}

/* Emit the patch-in header, before the main executor. */
static void
jitterc_emit_patch_in_header (FILE *f, const struct jitterc_vm *vm)
{
  /* Generate the patch-in header.  The generated code expands to an inline asm
     statement.  It is convenient to keep header and footer within the main
     executor function, so as to guarantee that the order is respected. */
  EMIT("#ifdef JITTER_HAVE_PATCH_IN\n");
  EMIT("  /* Generate the single patch-in header for this executor as a\n");
  EMIT("     global asm statement.  This expands into a global definition in\n");
  EMIT("     assembly in a separate subsection, and relies on toplevel C\n");
  EMIT("     definitions not being reordered: vmprefix_execute_or_initialize\n");
  EMIT("     will add to the same global.  Do the same for defects. */\n");
  EMIT("  JITTER_DEFECT_HEADER(vmprefix);\n");
  EMIT("  JITTER_PATCH_IN_HEADER(vmprefix);\n");
  EMIT("#endif // #ifdef JITTER_HAVE_PATCH_IN\n\n");
  EMIT("#ifndef JITTER_DISPATCH_SWITCH\n");
  EMIT("  JITTER_DATA_LOCATION_HEADER(vmprefix);\n");
  EMIT("#endif // #ifndef JITTER_DISPATCH_SWITCH\n");
  EMIT("\n");
}

/* Emit the patch-in footer, after the main executor. */
static void
jitterc_emit_patch_in_footer (FILE *f, const struct jitterc_vm *vm)
{
  /* Generate the patch-in footer.  See the comment in
     jitterc_emit_patch_in_header . */
  EMIT("#ifndef JITTER_DISPATCH_SWITCH\n");
  EMIT("  JITTER_DATA_LOCATION_FOOTER(vmprefix);\n");
  EMIT("#endif // #ifndef JITTER_DISPATCH_SWITCH\n");
  EMIT("#ifdef JITTER_HAVE_PATCH_IN\n");
  EMIT("  /* Close the patch-in global definition for this executor.  This defines a\n");
  EMIT("     new global in the patch-in subsection, holding the descriptor number.\n");
  EMIT("     This is a global asm statement.  Same for defects.  See the comment before\n");
  EMIT("      the JITTER_PATCH_IN_HEADER use above. */\n");
  EMIT("  JITTER_PATCH_IN_FOOTER(vmprefix);\n");
  EMIT("  JITTER_DEFECT_FOOTER(vmprefix);\n");
  EMIT("#endif // #ifdef JITTER_HAVE_PATCH_IN\n\n");
}

/* Emit the beginning of the case for a special specialized instruction in the
   executor.  The C code for the instruction body must be emitted right after
   this. */
static void
jitterc_emit_executor_special_specialized_instruction_beginning
   (FILE *f, const struct jitterc_vm *vm,
    const char *name,
    enum jitter_specialized_instruction_opcode opcode,
    const char *hotness, int residual_arity)
{
  EMIT("JITTER_INSTRUCTION_PROLOG_(%s, %s, %s)\n",
       name, jitterc_mangle (name), hotness);
  EMIT("#define JITTER_SPECIALIZED_INSTRUCTION_RESIDUAL_ARITY %i\n",
       residual_arity);
  EMIT("#define JITTER_SPECIALIZED_INSTRUCTION_OPCODE  %i\n", opcode);
  EMIT("#define JITTER_SPECIALIZED_INSTRUCTION_NAME  %s\n", name);
  EMIT("#define JITTER_SPECIALIZED_INSTRUCTION_MANGLED_NAME  %s\n",
       jitterc_mangle (name));
  EMIT("{\n");
}

/* Emit the end of the case for a special specialized instruction in the
   executor.  This must follow the emission of the C instruction body. */
static void
jitterc_emit_executor_special_specialized_instruction_end
   (FILE *f, const struct jitterc_vm *vm,
    const char *name,
    enum jitter_specialized_instruction_opcode opcode,
    const char *hotness, int residual_arity)
{
  EMIT("}\n");
  EMIT("JITTER_INSTRUCTION_EPILOG_(%s, %s, %i)\n",
       name, jitterc_mangle (name), residual_arity);
  EMIT("#undef JITTER_SPECIALIZED_INSTRUCTION_OPCODE\n");
  EMIT("#undef JITTER_SPECIALIZED_INSTRUCTION_NAME\n");
  EMIT("#undef JITTER_SPECIALIZED_INSTRUCTION_MANGLED_NAME\n");
  EMIT("#undef JITTER_SPECIALIZED_INSTRUCTION_RESIDUAL_ARITY\n");
  EMIT("\n");
}

/* Emit the case for a special specialized instruction in the executor. */
static void
jitterc_emit_executor_special_specialized_instruction
   (FILE *f, const struct jitterc_vm *vm,
    const char *name,
    enum jitter_specialized_instruction_opcode opcode,
    const char *hotness, int residual_arity,
    const char *c_code)
{
  jitterc_emit_executor_special_specialized_instruction_beginning
     (f, vm, name, opcode, hotness, residual_arity);
  EMIT("\n%s\n", c_code);
  jitterc_emit_executor_special_specialized_instruction_end
     (f, vm, name, opcode, hotness, residual_arity);
}

/* Emit macro calls to generate data locations in a separate subsection.  This
   needs to be called as the body of the special specialized instruction
   !DATALOCATIONS . */
static void
jitterc_emit_executor_data_locations (FILE *f, const struct jitterc_vm *vm)
{
  int i, j; char *comma __attribute__ ((unused));
  EMIT("#ifndef JITTER_DISPATCH_SWITCH\n");

  /* First emit reserved registers: these are in fact guaranteed to be
     registers. */

  /* Instruction pointer, if any. */
  EMIT("#ifndef JITTER_DISPATCH_NO_THREADING\n");
  EMIT("  JITTER_DATA_LOCATION_DATUM (\"instruction pointer\", jitter_ip);\n");
  EMIT("#endif // #ifndef JITTER_DISPATCH_NO_THREADING\n");

  /* Base. */
  EMIT("  JITTER_DATA_LOCATION_DATUM (\"base\", jitter_array_base);\n");

  /* Scratch, if any. */
  EMIT("#ifdef JITTER_DISPATCH_NO_THREADING\n");
  EMIT("#ifdef JITTER_SCRATCH_REGISTER\n");
  EMIT("  JITTER_DATA_LOCATION_DATUM (\"scratch\", jitter_residual_argument_scratch_register_variable);\n");
  EMIT("#endif // #ifdef JITTER_SCRATCH_REGISTER\n\n");
  EMIT("#endif // #ifdef JITTER_DISPATCH_NO_THREADING\n");

  /* Residual registers, if any. */
  EMIT("#ifdef JITTER_DISPATCH_NO_THREADING\n");
  for (i = 0; i < vm->max_residual_arity; i ++)
    {
      EMIT("#if (%i < JITTER_RESIDUAL_REGISTER_NO)\n", i);
      EMIT("  JITTER_DATA_LOCATION_DATUM (\"residual %i\", jitter_residual_argument_%i_register_variable);\n", i, i);
      EMIT("#endif // #if (%i < JITTER_RESIDUAL_REGISTER_NO)\n", i);
    }
  EMIT("#endif // #ifdef JITTER_DISPATCH_NO_THREADING\n");

  /* Link register, if any. */
  EMIT("#if    defined(JITTER_DISPATCH_SWITCH)                    \\\n");
  EMIT("    || defined(JITTER_DISPATCH_DIRECT_THREADING)          \\\n");
  EMIT("    || defined(JITTER_DISPATCH_MINIMAL_THREADING)         \\\n");
  EMIT("    || (   defined(JITTER_DISPATCH_NO_THREADING)          \\\n");
  EMIT("        && ! defined(JITTER_MACHINE_SUPPORTS_PROCEDURE))\n");
  EMIT("\n");
  EMIT("  JITTER_DATA_LOCATION_DATUM (\"link register\", jitter_state_runtime._jitter_link);\n");
  EMIT("#endif // link register\n");

  /* For each stack... */
  FOR_LIST(i, comma, vm->stacks)
    {
      const struct jitterc_stack *stack = gl_list_get_at (vm->stacks, i);
      const char * stack_name = stack->lower_case_name;
      if (stack->optimization == jitterc_stack_optimization_tos)
        {
          EMIT("  JITTER_DATA_LOCATION_DATUM (\"%s top\", \n", stack_name);
          EMIT("     JITTER_STACK_TOS_TOP_NAME (whatever, jitter_state_runtime., %s));\n",
               stack_name);
          EMIT("  JITTER_DATA_LOCATION_DATUM (\"%s undertop ptr\", \n", stack_name);
          EMIT("     JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME (whatever, jitter_state_runtime., %s));\n",
               stack_name);
        }
      else
        {
          EMIT("  JITTER_DATA_LOCATION_DATUM (\"%s top ptr\", \n", stack_name);
          EMIT("     JITTER_STACK_NTOS_TOP_POINTER_NAME (whatever, jitter_state_runtime., %s));\n",
               stack_name);
        }
    }

  /* For each register class... */
  FOR_LIST(i, comma, vm->register_classes)
    {
      const struct jitterc_register_class *c
        = (gl_list_get_at (vm->register_classes, i));
      /* Emit each fast register as a datum. */
      for (j = 0; j < c->fast_register_no; j ++)
        EMIT("JITTER_DATA_LOCATION_DATUM(\"%%%%%c%i\", JITTER_REGISTER_%c_%i);\n",
             c->character, j, c->character, j);
    }
  EMIT("#endif // #ifndef JITTER_DISPATCH_SWITCH\n");
}

static void
jitterc_emit_executor_main_function
   (FILE *f, const struct jitterc_vm *vm)
{
  /* Generate the actual executor main function. */
  EMIT("static void\n");
  EMIT("vmprefix_execute_or_initialize (bool jitter_initialize,\n");
  EMIT("                                vmprefix_program_point jitter_initial_program_point,\n");
  EMIT("                                struct vmprefix_state * const jitter_original_state)\n");
  EMIT("{\n");

  /* Emit debugging prints.  FIXME: implement something like this, cleanly, in a
     different function. */
/*
  EMIT("#ifdef JITTER_DISPATCH_NO_THREADING\n");
  EMIT("  printf (\"JITTER_RESIDUAL_REGISTER_NO is %%i\\n\", (int)JITTER_RESIDUAL_REGISTER_NO);\n");
  EMIT("#endif // #ifdef JITTER_DISPATCH_NO_THREADING\n");
  EMIT("  printf (\"VMPREFIX_MAX_RESIDUAL_ARITY is %%i\\n\", (int)VMPREFIX_MAX_RESIDUAL_ARITY);\n");
  EMIT("  printf (\"VMPREFIX_MAX_MEMORY_RESIDUAL_ARITY is %%i\\n\", (int)VMPREFIX_MAX_MEMORY_RESIDUAL_ARITY);\n");
  EMIT("  {int q;\n");
  EMIT("#ifdef JITTER_REPLICATE\n");
  EMIT("  for (q = JITTER_RESIDUAL_REGISTER_NO; q < VMPREFIX_MAX_MEMORY_RESIDUAL_ARITY; q ++)\n");
  EMIT("    printf (\"VMPREFIX_RESIDUAL_OFFSET(%%i) is %%i or 0x%%x\\n\", q, (int)VMPREFIX_RESIDUAL_OFFSET(q), (int)VMPREFIX_RESIDUAL_OFFSET(q));\n");
  EMIT("#endif // #ifdef JITTER_REPLICATE\n");
  EMIT("  printf (\"VMPREFIX_REGISTER_r_FAST_REGISTER_NO is %%i\\n\", (int)VMPREFIX_REGISTER_r_FAST_REGISTER_NO);\n");
  EMIT("  for (q = VMPREFIX_REGISTER_r_FAST_REGISTER_NO; q < (VMPREFIX_REGISTER_r_FAST_REGISTER_NO + 10); q ++)\n");
  EMIT("    printf (\"VMPREFIX_SLOW_REGISTER_OFFSET(r, %%i) is %%i or 0x%%x\\n\", q, (int)VMPREFIX_SLOW_REGISTER_OFFSET(r, q), (int)VMPREFIX_SLOW_REGISTER_OFFSET(r, q));\n");
  EMIT("  }\n");
  EMIT("  asm volatile (\"\\n.pushsection .rodata\\n\"\n");
  EMIT("                \"\\nFOO:\\n\"\n");
  EMIT("                \"\\n.asciz \\\"" JITTER_STRINGIFY(jitter_initial_program_point) " is at %%[thing]\\\"\\n\"\n");
  EMIT("                \"\\n.popsection\\n\"\n");
  EMIT("                :\n");
  EMIT("                : [thing] \"X\" (jitter_initial_program_point)\n");
  EMIT("               );\n");
  EMIT("\n\n");
*/
  EMIT("#ifdef JITTER_DISPATCH_NO_THREADING\n");
  EMIT("  /* Save the values in the registers we reserved as global variables,\n");
  EMIT("     since from the point of view of the other C compilation units such\n");
  EMIT("     registers are callee-save.  FIXME: this is not really needed when\n");
  EMIT("     initializing, if I've been careful; but for the time being I want\n");
  EMIT("     to play it safe. */\n");
  EMIT("  union jitter_word jitter_register_buffer [VMPREFIX_REGISTER_BUFFER_ELEMENT_NO];\n");
  EMIT("  vmprefix_save_registers (jitter_register_buffer);\n");
  EMIT("#endif // #ifdef JITTER_DISPATCH_NO_THREADING\n\n");

  /* Emit the patch-in header.  This must come before the frist patch-in or
     defect use. */
  jitterc_emit_patch_in_header (f, vm);

  /* The main executor function begins with three big static arrays containing
     the labels where every specialized instruction begins and ends, and their sizes
     (only when replication is enabled), to be used only at initialization. */
  EMIT("  /* Initialization.  This is only called once at startup. */\n");
  EMIT("  if (__builtin_expect (jitter_initialize, false))\n");
  EMIT("    {\n");
  EMIT("#ifndef JITTER_DISPATCH_SWITCH\n");
  EMIT("      /* FIXME: I can do this with only one relocation, by keeping\n");
  EMIT("         a pointer to the first VM instruction beginning in a static\n");
  EMIT("         variable, and then having a static vector of offsets with\n");
  EMIT("         respect to the first pointer.  This will slightly complicate\n");
  EMIT("         my initialization code, but should make startup faster.\n");
  EMIT("         FIXME: that won't work on AVR, according to the GCC\n");
  EMIT("         documentation.  Do I care?  Probably not, since AVRs can't\n");
  EMIT("         address more than 2^16 bytes, which is too little to run my\n");
  EMIT("         VMs. */\n");
  EMIT("      static const jitter_thread vmprefix_the_threads []\n");
  EMIT("        = {\n");
  int i; char *comma;
  FOR_LIST(i, comma, vm->specialized_instructions)
    {
      const struct jitterc_specialized_instruction* sins
        = ((const struct jitterc_specialized_instruction*)
           gl_list_get_at (vm->specialized_instructions, i));
      EMIT("            && JITTER_SPECIALIZED_INSTRUCTION_BEGIN_LABEL_OF(%s)%s\n",
           sins->mangled_name, comma);
    }
  EMIT("          };\n");

  /* Also generate thread ends. */
  EMIT("      static const jitter_thread vmprefix_the_thread_ends []\n");
  EMIT("        = {\n");
  FOR_LIST(i, comma, vm->specialized_instructions)
    {
      const struct jitterc_specialized_instruction* sins
        = ((const struct jitterc_specialized_instruction*)
           gl_list_get_at (vm->specialized_instructions, i));
      EMIT("            && JITTER_SPECIALIZED_INSTRUCTION_END_LABEL_OF(%s)%s\n",
           sins->mangled_name, comma);
    }
  EMIT("          };\n");
  EMIT("      static const long\n");
  EMIT("      vmprefix_the_thread_sizes [VMPREFIX_SPECIALIZED_INSTRUCTION_NO]\n");
  EMIT("        = {\n");
  FOR_LIST(i, comma, vm->specialized_instructions)
    {
      const struct jitterc_specialized_instruction* sins
        = ((const struct jitterc_specialized_instruction*)
           gl_list_get_at (vm->specialized_instructions, i));
      EMIT("            (long) ((jitter_int) (&& JITTER_SPECIALIZED_INSTRUCTION_END_LABEL_OF(%s))\n",
           sins->mangled_name);
      EMIT("                    - (jitter_int) (&& JITTER_SPECIALIZED_INSTRUCTION_BEGIN_LABEL_OF(%s)))%s\n",
           sins->mangled_name, comma);
    }
  EMIT("          };\n");
  EMIT("      vmprefix_thread_sizes = vmprefix_the_thread_sizes;\n");
  EMIT("      vmprefix_threads = vmprefix_the_threads;\n");
  EMIT("      vmprefix_thread_ends = vmprefix_the_thread_ends;\n");

  /// FIXME: this is for debugging: begin
  EMIT("#ifdef JITTER_PROFILE\n");
  EMIT("      fprintf (stderr, \"VM instruction range: \");\n");
  const struct jitterc_specialized_instruction* first_sins
    = ((const struct jitterc_specialized_instruction*)
       gl_list_get_at (vm->specialized_instructions, 0));
  const struct jitterc_specialized_instruction* last_sins
    = ((const struct jitterc_specialized_instruction*)
       gl_list_get_at (vm->specialized_instructions,
                       gl_list_size (vm->specialized_instructions) - 1));
  EMIT("      fprintf (stderr, \"[%%p, \", && JITTER_SPECIALIZED_INSTRUCTION_BEGIN_LABEL_OF(%s));\n",
       first_sins->mangled_name);
  EMIT("      fprintf (stderr, \"%%p)\", && JITTER_SPECIALIZED_INSTRUCTION_END_LABEL_OF(%s));\n",
       last_sins->mangled_name);
  EMIT("      fprintf (stderr, \"\\n\");\n");
  EMIT("#endif // #ifdef JITTER_PROFILE\n");
  /// FIXME: this is for debugging: end

  EMIT("#endif // #ifndef JITTER_DISPATCH_SWITCH\n");
  EMIT("\n");
  EMIT("      /* Back to regular C, without our reserved registers if any; I can share\n");
  EMIT("         the end code with the non-initialization case. */\n");
  EMIT("#ifdef JITTER_HAVE_PATCH_IN\n");
  EMIT("      //JITTER_DUMP_PATCH_IN_DESCRIPTORS(vmprefix);\n");
  EMIT("#endif // #ifdef JITTER_HAVE_PATCH_IN\n");
  EMIT("      goto jitter_possibly_restore_registers_and_return_label;\n");
  EMIT("    }\n");
  EMIT("\n\n");

  EMIT("  /* Here is the actual *executor* initialization, to be run before\n");
  EMIT("     actually running the code. */\n\n");

  jitterc_emit_executor_global_wrappers (f, vm);

  /* If control flow reaches this point then we are actually executing code. */
  EMIT("  /* Make an automatic struct holding a copy of the state whose pointer was given.\n");
  EMIT("     The idea is that the copy should be in registers, as far as possible. */\n");
  EMIT("  struct vmprefix_state_runtime jitter_state_runtime\n");
  EMIT("    = jitter_original_state->vmprefix_state_runtime;\n\n");

  EMIT("  /* Initialize a pointer to The Array base.  This pointer will be in a\n");
  EMIT("     global register variable with no-threading dispatch, and with\n");
  EMIT("     other dispatches in an automatic variable, still hopefully kept\n");
  EMIT("     in a register. */\n");
  EMIT("/* About the pragma, look for \"-Wmaybe-uninitialized\" in the comments above. FIXME: this is to avoid a GCC warning with profiling.  Check with profiling on. */\n");
  EMIT("#pragma GCC diagnostic push\n");
  EMIT("#pragma GCC diagnostic ignored \"-Wmaybe-uninitialized\"\n");
  EMIT("#ifdef JITTER_DISPATCH_NO_THREADING\n");
  EMIT("# define jitter_array_base vmprefix_array_base_register_variable\n");
  EMIT("#else\n");
  // FIXME: I recently (after finalizing big changes in the executor header) commented-out the volatile qualifier here.  Make sure it was not needed, by re-testing.
  EMIT("  /*volatile*/ char * restrict jitter_array_base;\n");
  EMIT("#endif // #ifdef JITTER_DISPATCH_NO_THREADING\n");
  EMIT("#pragma GCC diagnostic pop\n");
  EMIT("  jitter_array_base\n");
  EMIT("    = (((char *) jitter_original_state->vmprefix_state_backing.jitter_array)\n");
  EMIT("       + JITTER_ARRAY_BIAS);\n");
  EMIT("\n");

  EMIT("  /* Declare the instruction pointer from the thread array, unless the dispatching\n");
  EMIT("     model is no-threading, in which case no thread array even exists. */\n");
  EMIT("  vmprefix_program_point jitter_ip = NULL; /* Invalidate to catch errors. */\n");

  /* EMIT("  /\* Declare a variable to be supposedly used as a computed goto target for jumping;\n"); */
  /* EMIT("     to any VM instruction; in actuality the variable is not ever accessed by reachable\n"); */
  /* EMIT("     code, but only mentioned in inline assembly constraints to force GCC to keep its\n"); */
  /* EMIT("     register allocation compatible between the end of a VM instruction and the beginning\n"); */
  /* EMIT("     of any other.  Assembly constraints will always require jitter_anywhere_label to be\n"); */
  /* EMIT("     in memory rather than in a register, so as not to waste one register on this. *\/\n"); */
  /* EMIT("  volatile union jitter_word jitter_anywhere_variable __attribute__ ((unused));\n\n"); */

  EMIT("  /* Save an instruction address within this function, to jump to at VM exit\n");
  EMIT("     time; that way we can be sure that at exit time we are back to\n");
  EMIT("     non-replicated code, and stuff like PC-relative addressing work again\n");
  EMIT("     without special tricks.  This variable may safely (and actually should,\n");
  EMIT("     for performance) be kept on the stack.  We need it to be volatile to\n");
  EMIT("     prevent the compiler from being too clever and jump to it using a\n");
  EMIT("     PC-relative displacement from replicated code.   It must not be static,\n");
  EMIT("     since non-automatic variables are problematic to access from replicated\n");
  EMIT("     code, which might use PC-relative addressing. */\n");
  EMIT("  volatile typeof (&& jitter_exit_vm_label) restrict\n");
  EMIT("  jitter_saved_exit_non_replicated_code_pointer = &&jitter_exit_vm_label;\n");
  EMIT("  JITTER_MARK_MEMORY_AS_SET_BY_ASSEMBLY(jitter_saved_exit_non_replicated_code_pointer);\n");
  EMIT("\n\n");

  // FIXME: move to a new function: BEGIN
  /* Generate a variable per non-relocatable specialized instruction holding the
     address where to jump out of the relocated code.  This will be useful as a
     jump target, in the first implementation of non-relocatability. */
  EMIT("#ifdef JITTER_REPLICATE\n");
  EMIT("  /* FIXME: comment. */\n");
  FOR_LIST(i, comma, vm->specialized_instructions)
    {
      const struct jitterc_specialized_instruction* sins
        = ((const struct jitterc_specialized_instruction*)
           gl_list_get_at (vm->specialized_instructions, i));

      /* Ignore special and relocatable specialized instructions. */
      if (sins->instruction == NULL
          || sins->relocatability == jitterc_relocatability_relocatable)
        continue;

      EMIT("  volatile void *JITTER_SPECIALIZED_INSTRUCTION_NON_RELOCATABLE_CODE_VARIABLE_OF(%s)\n",
           sins->mangled_name);
      EMIT("    = && JITTER_SPECIALIZED_INSTRUCTION_NON_RELOCATABLE_CODE_LABEL_OF(%s);\n",
           sins->mangled_name);
      EMIT("  asm volatile (\"#pretend to affect \" JITTER_STRINGIFY(JITTER_SPECIALIZED_INSTRUCTION_NON_RELOCATABLE_CODE_VARIABLE_OF(%s)) \"\\n\"\n", sins->mangled_name);
      EMIT("                : \"+m\" (JITTER_SPECIALIZED_INSTRUCTION_NON_RELOCATABLE_CODE_VARIABLE_OF(%s)));\n", sins->mangled_name);
    }
  EMIT("#endif // #ifdef JITTER_REPLICATE\n");
  // FIXME: move to a new function: END

  /* Insert C code from the user.  This is supposed to come in right before
     execution starts. */
  EMIT("  /* Initialization C code from the user */\n");
  EMIT("%s", vm->initialization_c_code);
  EMIT("  /* End of the initialization C code from the user */\n\n");

  EMIT("  /* Jump to the first instruction.  If replication is enabled this point\n");
  EMIT("     marks the boundary between the ordinary world of C compiled code and\n");
  EMIT("     the more fragile replicated code, where PC-relative address does\n");
  EMIT("     not work as intended (which prevents the use of global and static\n");
  EMIT("     variables, string literals and possibly large literal constants), and\n");
  EMIT("     GDB gets easily confused. */\n");
  EMIT("  jitter_ip = jitter_initial_program_point;\n\n");
  EMIT("  /* This is the actual jump to the first instruction: it's not an\n");
  EMIT("     inline asm constraint lie like below. */\n\n");
  EMIT("# if   defined(JITTER_DISPATCH_SWITCH)\n");
  EMIT("    goto jitter_dispatching_switch_label;\n");
  EMIT("# elif (defined(JITTER_DISPATCH_DIRECT_THREADING)  \\\n");
  EMIT("        || defined(JITTER_DISPATCH_MINIMAL_THREADING))\n");
  EMIT("    goto * (jitter_ip->label);\n");
  EMIT("# elif defined(JITTER_DISPATCH_NO_THREADING)\n");
  EMIT("    /* On no-threading we only use jitter_ip for the first instruction.\n");
  EMIT("       Make it an alias for the base, which will be enough to satisfy\n");
  EMIT("       inline assembly code which pretends to alter the instruction\n");
  EMIT("       pointer in ways invisible to the compiler.\n");
  EMIT("       At least in my tests this trick frees up one hardware register,\n");
  EMIT("       which is not surprising. */\n");
  EMIT("    goto * jitter_ip;\n");
  EMIT("#   define jitter_ip vmprefix_array_base_register_variable\n");
  EMIT("# else\n");
  EMIT("#   error \"unknown dispatch\"\n");
  EMIT("# endif // if ... dispatch\n");

  EMIT("#ifdef JITTER_REPLICATE\n");
  EMIT("  /* FIXME: comment: this is the fake dispatch routine. */\n");
  // FIXME: Is clobbering memory really needed?  It would be better if I didn't do this.
  //        I should explicitly mark as set the base and possibly the instruction pointer,
  //        but nothing more.
  //EMIT("  asm volatile (\"\" : : : \"memory\");\n");
  EMIT(" jitter_dispatch_label: __attribute__ ((hot))\n");
  // FIXME: same.
  //EMIT("  asm volatile (\"\\njitter_dispatch_label_asm:\\n\" : : : \"memory\");\n");
  EMIT("  asm volatile (\"\\njitter_dispatch_label_asm:\\n\" :);\n");
  EMIT("  JITTER_PRETEND_TO_UPDATE_IP_;\n");
  FOR_LIST(i, comma, vm->specialized_instructions)
    {
      EMIT("  JITTER_PRETEND_TO_UPDATE_IP_;\n");
      const struct jitterc_specialized_instruction* sins
        = ((const struct jitterc_specialized_instruction*)
           gl_list_get_at (vm->specialized_instructions, i));
      EMIT("  JITTER_PRETEND_TO_POSSIBLY_JUMP_TO_(JITTER_SPECIALIZED_INSTRUCTION_BEGIN_LABEL_OF(%s));\n",
           sins->mangled_name);
      /*
      EMIT("  JITTER_PRETEND_TO_UPDATE_IP_;\n");
      EMIT("  JITTER_PRETEND_TO_POSSIBLY_JUMP_TO_(JITTER_SPECIALIZED_INSTRUCTION_END_LABEL_OF(%s));\n",
           sins->mangled_name);
      */
    }
  //EMIT("  JITTER_PRETEND_TO_POSSIBLY_JUMP_TO_(a_label);\n");
  EMIT("  JITTER_PRETEND_TO_UPDATE_IP_;\n");
  EMIT("  JITTER_PRETEND_TO_POSSIBLY_JUMP_TO_(jitter_exit_vm_label);\n");
  EMIT("  JITTER_PRETEND_TO_UPDATE_IP_;\n");
  EMIT("  JITTER_PRETEND_TO_POSSIBLY_JUMP_TO_(jitter_possibly_restore_registers_and_return_label);\n");
  EMIT("  goto jitter_dispatch_label;\n");
  EMIT("#endif // #ifdef JITTER_REPLICATE\n\n");

  /* EMIT("#ifdef JITTER_REPLICATE\n"); */
  /* EMIT("  /\* This is actually unreachable, but I use GCC inline assembly with\n"); */
  /* EMIT("     constraints declaring to jump here just to force the compiler to\n"); */
  /* EMIT("     allocate registers at the end of each VM instruction in a compatible\n"); */
  /* EMIT("     way with the beginning of any other.  This code could, in theory,\n"); */
  /* EMIT("     jump to any label within this function -- in practice it would\n"); */
  /* EMIT("     crash horribly if ever reached, but that is not a problem. *\/\n"); */
  /* EMIT(" jitter_jump_anywhere_label: __attribute__ ((cold, unused));\n"); */
  /* EMIT("  jitter_next_program_point = && jitter_dispatch_label;\n"); */
  /* EMIT("  asm (JITTER_ASM_COMMENT_UNIQUE(\"Pretend to alter next_program_point\"\n"); */
  /* EMIT("                                 \" at %%[next_program_point] based on\"\n"); */
  /* EMIT("                                 \" jitter_state_runtime at %%[runtime]\"\n"); */
  /* EMIT("                                 \" and * jitter_original_state %%[jitter_original_state].\")\n"); */
  /* EMIT("       : [next_program_point] \"+m\" (jitter_next_program_point) // m\n"); */
  /* /\* About the constraints on [runtime], GCC 8 20170430 snapshot, */
  /*    tested on the JitterLisp VM: */
  /*    - "X": */
  /*       aarch64: invalid 'asm': invalid expression as operand */
  /*       alpha:   ok */
  /*       sh:      ok */
  /*    - "ro", "rom", "romg", "roX": */
  /*       aarch64: ok */
  /*       alpha:   cannot reload integer constant operand in 'asm' */
  /*       sh:      cannot reload integer constant operand in 'asm' */
  /*    Any constraint works on the other architectures I'm testing. */

  /*    This is ugly.  I consider SH to be important, and Aarch64 is popular. */
  /*    Alpha is lower-priority, but I like to support it as well.  This will */
  /*    need a conditional.  The "X" constraint is more reasonable, so I will */
  /*    single out aarch64. *\/ */
  /* EMIT("       : [runtime] \"X\" (jitter_state_runtime) // \"X\"\n"); */
  /* EMIT("         , [jitter_original_state] \"m\" (* jitter_original_state) // m\n"); */
  /* EMIT("      );\n"); */
  /* EMIT("  goto * jitter_next_program_point;\n"); */
  /* EMIT("#endif // #ifdef JITTER_REPLICATE\n"); */
  /* EMIT("\n"); */

  /* Generate the switch dispatcher, which expands to nothing unless
     switch-dispatching is enabled. */
  EMIT("#ifdef JITTER_DISPATCH_SWITCH\n");
  EMIT("  /* This is the dispatching switch.  At the beginning of the first VM\n");
  EMIT("     VM instruction and at the end of each other, control jumps here. */\n");
  EMIT(" jitter_dispatching_switch_label:\n");
  EMIT("  switch (jitter_ip->fixnum)\n");
  EMIT("    {\n");
  EMIT("#endif // #ifdef JITTER_DISPATCH_SWITCH\n");
  EMIT("\n");

  /* Generate code for special specialized instructions.  This has to be kept
     manually synchronized with jitterc-vm.c in case I add, remove or change
     any special specialized instruction. */

  jitterc_emit_executor_special_specialized_instruction
     (f, vm, "!INVALID",
      jitter_specialized_instruction_opcode_INVALID,
      "cold", 0,
      "jitter_fatal (\"reached the !INVALID instruction\");");
  jitterc_emit_executor_special_specialized_instruction
     (f, vm, "!BEGINBASICBLOCK",
      jitter_specialized_instruction_opcode_BEGINBASICBLOCK,
      "hot", /* This zero is a special case.  FIXME: explain. */0,
      "#ifdef JITTER_DISPATCH_MINIMAL_THREADING\n"
      "  JITTER_SET_IP (jitter_ip + 1);\n"
      "#endif // #ifdef JITTER_DISPATCH_MINIMAL_THREADING\n");
  jitterc_emit_executor_special_specialized_instruction
     (f, vm, "!EXITVM",
      jitter_specialized_instruction_opcode_EXITVM,
      "cold", 0, "JITTER_EXIT();");
  jitterc_emit_executor_special_specialized_instruction_beginning
     (f, vm, "!DATALOCATIONS",
      jitter_specialized_instruction_opcode_DATALOCATIONS,
      "cold", 0);
  jitterc_emit_executor_data_locations (f, vm);
  jitterc_emit_executor_special_specialized_instruction_end
     (f, vm, "!DATALOCATIONS",
      jitter_specialized_instruction_opcode_DATALOCATIONS,
      "cold", 0);
  jitterc_emit_executor_special_specialized_instruction
     (f, vm, "!NOP",
      jitter_specialized_instruction_opcode_NOP,
      "cold", 0, "  /* Do nothing. */;");
  jitterc_emit_executor_special_specialized_instruction
     (f, vm, "!UNREACHABLE0",
      jitter_specialized_instruction_opcode_UNREACHABLE0,
      "cold", 0,
      "jitter_fatal (\"reached the !UNREACHABLE0 instruction\");");
  jitterc_emit_executor_special_specialized_instruction
     (f, vm, "!UNREACHABLE1",
      jitter_specialized_instruction_opcode_UNREACHABLE1,
      "cold", 0,
      "jitter_fatal (\"reached the !UNREACHABLE1 instruction\");");
  jitterc_emit_executor_special_specialized_instruction
     (f, vm, "!UNREACHABLE2",
      jitter_specialized_instruction_opcode_UNREACHABLE2,
      "cold", 0,
      "jitter_fatal (\"reached the !UNREACHABLE2 instruction\");");

  /* Generate code for the ordinary specialized instructions as specified in
     user code. */
  jitterc_emit_executor_ordinary_specialized_instructions (f, vm);

  /* Close the dispatcher switch; of course this will expand to nothing unless
     switch-dispatching is enabled. */
  EMIT("#ifdef JITTER_DISPATCH_SWITCH\n");
  EMIT("  default:\n");
  EMIT("    jitter_fatal (\"invalid opcode %%li for VM specialized instruction\",\n");
  EMIT("                  (long) jitter_ip->fixnum);\n");
  EMIT("  } /* switch */\n");
  EMIT("#endif // #ifdef JITTER_DISPATCH_SWITCH\n");
  EMIT("\n");

  /* Emit the final part of the function, consisting in the label to jump to
     before exiting from the executor. */
  EMIT("  /* The code jumps here when executing the special specialized instruction\n");
  EMIT("     EXITVM, or on a call to the macro JITTER_EXIT from an ordinary specialized\n");
  EMIT("     instruction.  This code is *not* replicated: when replication is enabled\n");
  EMIT("     jumping here means crossing the boundary from the fragaile replicated\n");
  EMIT("     code back into ordinary compiled C, where PC-relative addressing works. */\n");
  EMIT("  jitter_exit_vm_label: __attribute__ ((cold));\n");
  EMIT("    JITTER_COMMENT_IN_ASM_(\"About to exit the function\");\n");
  EMIT("    // fprintf (stderr, \"Restoring the VM state to the struct...\\n\");\n");
  EMIT("    /* Copy the VM state from the local copy we have modified back to\n");
  EMIT("       the structure to which we received a pointer. */\n");
  EMIT("    jitter_original_state->vmprefix_state_runtime = jitter_state_runtime;\n");
  EMIT("\n");
  EMIT("    // fprintf (stderr, \"Exiting the VM...\\n\");\n\n");

  /* Emit the patch-in footer.  This must come after every patch-in or defect
     use. */
  jitterc_emit_patch_in_footer (f, vm);

  /* Insert C code from the user.  This is supposed to come in right after
     execution ends. */
  EMIT("  /* Finalization C code from the user */\n");
  EMIT("%s", vm->finalization_c_code);
  EMIT("  /* End of the finalization C code from the user */\n\n");

  EMIT("  /* This program point is reachable for both thread initialization and\n");
  EMIT("     execution.  In either case it is not performance-critical. */\n");
  EMIT("  jitter_possibly_restore_registers_and_return_label: __attribute__ ((cold));\n");
  EMIT("    //if (jitter_initialize) puts (\"-- RETURNING FROM INITIALIZATION\\n\");\n");
  EMIT("#ifdef JITTER_DISPATCH_NO_THREADING\n");
  EMIT("    /* Back to regular C without our reserved registers: restore the\n");
  EMIT("       values held in such registers at entry. */\n");
  EMIT("    vmprefix_restore_registers (jitter_register_buffer);\n");
  EMIT("#endif // #ifdef JITTER_DISPATCH_NO_THREADING\n");

  // FIXME: this is a test, for profiling: begin
  EMIT("#ifdef JITTER_PROFILE\n");
  EMIT("#define PROFILING_SPACE (1024 * 1024 * 100)\n");
  EMIT("    if (jitter_initialize)\n");
  EMIT("      fprintf (stderr, \"Profiling space: [%%p, %%p)\\n\", && vmprefix_profiling_space, ((char *) (&& vmprefix_profiling_space)) + PROFILING_SPACE);\n");
  EMIT("    /* Do an indirect jump to the return statement rather than a simple\n");
  EMIT("       conditional.  With this trick I can afford even a very large gap\n");
  EMIT("       within the code for a single C function, without being constrained\n");
  EMIT("       by branch offset limits on any architecture. */\n");
  EMIT("    void *return_address_variable = && return_label;\n");
  EMIT("    JITTER_MARK_LVALUE_AS_SET_BY_ASSEMBLY (return_address_variable);\n");
  EMIT("    goto *return_address_variable;\n");
  EMIT("  vmprefix_profiling_space: __attribute__ ((unused)) // FIXME: do this from assembly\n");
  EMIT("    asm volatile (\".fill (\" JITTER_STRINGIFY(PROFILING_SPACE) \")\");\n");
  EMIT("  return_label:\n");
  EMIT("#endif // #ifdef JITTER_PROFILE\n");
  EMIT("    return;\n");
  // FIXME: this is a test, for profiling: end
  EMIT("}\n");
  EMIT("\n");
}

/* FIXME: move to a template.  This might need a forward declarartion for the
   main execute-or-initialize function, currently relying on complicated
   function attributes; but that will be simplified. */
void
jitterc_emit_executor_wrappers
   (FILE *f, const struct jitterc_vm *vm)
{
  /* This function is the most critical to compile with the right GCC options;
     for any threading model more sophisticated than direct threading this is a
     matter of correctness, not just efficiency. */
  EMIT("/* The definition of this is machine-generated in vmprefix-vm2.c , and the\n");
  EMIT("   function is not intended for the user.  If initializing then set\n");
  EMIT("   structuredvm_threads and structuredvm_thread_sizes and just return, ignoring\n");
  EMIT("   the other fieldsp and s.  If not initializing then actually enter VM code\n");
  EMIT("   starting from the given program point in the pointed state. */\n");
  EMIT("static void\n");
  EMIT("vmprefix_execute_or_initialize (bool jitter_initialize,\n");
  EMIT("                                vmprefix_program_point jitter_initial_program_point,\n");
  EMIT("                                struct vmprefix_state * const jitter_original_state)\n");
  EMIT("  __attribute__ ((noclone, noinline));\n");
  EMIT("\n");
  EMIT("void\n");
  EMIT("vmprefix_execute_executable_routine (const struct jitter_executable_routine *er,\n");
  EMIT("                                     struct vmprefix_state *s)\n");
  EMIT("{\n");
  EMIT("  vmprefix_make_place_for_slow_registers (s, er->slow_register_per_class_no);\n");
  EMIT("  jitter_program_point initial_program_point\n");
  EMIT("    = VMPREFIX_EXECUTABLE_ROUTINE_BEGINNING (er);\n");
  EMIT("  vmprefix_execute_or_initialize (false, initial_program_point, s);\n");
  EMIT("}\n");
  EMIT("\n");
  EMIT("\n");
  EMIT("/* Threads or pointers to native code blocks of course don't exist with\n");
  EMIT("   switch-dispatching. */\n");
  EMIT("#ifndef JITTER_DISPATCH_SWITCH\n");
  EMIT("const jitter_thread *\n");
  EMIT("vmprefix_threads;\n");
  EMIT("\n");
  EMIT("const jitter_thread *\n");
  EMIT("vmprefix_thread_ends;\n");
  EMIT("\n");
  EMIT("const long *\n");
  EMIT("vmprefix_thread_sizes;\n");
  EMIT("#endif // #ifndef JITTER_DISPATCH_SWITCH\n");
  EMIT("\n");

  EMIT("void\n");
  EMIT("vmprefix_initialize_threads (void)\n");
  EMIT("{\n");
  EMIT("  vmprefix_execute_or_initialize (true, NULL, NULL);\n");
  EMIT("}\n");
  EMIT("\n");

  EMIT("void\n");
  EMIT("vmprefix_branch_to_program_point (vmprefix_program_point p, struct vmprefix_state *s)\n");
  EMIT("{\n");
  EMIT("  vmprefix_execute_or_initialize (false, p, s);\n");
  EMIT("}\n");
  EMIT("\n");
}

/* Emit definitions for JITTER_VM_PREFIX_LOWER_CASE and
   JITTER_VM_PREFIX_UPPER_CASE .  These should not go to public headers, but
   they are convenient to have in more than one generated C file. */
static void
jitterc_emit_vm_name_macros (FILE *f, const struct jitterc_vm *vm)
{
  /* Generate private macro definitions in the JITTER_ namespace, not exported
     to the user via headers.  These are useful to compose VM-specific
     identifiers via CPP token concatenation, in a way which is unobstrusive to
     the user.  */
  EMIT("/* These two macros are convenient for making VM-specific identifiers\n");
  EMIT("   using VM-independent macros from a public header, without polluting\n");
  EMIT("   the global namespace. */\n");
  EMIT("#define JITTER_VM_PREFIX_LOWER_CASE %s\n", vm->lower_case_prefix);
  EMIT("#define JITTER_VM_PREFIX_UPPER_CASE %s\n", vm->upper_case_prefix);
  EMIT("\n");
}

/* Do the job of jitterc_emit_vm_name_macros for the generated .c file
   containing everything excpept the executor. */
static void
jitterc_emit_vm_name_macros_vm1 (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_a_basename (vm, "vm1.c");
  jitterc_emit_vm_name_macros (f, vm);
  jitterc_fclose (f);
}

static void
jitterc_emit_executor (const struct jitterc_vm *vm)
{
  FILE *f = jitterc_fopen_w_and_remember_basename (vm, "vm2.c");
  EMIT("//#include <config.h>\n\n");

  EMIT("#include <stdbool.h>\n");
  EMIT("#include <stdio.h>\n");
  EMIT("#include <stdlib.h>\n\n");

  /* Insert C code from the user.  This is supposed to come in very early,
     before most includes. */
  EMIT("/* Early C code from the user. */\n");
  EMIT("%s", vm->early_c_code);
  EMIT("/* End of the early C code from the user. */\n\n");

  EMIT("#include <jitter/jitter.h>\n");
  EMIT("#include <jitter/jitter-instruction.h>\n\n");
  EMIT("#define JITTER_THIS_CAN_INCLUDE_JITTER_EXECUTOR_H\n");
  EMIT("#include <jitter/jitter-executor.h>\n\n");

  EMIT("#ifdef JITTER_HAVE_ASSEMBLY\n");
  EMIT("#include <jitter/jitter-machine-common.h>\n");
  EMIT("#include <jitter/machine/jitter-machine.h>\n");
  EMIT("#endif // #ifdef JITTER_HAVE_ASSEMBLY\n");

  EMIT("#include <jitter/jitter-fatal.h>\n");
  EMIT("#include <jitter/jitter-malloc.h>\n\n");

  EMIT("#include \"vmprefix-vm.h\"\n");
  EMIT("//#include \"vmprefix-meta-instructions.h\"\n");
  EMIT("//#include \"vmprefix-specialized-instructions.h\"\n");
  EMIT("//#include \"vmprefix-state.h\"\n\n");

  EMIT("/* Include stack data structure support. */\n");
  EMIT("#include <jitter/jitter-stack.h>\n\n");

  EMIT("/* Include patch-in definitions, only if patch-in is enabled.  We knoe whether it is\n");
  EMIT("   by checking JITTER_HAVE_PATCH_IN , defined in jitter/jitter-patch-in.h . */\n");
  EMIT("#include <jitter/jitter-patch-in.h>\n");
  EMIT("#ifdef JITTER_HAVE_PATCH_IN\n");
  EMIT("# include <jitter/jitter-fast-branch.h>\n");
  EMIT("\n");
  EMIT("  JITTER_DEFECT_DESCRIPTOR_DECLARATIONS_(vmprefix);\n");
  EMIT("  JITTER_PATCH_IN_DESCRIPTOR_DECLARATIONS_(vmprefix);\n");
  EMIT("#endif // #ifdef JITTER_HAVE_PATCH_IN\n\n");

  EMIT("/* Always include fast-branch definitions, which use patch-ins where possible\n");
  EMIT("   or consist in fallback definitions otherwise. */\n");
  EMIT("#include <jitter/jitter-fast-branch.h>\n\n");
  EMIT("#define JITTER_FAST_BRANCH_PREFIX vmprefix_\n\n");

  jitterc_emit_vm_name_macros (f, vm);

  /* Emit register-access macros. */
  jitterc_emit_executor_register_access_macros (f, vm);

  /* Emit global register code. */
  jitterc_emit_executor_reserve_registers (f, vm);

  /* Emit CPP definitions for stack operations. */
  jitterc_emit_stack_operation_definitions (f, vm);

  /* Insert C code from the user.  This is supposed to come in late, after CPP
     includes and definitions, right before the executor functions. */
  EMIT("/* Late C code from the user. */\n");
  EMIT("%s", vm->before_main_c_code);
  EMIT("/* End of the late C code from the user. */\n\n");

  /* Generate a few easy wrapper functions calling vmprefix_execute_or_initialize ,
     which are the actual entry points into this compilation unit. */
  jitterc_emit_executor_wrappers (f, vm);

  /* Emit the main executor/initialization function. */
  jitterc_emit_executor_main_function (f, vm);

  jitterc_fclose (f);
}




/* File copying utility.
 * ************************************************************************** */

static void
jitterc_copy_file_to_tmp (struct jitterc_vm *vm,
                          const char *to_basename,
                          const char *from_pathname)
{
  FILE *to_stream = jitterc_fopen_w_and_remember_basename (vm, to_basename);
  FILE *from_stream = jitterc_fopen_r_pathname (from_pathname);
  while (! feof (from_stream))
    {
      int c = fgetc (from_stream);
      if (c == EOF)
        break;
      EMIT_TO(to_stream, "%c", c);
    }
  jitterc_fclose (to_stream);
  jitterc_fclose (from_stream);
}

static void
jitterc_copy_template_to_tmp (struct jitterc_vm *vm,
                                const char *basename)
{
  size_t basename_size
    = strlen (vm->template_directory) + 1 + strlen (basename) + 1;
  char *from_pathname = xmalloc (basename_size);
  sprintf (from_pathname, "%s/%s", vm->template_directory, basename);

  jitterc_copy_file_to_tmp (vm, basename, from_pathname);
  free (from_pathname);
}

static void
jitterc_copy_templates_to_tmp (struct jitterc_vm *vm,
                               bool generate_frontend)
{
  jitterc_copy_template_to_tmp (vm, "vm1.c");
  jitterc_copy_template_to_tmp (vm, "vm.h");
  if (generate_frontend)
    jitterc_copy_template_to_tmp (vm, "vm-main.c");
}




/* Template and temporary file handling.
 * ************************************************************************** */

/* Copy files from the temporary directory to the actual output directory,
   replacing prefixes. */

/* Return a malloc-allocated string holding the full content of the named
   text file, failing fatally on any error. */
static char *
jitterc_file_content (const char *pathname)
{
  /* Read the whole input into core. */
  FILE *from_stream = jitterc_fopen_r_pathname (pathname);
  size_t allocated_length = 15;
  char *buffer = xmalloc (allocated_length + 1);
  size_t used_size = 0;
  while (! feof (from_stream))
    {
      int c = fgetc (from_stream);
      if (c == EOF)
        break;

      buffer [used_size ++] = c;
      if ((used_size + 1) >= allocated_length)
        buffer = xrealloc (buffer, allocated_length *= 2);
    }
  assert (used_size < allocated_length);
  buffer [used_size] = '\0';
  jitterc_fclose (from_stream);

  /* Return the buffer, trimmed so as not to waste memory. */
  return xrealloc (buffer, strlen (buffer) + 1);
}

/* Replace every occurrence of from_string with to_string within in_string,
   which must be malloc-allocated.  Free in_string and return a new copy of it,
   allocated with malloc, with the substitution performed.
   Doing this in core sounds inefficient in terms of memory use, but I don't
   feel like implementing Knuth-Morris-Pratt from scratch when in practice we
   will always work with files of modest size. */
__attribute__ ((warn_unused_result, nonnull (1, 2, 3)))
static char*
jitterc_filter_and_realloc_string (char *in_string,
                                   const char *to_string,
                                   const char *from_string)
{
  /* It's not clear what the right thing to do would be if from_string were an
     empty string; it's probably a useless case to support anyway.  Instead
     to_string and in_string are allowed to be empty. */
  size_t from_string_length = strlen (from_string);
  assert (from_string_length > 0);
  size_t to_string_length = strlen (to_string);
  size_t in_string_length = strlen (in_string);

  /* Compute a safe upper bound on the result size, and allocate a sufficiently
     large buffer. */
  size_t greater_length = (from_string_length > to_string_length)
                          ? from_string_length
                          : to_string_length;
  size_t out_string_size
    = (size_t)
      (in_string_length * ((double) greater_length) / from_string_length)
      /* Add one unit in case we round up, plus another for the final '\0'. */
      + 1 + 1;
  char *out_string = xmalloc (out_string_size);

  /* Copy the input buffer text to the output stream, with replacements. */
  char *in_pointer = in_string, *out_pointer = out_string;
  char *next_occurrence;
  /* As long as we can find an occurrence of from_string ... */
  while ((next_occurrence = strstr (in_pointer, from_string)) != NULL)
    {
      /* We found an occurrence.  Copy the part of the text we haven't copied
         yet. */
      size_t literally_copied_length = next_occurrence - in_pointer;
      memcpy (out_pointer, in_pointer, literally_copied_length);

      /* We stopped right before the occurrence of from_string in the input
         text.  But of course instead of writing from_string now we need to
         write its replacement. */
      memcpy (out_pointer + literally_copied_length, to_string,
              to_string_length);

      /* The next point to search in the input starts right after the end of
         from_string in the input buffer.  Advance to_pointer as well by
         skipping the length of to_string . */
      in_pointer = next_occurrence + from_string_length;
      out_pointer += literally_copied_length + to_string_length;
    }
  /* Copy the input text after the last occurrence until the end, including the
     final '\0'. */
  strcpy (out_pointer, in_pointer);

  /* Free the input string and return the output string we filled, trimmed so as
     not to waste memory. */
  free (in_string);
  return xrealloc (out_string, strlen (out_string) + 1);
}




/* Template and temporary file moving.
 * ************************************************************************** */

/* Move a single file from the given path to the given path, replacing the VM
   prefix in the content. */
static void
jitterc_fix_and_move (const struct jitterc_vm *vm,
                      const char *to_pathname,
                      const char *from_pathname)
{
  /* Read the input file to core. */
  char *content = jitterc_file_content (from_pathname);

  /* Perform the replacements. */
  content
    = jitterc_filter_and_realloc_string (content,
                                           vm->lower_case_prefix,
                                           INPUT_LOWER_CASE_PREFIX);
  content
    = jitterc_filter_and_realloc_string (content,
                                           vm->upper_case_prefix,
                                           INPUT_UPPER_CASE_PREFIX);

  /* Write the modified text to the output file, and free it. */
  FILE *to_stream = jitterc_fopen_w_pathname (to_pathname);
  EMIT_TO(to_stream, "%s", content);
  jitterc_fclose (to_stream);
  free (content);

  /* Remove the original file, which is supposed to be in the temporary
     directory if this function is called as intended.  Errors are not fatal
     here.  FIXME: warn? */
  unlink (from_pathname);
}

/* Move generated files from the temporary directory to the final directory,
   replacing the prefix in the content and prepending the prefix to
   basenames. */
static void
jitterc_fix_and_move_files_from_tmp (const struct jitterc_vm *vm)
{
  int i; char *comma __attribute__ ((unused));
  size_t to_directory_length = strlen (vm->directory);
  size_t tmp_directory_length = strlen (vm->tmp_directory);
  size_t prefix_length = strlen (vm->lower_case_prefix);
  FOR_LIST(i, comma, vm->written_file_names)
    {
      const char *basename = gl_list_get_at (vm->written_file_names, i);
      size_t tmp_pathname_length
        = tmp_directory_length + 1 + strlen (basename) + 1;
      char *tmp_pathname = xmalloc (tmp_pathname_length);
      sprintf (tmp_pathname, "%s/%s", vm->tmp_directory, basename);
      size_t to_pathname_length
        = to_directory_length + 1 + prefix_length + 1 + strlen (basename) + 1;
      char *to_pathname = xmalloc (to_pathname_length);
      sprintf (to_pathname, "%s/%s-%s",
               vm->directory, vm->lower_case_prefix, basename);
      jitterc_fix_and_move (vm, to_pathname, tmp_pathname);
      free (tmp_pathname);
      free (to_pathname);
    }

  /* Remove the temporary directory.  It makes no sense to fail fatally in this
     case, since the result is usable.  FIXME: warn? */
  rmdir (vm->tmp_directory);
}



/* Entry point.
 * ************************************************************************** */

void
jitterc_generate (struct jitterc_vm *vm,
                  bool generate_frontend,
                  const char *template_directory,
                  const char *output_directory)
{
  assert (vm->template_directory == NULL);
  assert (vm->directory == NULL);
  assert (vm->tmp_directory == NULL);

  /* Set directories in the VM data structure.  Make output directories if needed. */
  vm->template_directory = jitter_clone_string (template_directory);
  vm->directory = jitter_clone_string (output_directory);
  jitterc_mkdir (vm->directory);
  char *tmp = getenv ("TMPDIR");
  if (tmp == NULL)
    tmp = "/tmp";
  char *tmp_directory_basename = "jitterc-XXXXXX";
  vm->tmp_directory
    = xmalloc (strlen (tmp) + 1 + strlen (tmp_directory_basename) + 1);
  sprintf (vm->tmp_directory, "%s/%s", tmp, tmp_directory_basename);
  if (mkdtemp (vm->tmp_directory) == NULL)
    jitter_fatal ("could not make the temporary directory %s",
                   vm->tmp_directory);

  /* Copy all the templates to the temporary directory. */
  jitterc_copy_templates_to_tmp (vm, generate_frontend);

  /* Append machine-generated code to the copied templates in the temporary
     directory, and generate a separate file for the heavyweight part.  Perform
     no prefix-replacement yet. */
  jitterc_emit_vm_name (vm);
  jitterc_emit_early_header_c (vm);
  jitterc_emit_configuration (vm);
  jitterc_emit_register_classes_h (vm);
  jitterc_emit_state_h (vm);
  jitterc_emit_meta_instructions_h (vm);
  jitterc_emit_specialized_instructions_h (vm);
  jitterc_emit_register_access_macros_h (vm);
  jitterc_emit_late_header_c (vm);
  jitterc_emit_header_closing (vm);
  /* From this point on the generated code goes to non-header C files. */
  jitterc_emit_vm_name_macros_vm1 (vm);
  jitterc_emit_printer_c (vm);
  jitterc_emit_meta_instructions (vm);
  jitterc_emit_register_classes (vm);
  jitterc_emit_specialized_instruction_names (vm);
  jitterc_emit_specialized_instruction_residual_arities (vm);
  jitterc_emit_specialized_instruction_label_bitmasks (vm);
  jitterc_emit_specialized_instruction_fast_label_bitmasks (vm);
  jitterc_emit_specialized_instruction_relocatables (vm);
  jitterc_emit_specialized_instruction_callers (vm);
  jitterc_emit_specialized_instruction_callees (vm);
  jitterc_emit_worst_case_defect_table (vm);
  jitterc_emit_rewriter (vm);
  jitterc_emit_specializer (vm);
  jitterc_emit_state (vm);
  jitterc_emit_executor (vm);

  /* Move files from the temporary directory to their actual destination,
     replacing prefixes in the content and also prepending the prefix to
     the final basenames. */
  jitterc_fix_and_move_files_from_tmp (vm);
}


/* This Emacs Lisp function is convenient for turning hand-written C code into
   code for generating it.  It's not necessarily intended for the user, but
   comes in handy for editing this file.

(defun replace-region-with-emits (beginning end)
  (interactive "r")
  (save-mark-and-excursion
    (save-restriction
      (narrow-to-region beginning end)
      (let ((pairs '(("\\\\" . "\\\\\\\\")
                     ("%" . "%%")
                     ("\"" . "\\\\\"")
                     ("^" . "EMIT(\"")
                     ("$" . "\\\\n\");")))
            (case-replace t)
            (case-fold-search t)
            (fill-prefix nil)
            (indent-region-function nil))
        (dolist (pair pairs)
          (goto-char (point-min))
          (replace-regexp (car pair) (cdr pair)))))))
*/

/* FIXME: what about this hack?
   #define STRING(...) #__VA_ARGS__

   It works fine, but the C++ (and, I guess C as well) standard only requires
   compilers to accept a limited-length logical line.  This limit is 65536
   characters, which should be enough if long literal code is split across
   different macro calls once in a while.  Anyway, this is dumb.  I'm sure
   GCC, and any other well-designed compiler, has no such artificial limit. */
