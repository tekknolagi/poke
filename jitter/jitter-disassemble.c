/* VM library: native code disassembler.

   Copyright (C) 2017, 2019 Luca Saiu
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


//#include <config.h>

#include <stdio.h>
#include <sys/types.h> /* for getpid */
#include <unistd.h>
#include <string.h>

#include <jitter/jitter.h>
#include <jitter/jitter-malloc.h>
#include <jitter/jitter-fatal.h>

#include <jitter/jitter-instruction.h>
#include <jitter/jitter-routine.h>
#include <jitter/jitter-specialize.h>
#include <jitter/jitter-disassemble.h>
#include <jitter/jitter-vm.h>

/* I need objdump's name and the architecture name for objdump.  Notice that
   this is not Gnulib's special config.h: the libjitter runtime must not depend
   on Gnulib. */
#include "config.h"

/* Almost nothing of what follows is relevant with switch-dispatching. */
#ifdef JITTER_DISPATCH_SWITCH
__attribute__ ((noinline, noclone))
void
jitter_disassemble_executable_routine_to (FILE *f,
                                          const struct jitter_executable_routine
                                          *er, bool raw,
                                          const char *objdump_name,
                                          const char *objdump_options_or_NULL)
{
  /* Just refuse to disassemble under switch dispatching. */
  fprintf (f, "<switch dispatching: refusing to disassemble>\n");
}

#else /* not switch-dispatching */

static const char *
endianness_option
#ifdef WORDS_BIGENDIAN
  = "--endian=big";
#else
  = "--endian=little";
#endif // #ifdef WORDS_BIGENDIAN

/* Return a the full pathname of a fresh temporary file name whose basename
   starts with the given prefix, or NULL on failure.  This might be worth
   factoring into the Jitter library, but the non-fatal failure mode is probably
   only needed here. */
static char *
jitter_temporary_file_pathname (const char *basename_prefix)
{
  /* Find a directory for temporary files. */
  char *tmp = getenv ("TMPDIR");
  if (tmp == NULL)
    tmp = "/tmp";

  /* I would like to just use mktemp here as the security risk is completely
     laughable in this case, but if I do it the linker on GNU prints a scary
     warning which might give users a wrong impression.  I'll just make my own
     crude imitation. */
  size_t pathname_length
    = strlen (tmp) + 1 + strlen (basename_prefix) + 1 + 100 + 1;
  char *pathname = jitter_xmalloc (pathname_length);
  sprintf (pathname, "%s/%s-%lu%lu%lu", tmp, basename_prefix,
           (unsigned long) rand (), (unsigned long) rand (),
           (unsigned long) rand ());

  /* There is no actual way to fail right now; but if later I switch to mktemp
     or mkstemp this will change. */
  return pathname;
}

/* Disassemble a range on the given output.  Return 0 on success, nonzero on
   failure. */
static int
jitter_disassemble_range_objdump (FILE *output,
                                  const void *beginning, size_t size_in_bytes,
                                  const char *prefix,
                                  bool raw,
                                  const char *objdump_name,
                                  const char *objdump_options)
{
  /* Copy the raw native instruction range from memory to a temporary file. */
  char *temporary_file_name
    = jitter_temporary_file_pathname ("jitter-disassembly");
  if (temporary_file_name == NULL)
    return -1;
  FILE *temporary_file = fopen (temporary_file_name, "w");
  if (temporary_file == 0)
    {
      free (temporary_file_name);
      return -1;
    }
  if (fwrite (beginning, 1, size_in_bytes, temporary_file) != size_in_bytes)
    {
      fclose (output);
      unlink (temporary_file_name);
      free (temporary_file_name);
      return -1;
    }
  if (fclose (temporary_file) != 0)
    {
      unlink (temporary_file_name);
      free (temporary_file_name);
      return -1;
    }
  /* Prepare our complicated command line calling objdump disassemble the
     temporary file. */
  char *command_line =
    jitter_xmalloc (  strlen (objdump_name)
                    + strlen (objdump_options)
                    + 1000);
  sprintf (command_line,
           "LC_ALL=C; LANG=C; LANGUAGE=C; export LC_ALL; export LANG; export LANGUAGE; %s --disassemble-all -b binary --%sshow-raw-insn %s %s --adjust-vma=0x%lx --prefix-addresses --wide %s 2> /dev/null",
           objdump_name,
           (raw ? "" : "no-"),
           endianness_option,
           objdump_options,
           (unsigned long) (jitter_uint) beginning,
           temporary_file_name);
//fprintf (stderr, "%s\n", command_line);

  /* Run the complicated command in a pipe, so that we have access to its
     output.  At this point we are also done with the command line,
     irrespectively of any errors. */
  FILE *objdump_output = popen (command_line, "r");
  free (command_line);
  if (objdump_output == NULL)
    {
      unlink (temporary_file_name);
      free (temporary_file_name);
      return -1;
    }

  /* Read the command output character by character, diverting it to the given
     stream.  I can't use a char for c, which could be unsigned on some
     platforms and therefore not able to store EOF.  This bit me on ARM. */
  int c;

  /* We want to skip the initial output from objdump until the first occurrence
     of "\n0x", and insert the given prefix before each newline; therefore we
     need to keep track of the last character we saw before the current one, and
     in the beginning phase the two previous ones as well. */
  char previous_char = '\0', previous_previous_char = '\0',
       previous_previous_previous_char = '\0';
  bool the_useful_part_has_started = false;
  while (   ! feof (objdump_output)
         && (c = fgetc (objdump_output)) != EOF)
    {
      /* Are we already past the first "\n0x" in the output from objdump? */
      if (the_useful_part_has_started)
        {
          /* Yes.  Print the prefix if we have just started a line, then, in any
             case, the character coming from objdump. */
          if (previous_char == '\n')
            fputs (prefix, output);
          fputc (c, output);
        }
      else if (   previous_previous_previous_char == '\n'
               && previous_previous_char == '0'
               && previous_char == 'x')
        {
          /* The part we want to keep has begun, but in order to recognize it we
             have not printed the prefix "\n0x"; print the "0x" part (but not an
             initial newline, which would look ugly) now, after the prefix, and
             then the last seen character, which we had also skipped. */
          fprintf (output, "%s0x%c", prefix, c);
          the_useful_part_has_started = true;
        }

      /* Keep track of the last three characters we saw.  Doing two of the three
         assignments is actually useless after the_useful_part_has_started , but
         this code is not worth optimizing.  The bottleneck is obviously not
         here. */
      previous_previous_previous_char = previous_previous_char;
      previous_previous_char = previous_char;
      previous_char = c;
    }

  /* Close the pipe, and remove the temporary file.  We get the subprocess exit
     status from pclose. */
  unlink (temporary_file_name); // !!! A useful line to disable when debugging.
  free (temporary_file_name);
  return pclose (objdump_output);
}

/* Print a memory dump of the given range.  This is useful as a fallback
   solution when objdump is not usable. */
static void
jitter_dump_range (FILE *output,
                   const void *beginning, size_t size_in_bytes,
                   const char *prefix,
                   size_t bytes_per_row)
{
  /* Prepare a format string such as "0x%08x" or "0x%016x" to print addresses,
     with every hexadecimal digit shown and left-padded with zeroes to fit a
     full word size. */
  char address_format [100];
  sprintf (address_format, "0x%%0%ulx", JITTER_BYTES_PER_WORD * 2);

  /* For every byte in the range... */
  const unsigned char *p = beginning;
  const unsigned char * const limit = p + size_in_bytes;
  size_t index;
  for (index = 0; p < limit; p ++, index ++)
    {
      /* Keep track of the byte index within the row, 0-based. */
      size_t index_in_row = index % bytes_per_row;

      /* It this is the first byte in a row then print the prefix and the
         address. */
      if (index_in_row == 0)
        {
          fputs (prefix, output);
          fprintf (output, address_format, (long) (jitter_int) p);
        }

      /* Print the byte, preceded by a space; this way there will be no trailing
         spaces on the right. */
      fprintf (output, " %02x", * p);

      /* If this is the last byte in a full row, or the last byte in the whole
         buffer in vase the last row is incomplete, we need to close the
         line. */
      if (   index_in_row == bytes_per_row - 1
          || p + 1 == limit)
        fputs ("\t?\n", output);
    }
}

static void
jitter_disassemble_range (FILE *output,
                          const void *beginning, size_t size_in_bytes,
                          bool raw,
                          const char *objdump_name,
                          const char *objdump_options)
{
  /* Optimization: do nothing if the range is empty. */
  if (size_in_bytes == 0)
    return;

  /* Avoid invoking objdump every time when it may fail, since that is
     relatively expensive.  We also don't want to check whether objdump works at
     startup, since that is not normally used in production, and again running a
     subprocess is expensive.
     Using a static varaible with no synchronization here in practice is
     harmless; multiple threads may write to it in parallel, but it would be
     always the same value. */
  static bool is_objdump_known_to_fail = false;

  /* If objdump has a possibility of working then try it, and set
     is_objdump_known_to_fail.  If it remains false after calling objdump we are
     done. */
  if (! is_objdump_known_to_fail)
      is_objdump_known_to_fail
        = jitter_disassemble_range_objdump (output,
                                            beginning, size_in_bytes,
                                            "    ",
                                            raw,
                                            objdump_name,
                                            objdump_options);

  /* If objdump fails then we have not printed anything at this point.  Use the
     fallback solution. */
  if (is_objdump_known_to_fail)
    /* The number of bytes per row here is arbitrary, even if 4 is a common
       instruction size; shall I add arguments to control this? */
    jitter_dump_range (output,
                       beginning, size_in_bytes,
                       "    ",
                       4);
}

/* The opcode parameter represents a VM-specific specialized opcode of type enum
   vmprefix_specialized_instruction_opcode , but this code is VM-independent.
   Any opcode can be encoded in a sufficiently wide unsigned integer such as
   jitter_uint . */
static void
jitter_disassemble_show_specialized_instruction
   (FILE *f,
    const struct jitter_routine *p,
    /* enum vmprefix_specialized_instruction_opcode */
    jitter_uint opcode,
    const union jitter_word * const first_residual_argument_pointer,
    size_t residual_argument_no,
    const char *native_code,
    size_t native_code_size,
    bool raw,
    const char *objdump_name,
    const char *objdump_options)
{
  fprintf (f, "%s", p->vm->specialized_instruction_names [opcode]);
  int i;
  const union jitter_word *residual_argument_pointer
    = first_residual_argument_pointer;
  for (i = 0; i < residual_argument_no; i ++)
    fprintf (f, " 0x%lx%s",
            (unsigned long)((residual_argument_pointer ++)->ufixnum),
            i < residual_argument_no - 1 ? "," : "");
  fprintf (f, " (%li bytes):\n", (long)native_code_size);
  jitter_disassemble_range (f, native_code, native_code_size, raw, objdump_name,
                            objdump_options);
}

__attribute__ ((noinline, noclone))
void
jitter_disassemble_executable_routine_to (FILE *f,
                                          const struct jitter_executable_routine
                                          *er,
                                          bool raw,
                                          const char *objdump_name,
                                          const char *objdump_options_or_NULL)
{
  /* Get the non-executable routine for er.  If that is no longer available we
     have to work differently. */
  const struct jitter_routine *p = er->routine;
  if (p == NULL)
    {
#if defined(JITTER_DISPATCH_SWITCH)
# error "this code should never be compiled."
#elif defined(JITTER_DISPATCH_DIRECT_THREADING)
      fprintf (f, "<cannot disassemble direct-threaded code without\n");
      fprintf (f, " non-executable routine>\n");
#elif defined(JITTER_DISPATCH_MINIMAL_THREADING)
#elif defined(JITTER_DISPATCH_NO_THREADING)
      jitter_disassemble_range (f, er->native_code, er->native_code_size, raw,
                                objdump_name,
                                ((objdump_options_or_NULL != NULL)
                                 ? objdump_options_or_NULL
                                 : JITTER_OBJDUMP_OPTIONS));
#else
# error "unknown dispatch: this should never happen."
#endif // dispatch
      return;
    }

  /* If we arrived here then the non-executable routine is available. */
  
  /* Refuse to disassemble if threads are overlapping or of negative size. */
  if (! p->vm->threads_validated)
    {
      fprintf (f, "<threads not validated: refusing to disassemble>\n");
      return;
    }

  /* The specialized_instructions field has been extracted to the specialized
     routine, in every dispatching mode except no-threading , which doesn't
     need it at all. */
  const union jitter_word *specialized_instructions
#if   (defined(JITTER_DISPATCH_SWITCH)                 \
       || defined(JITTER_DISPATCH_DIRECT_THREADING)    \
       || defined(JITTER_DISPATCH_MINIMAL_THREADING))
    = (const union jitter_word *) er->specialized_program;
#elif defined(JITTER_DISPATCH_NO_THREADING)
    = jitter_dynamic_buffer_to_const_pointer (& p->specialized_program);
#else
# error "unknown dispatch: this should not happen"
#endif /* dispatch */

  const int specialized_instruction_no
    = jitter_dynamic_buffer_size (& p->replicated_blocks)
      / sizeof (struct jitter_replicated_block);
  const struct jitter_replicated_block * const replicated_blocks
    = jitter_dynamic_buffer_to_const_pointer (& p->replicated_blocks);
  const union jitter_word *next_thread
    = specialized_instructions;
  int i;

  const char *objdump_options = JITTER_OBJDUMP_OPTIONS;
  if (objdump_options_or_NULL != NULL)
    objdump_options = objdump_options_or_NULL;

#ifdef JITTER_REPLICATE
  if (p->stage != jitter_routine_stage_replicated)
    jitter_fatal ("disassembling non-replicated routine");

  for (i = 0; i < specialized_instruction_no; i ++)
    {
      /* Find the native code for the i-th VM specialized instruction. */
      const struct jitter_replicated_block *replicated_block
        = replicated_blocks + i;
      /* See the comment before
         jitter_disassemble_show_specialized_instruction about the opcode
         type. */
      jitter_uint opcode = replicated_block->specialized_opcode;
      char *native_code = replicated_block->native_code;
      size_t native_code_size = replicated_block->native_code_size;
      size_t residual_argument_no
        = p->vm->specialized_instruction_residual_arities [opcode];
      /*
      fprintf (f, "%s(%li)/%li (size %li): The next code block size is %li\n",
              p->vm->specialized_instruction_names [opcode], (long)opcode,
              (long)residual_argument_no,
              (long) (p->vm->thread_sizes [opcode]),
              (long)next_native_code_block_size);
      */

      /* Disassemble this VM instruction, and only this, to stdout. */
      fprintf (f, "# ");
      fprintf (f, "%p: ", (void*)next_thread);
      jitter_disassemble_show_specialized_instruction
         (f,
          p,
          opcode,
          next_thread,
          residual_argument_no,
          native_code,
          native_code_size,
          raw,
          objdump_name,
          objdump_options);

      /* Move the native code pointer to the next VM specialized instruction,
         and the thread pointer past the arguments of the current
         instruction. */
      next_thread += residual_argument_no;
    }
#else
  if (p->stage != jitter_routine_stage_specialized)
    jitter_fatal ("disassembling non-specialized routine");
  for (i = 0; i < specialized_instruction_no; i ++)
    {
      /* Find the native code for the i-th VM specialized instruction.  See the
         comment before jitter_disassemble_show_specialized_instruction about
         the opcode type. */
      jitter_uint opcode = replicated_blocks [i].specialized_opcode;
      const char *native_code_block = p->vm->threads [opcode];
      size_t native_code_block_size = p->vm->thread_sizes [opcode];
      size_t residual_argument_no
        = p->vm->specialized_instruction_residual_arities [opcode];

      /* Disassemble this VM instruction, and only this, to stdout. */
      fprintf (f, "# ");
      fprintf (f, "%p: ", (void*)next_thread);
      jitter_disassemble_show_specialized_instruction
         (f, p,
          opcode,
          next_thread,
          residual_argument_no + 1,
          native_code_block,
          native_code_block_size,
          raw,
          objdump_name,
          objdump_options);

      /* Move the thread pointer past the arguments of the current
         instruction. */
      next_thread += residual_argument_no + 1;
    }
  return;
#endif // #ifdef JITTER_REPLICATE
}
#endif // #ifdef JITTER_DISPATCH_SWITCH

void
jitter_disassemble_executable_routine (const struct jitter_executable_routine
                                       *er, bool raw, const char *objdump_name,
                                       const char *objdump_options_or_NULL)
{
  jitter_disassemble_executable_routine_to (stdout, er, raw, objdump_name,
                                            objdump_options_or_NULL);
}
