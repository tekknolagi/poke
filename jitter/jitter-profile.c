/* Jitter: profiling subsystem.

   Copyright (C) 2020 Luca Saiu
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


#include <jitter/jitter.h>

#include <stdlib.h>
#include <string.h>

#include <jitter/jitter-profile.h>
#include <jitter/jitter-malloc.h>




/* Data structures.
 * ************************************************************************** */

jitter_profile
jitter_profile_make (const struct jitter_vm *vm)
{
  if (! vm->configuration->profile_instrumented)
    return NULL;

  jitter_profile res = jitter_xmalloc (vm->specialized_instruction_no
                                       * sizeof (jitter_ulong_long));
  jitter_profile_reset (vm, res);
  return res;
}

void
jitter_profile_destroy (jitter_profile p)
{
  free (p);
}

void
jitter_profile_reset (const struct jitter_vm *vm, jitter_profile p)
{
  if (! vm->configuration->profile_instrumented)
    return;

  size_t element_no = vm->specialized_instruction_no;
  int i;
  for (i = 0; i < element_no; i ++)
    p [i] = 0;
}

void
jitter_profile_merge_from (const struct jitter_vm *vm,
                           jitter_profile to, const jitter_profile from)
{
  if (! vm->configuration->profile_instrumented)
    return;

  size_t element_no = vm->specialized_instruction_no;
  int i;
  for (i = 0; i < element_no; i ++)
    to [i] += from [i];
}




/* Internal definitions: <name, count> pairs.
 * ************************************************************************** */

/* One entry in the profile. */
struct jitter_profile_item
{
  /* The name of the instruction, specialised or not.  The pointed memory
     belongs to the VM structure and is not copied here. */
  const char *name;

  /* The number of executions. */
  jitter_ulong_long count;

  /* The same number printed as text, not left-aligned. */
  char count_as_text [100];
};

/* Return a malloc-allocated array of profile items, not sorted, containing
   specialised instructions for the pointed VM and data from the given
   profile.  Ignore instructions having a zero count.
   Set the pointed size_t value to the number of elements. */
static struct jitter_profile_item *
jitter_specialized_instruction_profile_items (const struct jitter_vm *vm,
                                              const jitter_profile p,
                                              size_t *element_no_p)
{
  const char * const * const names = vm->specialized_instruction_names;
  size_t element_no = vm->specialized_instruction_no;
  struct jitter_profile_item *items
    = jitter_xmalloc (element_no * sizeof (struct jitter_profile_item));
  int ini, outi = 0;
  for (ini = 0; ini < element_no; ini ++)
    if (p [ini] > 0)
      {
        items [outi].name = names [ini];
        items [outi].count = p [ini];
        sprintf (items [outi].count_as_text, "%" JITTER_PRIull, p [ini]);
        outi ++;
      }
  * element_no_p = outi;
  return items;
}

/* A comparison function between struct jitter_profile_item objects, for
   qsort. */
static int
jitter_profile_item_compare (const void *pa, const void *pb)
{
  const struct jitter_profile_item *ia = pa;
  const struct jitter_profile_item *ib = pb;

  /* Notice that we sort in descending order.
     It would be nice to be able to just return the result of a subtraction, but
     here there is concrete danger of affecting the sign bit by truncating a
     wider number into an int. */
  if (ia->count > ib->count)
    return -1;
  else if (ia->count == ib->count)
    /* Secondary sort criterion: probably unimportant in practice. */
    return strcmp (ia->name, ib->name);
  else
    return 1;
}

/* Destructively sort the pointed array of items by execution count, in
   descending order. */
static void
jitter_profile_items_sort (struct jitter_profile_item *is, size_t element_no)
{
  qsort (is, element_no, sizeof (struct jitter_profile_item),
         jitter_profile_item_compare);
}

/* The common logic factoring jitter_maximal_name_length_in and
   jitter_maximal_count_width_in . */
#define JITTER_MAXIMUM_IN_ITEMS(the_items, field_name, the_element_no)   \
  do                                                                     \
    {                                                                    \
      const struct jitter_profile_item *_jitter_items = (the_items);     \
      size_t _jitter_element_no = (the_element_no);                      \
      size_t _jitter_candidate_maximal_length = 0;                       \
      int _jitter_i;                                                     \
      for (_jitter_i = 0; _jitter_i < _jitter_element_no; _jitter_i ++)  \
        {                                                                \
          size_t _jitter_length                                          \
            = strlen (_jitter_items [_jitter_i].field_name);             \
          if (_jitter_length > _jitter_candidate_maximal_length)         \
            _jitter_candidate_maximal_length = _jitter_length;           \
        }                                                                \
      return _jitter_candidate_maximal_length;                           \
    }                                                                    \
  while (false)

/* Return the maximum name length in the pointed array of struct
   jitter_profile_item values. */
static
size_t jitter_maximal_name_length_in (const struct jitter_profile_item *items,
                                      size_t element_no)
{
  JITTER_MAXIMUM_IN_ITEMS (items, name, element_no);
}

/* Return the maximum count width in the pointed array of struct
   jitter_profile_item values. */
static
size_t jitter_maximal_count_width_in (const struct jitter_profile_item *items,
                                      size_t element_no)
{
  JITTER_MAXIMUM_IN_ITEMS (items, count_as_text, element_no);
}




/* Printing.
 * ************************************************************************** */

/* Like jitter_mutable_routine_begin_class. */
static void
jitter_profile_begin_class (jitter_print_context ctx,
                            const struct jitter_vm *vm,
                            const char *suffix)
{
  char *prefix = vm->configuration->lower_case_prefix;
  size_t size = strlen (prefix) + 1 + strlen (suffix) + 1;
  char *buffer = jitter_xmalloc (size);
  sprintf (buffer, "%s-%s", prefix, suffix);
  jitter_print_begin_class (ctx, buffer);
  free (buffer);
}

/* Print a warning saying that the VM is not instrumented for profiling. */
static void
jitter_profile_print_uninstrumented (jitter_print_context ct,
                                     const struct jitter_vm *vm)
{
  jitter_profile_begin_class (ct, vm, "warning");
  jitter_print_char_star (ct, "[Profiling instrumentation not enabled for ");
  jitter_print_char_star (ct, vm->configuration->lower_case_prefix);
  jitter_print_char_star (ct, "; you may want to recompile with the macro ");
  jitter_print_char_star (ct, "JITTER_INSTRUMENT_FOR_PROFILING defined.]\n");
  jitter_print_end_class (ct);
}

void
jitter_profile_print_specialized (jitter_print_context ct,
                                  const struct jitter_vm *vm,
                                  const jitter_profile p)
{
  /* If the code was not instrumented print a warning and do nothing else. */
  if (! vm->configuration->profile_instrumented)
    {
      jitter_profile_print_uninstrumented (ct, vm);
      return;
    }

  size_t element_no;
  struct jitter_profile_item *items
    = jitter_specialized_instruction_profile_items (vm, p, & element_no);

  jitter_profile_items_sort (items, element_no);

  size_t maximal_name_length = jitter_maximal_name_length_in (items, element_no);
  size_t maximal_count_width = jitter_maximal_count_width_in (items, element_no);
  int i;
  for (i = 0; i < element_no; i ++)
    {
      struct jitter_profile_item *item = items + i;
      jitter_profile_begin_class (ct, vm, "instruction");
      jitter_print_char_star (ct, item->name);
      jitter_print_end_class (ct);

      /* Print enough additional spaces to get to the width of the longest
         instruction name, plus one space for separation, plus enough additional
         spaces so that, adding the count width, we get to the maximum width. */
      size_t name_length = strlen (item->name);
      size_t count_width = strlen (item->count_as_text);
      int j;
      for (j = name_length;
           j < maximal_name_length + 1 + maximal_count_width - count_width;
           j ++)
        jitter_print_char (ct, ' ');

      /* Print the number, aligned to the right.  This requires printing as many*/
      jitter_profile_begin_class (ct, vm, "number");
      jitter_print_char_star (ct, item->count_as_text);
      jitter_print_end_class (ct);
      jitter_print_char (ct, '\n');
    }
  free (items);
}
