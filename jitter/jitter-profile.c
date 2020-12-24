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
#if defined (JITTER_INSTRUMENT_FOR_PROFILING)
  size_t element_no = vm->specialized_instruction_no;
  jitter_profile res = jitter_xmalloc (vm->specialized_instruction_no
                                       * sizeof (jitter_ulong_long));
  int i;
  for (i = 0; i < element_no; i ++)
    res [i] = 0;
  return res;
#else
  return NULL;
#endif // #if defined (JITTER_INSTRUMENT_FOR_PROFILING)
}

void
jitter_profile_destroy (jitter_profile p)
{
  free (p);
}

void
jitter_profile_merge_from (const struct jitter_vm *vm,
                           jitter_profile to, const jitter_profile from)
{
#if defined (JITTER_INSTRUMENT_FOR_PROFILING)
  size_t element_no = vm->specialized_instruction_no;
  int i;
  for (i = 0; i < element_no; i ++)
    to [i] += from [i];
#endif // #if defined (JITTER_INSTRUMENT_FOR_PROFILING)
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
};

/* Return a malloc-allocated array of profile items, not sorted, containing
   specialised instructions for the pointed VM and data from the given
   profile.  Ignore instructions having a zero count.
   Set the pointed size_t value to the number of elements. */
__attribute__ ((unused))
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
        outi ++;
      }
  * element_no_p = outi;
  return items;
}

/* A comparison function between struct jitter_profile_item objects, for
   qsort. */
__attribute__ ((unused))
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
__attribute__ ((unused))
static void
jitter_profile_items_sort (struct jitter_profile_item *is, size_t element_no)
{
  qsort (is, element_no, sizeof (struct jitter_profile_item),
         jitter_profile_item_compare);
}

/* Return the maximum name length in the pointed array of struct
   jitter_profile_item values. */
__attribute__ ((unused))
static
size_t jitter_longest_name_in (const struct jitter_profile_item *items,
                               size_t element_no)
{
  size_t candidate_maximum_length = 0;
  int i;
  for (i = 0; i < element_no; i ++)
    {
      size_t length = strlen (items [i].name);
      if (length > candidate_maximum_length)
        candidate_maximum_length = length;
    }
  return candidate_maximum_length;
}




/* Printing.
 * ************************************************************************** */

/* Like jitter_mutable_routine_begin_class. */
static void
jitter_profile_begin_class (jitter_print_context ctx,
                            const struct jitter_vm *vm,
                            const char *suffix)
{
  char *prefix = vm->configuration.lower_case_prefix;
  size_t size = strlen (prefix) + 1 + strlen (suffix) + 1;
  char *buffer = jitter_xmalloc (size);
  sprintf (buffer, "%s-%s", prefix, suffix);
  jitter_print_begin_class (ctx, buffer);
  free (buffer);
}

void
jitter_profile_print_specialized (jitter_print_context ct,
                                  const struct jitter_vm *vm,
                                  const jitter_profile p)
{
#if defined (JITTER_INSTRUMENT_FOR_PROFILING)
  size_t element_no;
  struct jitter_profile_item *items
    = jitter_specialized_instruction_profile_items (vm, p, & element_no);

  jitter_profile_items_sort (items, element_no);

  size_t maximum_name_length = jitter_longest_name_in (items, element_no);
  int i;
  for (i = 0; i < element_no; i ++)
    {
      struct jitter_profile_item *item = items + i;
      jitter_profile_begin_class (ct, vm, "instruction");
      jitter_print_char_star (ct, item->name);
      jitter_print_end_class (ct);
      size_t name_length = strlen (item->name);
      int j;
      for (j = name_length; j < maximum_name_length; j ++)
        jitter_print_char (ct, ' ');
      jitter_print_char_star (ct, "  ");
      jitter_profile_begin_class (ct, vm, "number");
      jitter_print_ulong_long (ct, 10, item->count);
      jitter_print_end_class (ct);
      jitter_print_char (ct, '\n');
    }
  free (items);
#else
  jitter_profile_begin_class (ct, vm, "warning");
  jitter_print_char_star (ct, "[Profiling not configured in for ");
  jitter_print_char_star (ct, vm->configuration.lower_case_prefix);
  jitter_print_char_star (ct, "; you may want to recompile with the macro ");
  jitter_print_char_star (ct, "JITTER_INSTRUMENT_FOR_PROFILING defined.]\n");
  jitter_print_end_class (ct);
#endif // #if defined (JITTER_INSTRUMENT_FOR_PROFILING)
}
