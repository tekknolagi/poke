/* Jitter: profiling subsystem: header.

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


#ifndef JITTER_PROFILE_H_
#define JITTER_PROFILE_H_


#include <jitter/jitter.h>
#include <jitter/jitter-print.h>
#include <jitter/jitter-vm.h>


/* Introduction.
 * ************************************************************************** */

/* At the current time this subsystem implements a counting profile of VM
   instructions, by instrumentation.  This style of profiling is exact and
   deterministic but disruptive for performance and therefore not enabled by
   default.
   When JITTER_INSTRUMENT_FOR_PROFILING is not defined this API remains
   available for compatibility, but the functions do nothing useful.

   The functions in this header are independent from the VM, and serve to factor
   the the common logic.  The user should use the equivalent VM-specific
   functionality from generated code, which do not require a VM pointer. */




/* Data structures.
 * ************************************************************************** */

/* A profile contains execution counts for specialised VM instructions, in an
   array indexed by specialised opcode.  Each count is a jitter_ulong_long
   number.
   The user should treat this data type as abstract. */
typedef jitter_ulong_long *
jitter_profile;

/* Return a fresh profile for the pointed VM, of the correct size, initialised
   to all zeroes.  Fail fatally on allocation error. */
jitter_profile
jitter_profile_make (const struct jitter_vm *vm)
  __attribute__ ((nonnull (1), returns_nonnull));

/* Destroy the given profile. */
void
jitter_profile_destroy (jitter_profile p)
  __attribute__ ((nonnull (1)));

/* Modify the first profile by adding values from the second profile.  Both
   must have been created for the same pointed VM. */
void
jitter_profile_merge_from (const struct jitter_vm *vm,
                           jitter_profile to, const jitter_profile from)
  __attribute__ ((nonnull (1, 2, 3)));




/* Run-time update.
 * ************************************************************************** */

/* Increment the user count for the specialised instruction with the given
   specialised opcode, in the given profile.  No bound check.

   In order to minimise latency The specialised opcode should be a compile-time
   constant; this is the case when this macro is used in generated code. */
#define JITTER_PROFILE_ADD_SPECIALIZED_INSTRUCTION(profile,             \
                                                   specialised_opcode)  \
  do                                                                    \
    {                                                                   \
      (profile) [specialised_opcode] ++;                                \
    }                                                                   \
  while (false)




/* Printing.
 * ************************************************************************** */

/* Print a human-readable version of the given profile for specialised
   instructions, which must belong to the pointed VM, using the given print
   context.
   The output uses the classes:
   - "vmprefix-warning";
   - "vmprefix-instruction";
   - "vmprefix-number",
   with "vmprefix" replaced by the lower-case name for the VM. */
void
jitter_profile_print_specialized (jitter_print_context ct,
                                  const struct jitter_vm *vm,
                                  const jitter_profile p)
  __attribute__ ((nonnull (1, 2, 3)));


#endif // #ifndef JITTER_PROFILE_H_
