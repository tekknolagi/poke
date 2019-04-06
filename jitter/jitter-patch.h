/* VM library: native code patching, machine-independent header file.

   Copyright (C) 2017 Luca Saiu
   Updated in 2019 by Luca Saiu
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


#ifndef JITTER_MACHINE_INDEPENDENT_PATCH_
#define JITTER_MACHINE_INDEPENDENT_PATCH_


/* Ignore the rest of this file if we have no host assembly support.
 * ************************************************************************** */

/* Import the preprocessor definition of JITTER_HAVE_ASSEMBLY , if any. */
#include <jitter/jitter.h>

/* Here we need to know whether a <jitter/machine/jitter-machine.h> exists for
   this architecture.  Moreover architecture-specific files may rely on C type
   sizes and endianness, and it is convenient to include the configuration file
   here rather than in every machine-specific file. */

/* If there is no assembly support ignore the rest of this file. */
#ifdef JITTER_HAVE_ASSEMBLY




/* Include headers.
 * ************************************************************************** */

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

/* Include the architecture-dependent file.  This is found at Jitter compile
   time with an explicit -I option when working with uninstalled headers, or in
   standard include paths when installed.  This way we avoid the trouble of
   referring to directories whose names depend on the configuration. */
#include <jitter/machine/jitter-machine.h>

/* Include patch-in and fast-branch information, if any. */
#include <jitter/jitter-patch-in.h>
#include <jitter/jitter-fast-branch.h>




/* Machine-independent declarations for machine-specific routines.
 * ************************************************************************** */

/* The architecture-independent declaration of an enumerate representing the
   different assembly routines we support.  Of course the actual definition is
   architecture-dependent, and comes in vm/assembly/ARCHITECTURE/machine.h .  The
   last case must be called jitter_routine_no , and must not describe an actual
   routine; it is used as a routine count. */
enum jitter_routine_to_patch;

/* A constant array of routine sizes (in bytes), to be defined for every
   architecture.  The array is meant to be indexed by enum jitter_routine_to_patch
   objects. */
extern const uint32_t
jitter_native_routine_sizes [jitter_routine_no];

/* A constant array of routine native code pointers, to be defined for every
   architecture.  The array is meant to be indexed by enum jitter_routine_to_patch
   objects. */
extern const char* const
jitter_native_routine_pointers [jitter_routine_no];

/* A constant array of routine textual names mostly intended for debugging, to
   be defined for every architecture.  The array is meant to be indexed by enum
   jitter_routine_to_patch objects. */
extern const char* const
jitter_native_routine_names [jitter_routine_no];




/* Machine-specific routine-choosing functions.
 * ************************************************************************** */

/* This function needs to be implemented for every architecture.  Return what
   routine to use for loading the pointed word-sized immediate into the given
   residual register.

   This function also takes the address of the loading code to be filled with
   the routine is given, only to be able to take its alignment in consideration;
   the code is not copied here. */
enum jitter_routine_to_patch
jitter_routine_for_loading_register (const char* immediate_pointer,
                                     unsigned int residual_register_index,
                                     const char *loading_code_to_write);

/* This function needs to be implemented for every architecture.  Return what
   routine to use for loading the pointed word-sized immediate into residual
   memory at the given index.  The index is in elements, relative to the
   residual memory base: if there are residual registers as well (which always
   take priority over memory), those are not counted here.

   This function also takes the address of the loading code to be filled with
   the routine is given, only to be able to take its alignment in consideration;
   the code is not copied here. */
enum jitter_routine_to_patch
jitter_routine_for_loading_memory (const char *immediate_pointer,
                                   unsigned int index,
                                   const char *loading_code_to_write);

#ifdef JITTER_HAVE_PATCH_IN
/* This function needs to be implemented for each architecture supporting
   patch-ins.  Return the routine for a patch-in in the given case.
   FIXME: this part of the API is still tentative. */
enum jitter_routine_to_patch
jitter_routine_for_patch_in (const struct jitter_patch_in_descriptor *dp);
#endif // #ifdef JITTER_HAVE_PATCH_IN





/* Machine-specific code-patching functions.
 * ************************************************************************** */

/* This function needs to be implemented for every architecture.  Patch the
   native code at the given address, which must already contain a copy of the
   specified routine which is assumed to be of the given size, to the pointed
   word-sized immediate.  The function cannot assume that the pointed object
   remain valid or allocated after it returns. */
void
jitter_patch_load_immediate_to_register (char *native_code,
                                         size_t native_code_size,
                                         const char *immediate_pointer,
                                         enum jitter_routine_to_patch routine);

/* This function needs to be implemented for every architecture.  Patch the
   native code at the given address, which must already contain a copy of the
   specified routine, assumed to be of the given size, to load the pointed
   word-sized immediate into residual memory, at the given index.  The index is
   specified in memory elements, 0-based; in other words index holds the element
   index within memory: if there are residual registers as well (which always
   take priority over memory), those are not counted here. */
void
jitter_patch_load_immediate_to_memory (char *native_code,
                                       size_t native_code_size,
                                       unsigned int memory_index,
                                       const char *immediate_pointer,
                                       enum jitter_routine_to_patch routine);


#ifdef JITTER_HAVE_PATCH_IN
/* This function needs to be implemented for each architecture supporting
   patch-ins.  Given a pointer to the native code to modify (within replicated
   code, with the offset already added), a pointer to the immediate value
   and a pointer to the description, patch the code using the given routine.
   FIXME: this part of the API is still tentative. */
void
jitter_patch_patch_in (char *native_code,
                       const char *immediate_pointer,
                       const struct jitter_patch_in_descriptor *descriptor,
                       enum jitter_routine_to_patch routine);
#endif // #ifdef JITTER_HAVE_PATCH_IN




/* Machine-independent convenience functions.
 * ************************************************************************** */

/* The functions below are implemented in jitter-patch.c once and for all, and
   are used for every architecture.  They are convenient to be called from the
   replication subsystem, while the functions above serve to implement these. */

/* Call either jitter_routine_for_loading_register or jitter_routine_for_loading_memory
   as appropriate according to the given residual index. */
enum jitter_routine_to_patch
jitter_routine_for_loading (const char *immediate_pointer,
                            unsigned int residual_index,
                            const char *loading_code_to_write);

/* Copy the given routine into memory starting from the given address. */
void
jitter_copy_routine (char *native_code, enum jitter_routine_to_patch routine);

/* Call either jitter_patch_load_immediate_to_register or
   jitter_patch_load_immediate_to_memory , as appropriate according to the residual
   index.  Differently from jitter_patch_load_immediate_to_memory , here the index
   parameter (still 0-based) is an index of the residual argument, independent
   from how many residual registers are available. */
void
jitter_patch_load_immediate (char *native_code,
                             unsigned int residual_index,
                             const char *immediate_pointer,
                             enum jitter_routine_to_patch routine);

/* Return a pointer to the beginning of the native code of the given routine.
   This relies on jitter_native_routine_pointers, which must be defined for every
   architecture. */
const char*
jitter_routine_code (enum jitter_routine_to_patch routine);

/* Return the size of the given routine code, in bytes.  This relies on
   jitter_native_routine_sizes, which must be defined for every architecture. */
size_t
jitter_routine_size (enum jitter_routine_to_patch routine);

/* Return the name of the given routine.  This relies on
   jitter_native_routine_names, which must be defined for every architecture. */
const char*
jitter_routine_name (enum jitter_routine_to_patch routine);




/* Machine-independent utility functions for choosing routines.
 * ************************************************************************** */

/* The functions below are designed to be called by the architecture-specific
   code, particularly for deciding which routine is appropriate to use when
   dealing with a given immediate. */

/* Return true iff the given argument, taken as a word-sized integer, would be
   represented as negative. */
bool
jitter_is_negative (int64_t word);

/* Return true iff the given argument, taken as a word-sized unsigned integer,
   fits within the specified number of bits.  For example 0, 4, 45 or 255 fit
   in 8 bits, but 256 does not.
   The number of bits does not need to be a power of two, and can even be
   greater than 64. */
bool
jitter_fits_in_bits_zero_extended (uint64_t word, unsigned bit_no);

/* Return true iff the given two's complement word can be truncated to the given
   number of (low) bits and sign-extended back to a word without loss of
   information.
   The given number of bits must be between 0 and 64, included. */
bool
jitter_fits_in_bits_sign_extended (uint64_t word, unsigned bit_no);

#endif // #ifdef JITTER_HAVE_ASSEMBLY
#endif // #ifndef JITTER_MACHINE_INDEPENDENT_PATCH_
