/* pvm.h - Poke Virtual Machine.  Definitions.   */

/* Copyright (C) 2019, 2020 Jose E. Marchesi */

/* This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef PVM_H
#define PVM_H

#include <config.h>

#include "ios.h"

#include "pvm-val.h"
#include "pvm-env.h"
#include "pvm-alloc.h"

/* The following enumeration contains every possible exit code
   resulting from the execution of a routine in the PVM.

   PVM_EXIT_OK is returned if the routine was executed successfully,
   and every raised exception was properly handled.

   PVM_EXIT_ERROR is returned in case of an unhandled exception.  */

enum pvm_exit_code
  {
    PVM_EXIT_OK,
    PVM_EXIT_ERROR
  };

/* Exceptions.  These should be in sync with the exception variables
   declared in pkl-rt.pkl */

#define PVM_E_GENERIC       0
#define PVM_E_DIV_BY_ZERO   1
#define PVM_E_NO_IOS        2
#define PVM_E_NO_RETURN     3
#define PVM_E_OUT_OF_BOUNDS 4
#define PVM_E_MAP_BOUNDS    5
#define PVM_E_EOF           6
#define PVM_E_MAP           7
#define PVM_E_CONV          8
#define PVM_E_ELEM          9
#define PVM_E_CONSTRAINT   10
#define PVM_E_IO           11

/* Note that the jitter-generated header should be included this late
   in the file because it uses some stuff defined above.  */
#include "pvm-vm.h"

typedef struct pvm *pvm;

/* Initialize a new Poke Virtual Machine and return it.  */

pvm pvm_init (void);

/* Finalize a Poke Virtual Machine, freeing all used resources.  */

void pvm_shutdown (pvm pvm);

/* Get the current run-time environment of PVM.  */

pvm_env pvm_get_env (pvm pvm);

/* Run a PVM routine in a given Poke Virtual Machine.  Put the
   resulting value in RES, if any, and return an exit code.  */

enum pvm_exit_code pvm_run (pvm pvm,
                            pvm_routine routine,
                            pvm_val *res);

/* Get and set the current endianness, negative encoding and other
   global flags for the given PVM.  */

enum ios_endian pvm_endian (pvm pvm);
void pvm_set_endian (pvm pvm, enum ios_endian endian);

enum ios_nenc pvm_nenc (pvm pvm);
void pvm_set_nenc (pvm pvm, enum ios_nenc nenc);

int pvm_pretty_print (pvm pvm);
void pvm_set_pretty_print (pvm pvm, int flag);

/* Set the current negative encoding for PVM.  NENC should be one of
 * the IOS_NENC_* values defined in ios.h */

/* The following function is to be used in pvm.jitter, because the
   system `assert' may expand to a macro and is therefore
   non-wrappeable.  */

void pvm_assert (int expression);

#endif /* ! PVM_H */
