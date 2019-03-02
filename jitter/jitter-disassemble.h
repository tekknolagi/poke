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


#ifndef JITTER_DISASSEMBLE_H_
#define JITTER_DISASSEMBLE_H_

#include <stdbool.h>
#include <stdio.h>

#include <jitter/jitter.h>
#include <jitter/jitter-specialize.h>

/* Disassemble the given routine to stdout.  The internal implementation is
   currently inefficient, as it relies on one objdump invocation per specialized
   instruction. */
void
jitter_disassemble_executable_routine (const struct jitter_executable_routine
                                       *er, bool raw, const char *objdump_name,
                                       const char *objdump_options_or_NULL)
  __attribute__ ((nonnull (1, 3)));

/* Like jitter_disassemble_routine, but write the output to the pointed stream
   instead of stdout. */
void
jitter_disassemble_executable_routine_to (FILE *f,
                                          const struct jitter_executable_routine
                                          *er, bool raw, const char *objdump_name,
                                          const char *objdump_options_or_NULL)
  __attribute__ ((nonnull(1, 2, 4)));


#endif // #ifndef JITTER_DISASSEMBLE_H_
