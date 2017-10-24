/* Jittery structured language example: code generator header.

   Copyright (C) 2017 Luca Saiu
   Written by Luca Saiu

   This file is part of the Jitter structured-language example, distributed
   along with Jitter under the same license.

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


#ifndef STRUCTURED_CODE_GENERATOR_H_
#define STRUCTURED_CODE_GENERATOR_H_

#include "structuredvm-vm.h"
#include "structured-syntax.h"

/* Make a new VM program which is the translation of the given AST program, and
   return a pointer to it.  It will be the user's responsibility to free it.  */
struct structuredvm_program *
structured_make_vm_program (struct structured_program *p)
  __attribute__ ((returns_nonnull, nonnull (1)));

#endif // #ifndef STRUCTURED_CODE_GENERATOR_H_
