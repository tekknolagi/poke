/* Jittery Lisp: main file.

   Copyright (C) 2017 Luca Saiu
   Written by Luca Saiu

   This file is part of the Jittery Lisp language implementation, distributed as
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


#include "jitterlisp.h"

void
jitterlisp_initialize (void)
{
  /* Initialize every subsystem. */
  jitterlisp_memory_initialize ();
  jitterlisp_sexpression_initialize ();
}

void
jitterlisp_finalize (void)
{
  /* Finalize every subsystem, in the opposite order of initialization. */
  jitterlisp_sexpression_finalize ();
  jitterlisp_memory_finalize ();
}
