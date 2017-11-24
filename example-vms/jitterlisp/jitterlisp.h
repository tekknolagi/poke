/* Jittery Lisp: main header.

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


#ifndef JITTERLISP_H_
#define JITTERLISP_H_


/* Header transitive inclusion.
 * ************************************************************************** */

/* Include the other JitterLisp headers, so that the user has to deal with this
   one only. */
#include "jitterlisp-config.h"
#include "jitterlisp-sexpression.h"
#include "jitterlisp-allocator.h"
#include "jitterlisp-error.h"
#include "jitterlisp-reader.h"
#include "jitterlisp-run-input.h"
#include "jitterlisp-printer.h"
#include "jitterlisp-settings.h"
#include "jitterlisp-eval.h"




/* Initialization and finalization.
 * ************************************************************************** */

/* Initialize JitterLisp.  It is in general unsafe to call any function or macro
   from the JitterLisp headers before this. */
void
jitterlisp_initialize (void);

/* Finalize JitterLisp.  It is in general unsafe to call any function or macro
   from the JitterLisp headers after this, until jitterlisp_initialize is called
   again. */
void
jitterlisp_finalize (void);

#endif // #ifndef JITTERLISP_H_
