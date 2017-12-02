/* Jittery Lisp: eval wrapper.

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


#include "jitterlisp-eval.h"

#include <jitter/jitter-fatal.h>

#include "jitterlisp.h"
#include "jitterlisp-eval-interpreter.h"
#include "jitterlisp-settings.h"


/* Eval wrapper.
 * ************************************************************************** */

jitterlisp_object
jitterlisp_eval_globally (jitterlisp_object form)
{
  /* Right now only the naïf interpreter exists. */
  if (jitterlisp_settings.vm)
    jitterlisp_error_cloned ("Jittery VM not implemented yet: plase run "
                             "with --no-vm if you want to use eval");
  else
    return jitterlisp_eval_globally_interpreter (form);
}
