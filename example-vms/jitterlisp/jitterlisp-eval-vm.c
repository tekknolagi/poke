/* Jittery Lisp: interpreter: naïve C version.

   Copyright (C) 2018 Luca Saiu
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


#include "jitterlisp-eval-vm.h"

#include "jitterlisp.h"




/* Jittery evaluation engine.
 * ************************************************************************** */

/* Fail saying that the VM is not yet implemented. */
__attribute__ ((noreturn))
static void
jitterlisp_vm_unimplemented (void)
{
  jitterlisp_error_cloned ("Jittery VM not implemented yet: plase run "
                           "with --no-vm or explicitly use the AST "
                           "interpreter");
}

jitterlisp_object
jitterlisp_eval_globally_vm (jitterlisp_object form)
{
  jitterlisp_vm_unimplemented ();
}

jitterlisp_object
jitterlisp_eval_vm (jitterlisp_object form, jitterlisp_object env)
{
  jitterlisp_vm_unimplemented ();
}

jitterlisp_object
jitterlisp_apply_vm (jitterlisp_object closure_value,
                     jitterlisp_object operands_as_list)
{
  jitterlisp_vm_unimplemented ();
}
