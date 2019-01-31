/* Jittery structured language example: AST operations.

   Copyright (C) 2019 Luca Saiu
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


#include "structured-syntax.h"
#include <jitter/jitter.h>
#include <jitter/jitter-fatal.h>


/* Reversing of boolean primitives.
 * ************************************************************************** */

enum structured_primitive
structured_reverse_boolean_primitive (enum structured_primitive p)
{
  switch (p)
    {
    case structured_primitive_equal:
      return structured_primitive_different;
    case structured_primitive_different:
      return structured_primitive_equal;
    case structured_primitive_less:
      return structured_primitive_greater_or_equal;
    case structured_primitive_less_or_equal:
      return structured_primitive_greater;
    case structured_primitive_greater:
      return structured_primitive_less_or_equal;
    case structured_primitive_greater_or_equal:
      return structured_primitive_less;
    default:
      jitter_fatal ("cannot reverse boolean (?) primitive: %i", (int) p);
    }
}
