/* Jitter: binary number printing.

   Copyright (C) 2017 Luca Saiu
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


#include "jitter-print-binary.h"

#include <stdio.h>
#include <jitter/jitter.h>




/* Binary printing.
 * ************************************************************************** */

/* A helper function for jitter_print_binary_zero_padded, with the same API, but
   not printing the "0b" prefix and not printing any "0" digit for u == 0,
   unless padding requires it. */
static void
jitter_print_binary_recursive (FILE *stream, jitter_uint u, int digit_no)
{
  /* When the number is zero we have nothing more to print, except if we have to
     fill at least digit_no digits; in that case we print the approporiate
     number of zero digits, and we're done.  Notice that the recursive call is
     on the left, so these padding zeroes correctly end up in the least
     significant digits of the output. */
  if (u == 0)
    {
      int i;
      for (i = 0; i < digit_no; i ++)
        fputc ('0', stream);
      return;
    }

  /* Recursively print half the number rounded down, which is to say every
     binary digit but the least significant, in order. */
  jitter_print_binary_recursive (stream, u / 2, digit_no - 1);

  /* Print the least significant digit. */
  fputc ('0' + (u & 1), stream);
}

void
jitter_print_binary_zero_padded (FILE *stream, jitter_uint u, int digit_no)
{
  fputs ("0b", stream);
  jitter_print_binary_recursive (stream, u, digit_no);
}

void
jitter_print_binary (FILE *stream, jitter_uint u)
{
  /* Print a binary number with at least one digit (which may be zero). */
  jitter_print_binary_zero_padded (stream, u, 1);
}
