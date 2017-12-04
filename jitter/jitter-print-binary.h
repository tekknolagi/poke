/* Jitter: binary number printing header.

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


#ifndef JITTER_PRINT_BINARY_H_
#define JITTER_PRINT_BINARY_H_

/* We need the FILE type. */
#include <stdio.h>

/* We need the signed and unsigned Jitter integer types. */
#include <jitter/jitter.h>




/* Binary printing.
 * ************************************************************************** */

/* Print the given number in binary to the pointed stream, using at least
   digit_no binary digits (left-padding with zeroes), and prepending a "0b"
   prefix. */
void
jitter_print_binary_zero_padded (FILE *stream, jitter_uint u, int digit_no);

/* Print the given number in binary to the pointed stream, prepending a "0b"
   prefix. */
void
jitter_print_binary (FILE *stream, jitter_uint u);

#endif // #ifndef JITTER_PRINT_BINARY_H_
