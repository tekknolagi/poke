/* Jitter utility: convenient integer parsing.

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


#include <stdlib.h>
#include <assert.h>

#include "jitter-parse-int.h"


int
jitter_string_to_long_long_inconvenient_radix (const char *s,
                                               long long *result_pointer,
                                               unsigned radix)
{
  assert (s != NULL);
  assert (result_pointer != NULL);
  char *end;
  long long res = strtoll (s, & end, 10);
  if (s [0] == '\0' || end [0] != '\0')
    return -1;

  * result_pointer = res;
  return 0;
}

int
jitter_string_to_long_long_inconvenient (const char *s,
                                         long long *result_pointer)
{
  return jitter_string_to_long_long_inconvenient_radix (s, result_pointer, 10);
}

long long
jitter_string_to_long_long_unsafe_radix (const char *s, unsigned radix)
{
  long long res;
  int conversion_result
    = jitter_string_to_long_long_inconvenient_radix (s, &res, radix);
  assert (conversion_result == 0);
  return res;
}

long long
jitter_string_to_long_long_unsafe (const char *s)
{
  return jitter_string_to_long_long_unsafe_radix (s, 10);
}

long long
jitter_string_to_natural_radix (const char *s, unsigned radix)
{
  long long res;
  int conversion_result
    = jitter_string_to_long_long_inconvenient_radix (s, &res, radix);
  if (conversion_result != 0 || res < 0)
    return -1;

  return res;
}

long long
jitter_string_to_natural (const char *s)
{
  return jitter_string_to_natural_radix (s, 10);
}
