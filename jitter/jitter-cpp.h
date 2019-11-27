/* Jitter: general-purpose CPP macros for stringification and concatenation.

   Copyright (C) 2017 Luca Saiu
   Updated in 2019 by Luca Saiu
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


#ifndef JITTER_CPP_H_
#define JITTER_CPP_H_

/* This header has no dependencies and expands to nothing, by design.  It is
   safe to include from assembly sources. */

/* Remark on multiple-level expansion.
 * ************************************************************************** */

/* Yes, these two-levels of macros are needed; see the (excellent) GNU CPP
   manual.  The resulting clumsiness is the real reason why the macros in this
   header are useful. */




/* Convenient stringification.
 * ************************************************************************** */

/* Expand to the stringification of the macro parameter, itself unexpanded. 
   This is not intended for the user. */
#define JITTER_STRINGIFY_UNEXPANDED(whatever)  \
  # whatever

/* Expand to the stringification of the macro parameter, after it is expanded
   itself. */
#define JITTER_STRINGIFY(whatever)       \
  JITTER_STRINGIFY_UNEXPANDED(whatever)




/* Convenient token concatenation.
 * ************************************************************************** */

/* Expand to the token concatentation of the given macro parameters, unexpanded.
   These are not intended for the user. */
#define JITTER_CONCATENATE_TWO_UNEXPANDED(a, b)  \
  a ## b
#define JITTER_CONCATENATE_THREE_UNEXPANDED(a, b, c)  \
  a ## b ## c
#define JITTER_CONCATENATE_FOUR_UNEXPANDED(a, b, c, d)  \
  a ## b ## c ## d
#define JITTER_CONCATENATE_FIVE_UNEXPANDED(a, b, c, d, e)  \
  a ## b ## c ## d ## e

/* Expand to the token concatentation of the given macro parameters, after they
   are expanded themselves. */
#define JITTER_CONCATENATE_TWO(a, b)  \
  JITTER_CONCATENATE_TWO_UNEXPANDED(a, b)
#define JITTER_CONCATENATE_THREE(a, b, c)  \
  JITTER_CONCATENATE_THREE_UNEXPANDED(a, b, c)
#define JITTER_CONCATENATE_FOUR(a, b, c, d)  \
  JITTER_CONCATENATE_FOUR_UNEXPANDED(a, b, c, d)
#define JITTER_CONCATENATE_FIVE(a, b, c, d, e)  \
  JITTER_CONCATENATE_FIVE_UNEXPANDED(a, b, c, d, e)


#endif // #ifndef JITTER_CPP_H_
