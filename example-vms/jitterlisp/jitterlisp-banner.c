/* JitterLisp: banners for interactive use.

   Copyright (C) 2017, 2018, 2019 Luca Saiu
   Written by Luca Saiu

   This file is part of the JitterLisp language implementation, distributed as
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
#include "jitterlisp-banner.h"
#include <jitter/jitter.h>


/* Banner definitions.
 * ************************************************************************** */

/* The text to show before starting the interactive REPL. */
static const char *
jitterlisp_interactive_banner_text =
"================================================================\n"
"JitterLisp (from Jitter version " JITTER_PACKAGE_VERSION ")\n"
"Copyright (C) 2018, 2019 Luca Saiu\n"
"\n"
"JitterLisp comes with ABSOLUTELY NO WARRANTY; type (no-warranty)\n"
"for details.  This program is free software, and you are welcome\n"
"to redistribute it under the GNU General Public License, version\n"
"3 or later; type (copying) to display the license text.\n"
"================================================================\n"
"* Heap memory handling: "
#if defined (JITTERLISP_LITTER)
  "litter (all heap memory leaked)"
#elif defined (JITTERLISP_BOEHM_GC)
  "Boehm garbage collector"
#else
# error "unknown GC method"
#endif // GC
"\n"
"* VM dispatch:          " JITTER_DISPATCH_NAME_STRING "\n"
"* VM primitive safety:  "
#if defined (JITTERLISP_UNSAFE)
  "no type checking (unsafe)"
#else
  "run-time type checking"
#endif // safety
"\n"
"* Line editing:         "
#if defined (JITTER_HAVE_GNU_READLINE)
  "GNU Readline"
#else
  "not available"
#endif // readline
"\n"
"\n";


/* Banner printing.
 * ************************************************************************** */

/* Print the banner. */
void
jitterlisp_interactive_banner (void)
{
  printf ("%s", jitterlisp_interactive_banner_text);
}
