/* JitterLisp: banners for interactive use.

   Copyright (C) 2017, 2018, 2019, 2020 Luca Saiu
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
#include <jitter/jitter-fatal.h>


/* Banner definitions.
 * ************************************************************************** */

/* The text to show before starting the interactive REPL. */
static const char *
jitterlisp_interactive_banner_text =
"================================================================\n"
"JitterLisp (from Jitter version " JITTER_PACKAGE_VERSION ")\n"
"Copyright (C) 2017-2020 Luca Saiu\n"
"\n"
"JitterLisp comes with ABSOLUTELY NO WARRANTY; type (no-warranty)\n"
"for details.  This program is free software, and you are welcome\n"
"to redistribute it under the GNU General Public License, version\n"
"3 or later; type (copying) to display the license text.\n"
"================================================================\n";




/* Banner printing.
 * ************************************************************************** */

static void
jitterlisp_interactive_banner_feature (const char *feature_name,
                                       const char *feature_value)
{
  const int name_width = 29;
  jitter_print_char_star (jitterlisp_print_context, "* ");
  jitterlisp_begin_class (jitterlisp_print_context, "banner_feature_name");
  jitter_print_char_star (jitterlisp_print_context, feature_name);
  jitterlisp_end_class (jitterlisp_print_context);
  jitter_print_char_star (jitterlisp_print_context, ":");
  int i;
  for (i = /* "* " */ 2 + strlen (feature_name) + /* ":" */ 1;
       i < name_width;
       i ++)
    jitter_print_char (jitterlisp_print_context, ' ');

  /* By convention if the feature value contains a '!' character then the
     setting is dangerous and the text describing it should be decorated as a
     warning. */
  char *class_suffix = "banner_feature_value";
  if (strchr (feature_value, '!') != NULL)
    class_suffix = "warning";
  jitterlisp_begin_class (jitterlisp_print_context, class_suffix);
  jitter_print_char_star (jitterlisp_print_context, feature_value);
  jitterlisp_end_class (jitterlisp_print_context);
  jitter_print_char_star (jitterlisp_print_context, "\n");
}

/* Print the banner. */
void
jitterlisp_interactive_banner (void)
{
  jitterlisp_begin_class (jitterlisp_print_context, "banner");
  jitter_print_char_star (jitterlisp_print_context,
                          jitterlisp_interactive_banner_text);
  jitterlisp_end_class (jitterlisp_print_context);

  /* Show information about configured or enabled features. */
  const struct jitter_vm_configuration *c = jitterlispvm_vm_configuration;
  jitterlisp_interactive_banner_feature ("VM dispatch",
                                         c->dispatch_human_readable);
  switch (c->instrumentation)
    {
    case jitter_vm_instrumentation_none:
      /* Do not waste a line in the banner for every run when profiling is
         disabled, which will be almost all the time: only print when profiling
         is on. */
      break;
    case jitter_vm_instrumentation_count:
      jitterlisp_interactive_banner_feature
        ("VM profiling",
         "count instrumentation (slow!)");
      break;
    case jitter_vm_instrumentation_sample:
      jitterlisp_interactive_banner_feature
        ("VM profiling",
         "sample instrumentation (slow!)");
      break;
    case jitter_vm_instrumentation_count_and_sample:
      jitterlisp_interactive_banner_feature
        ("VM profiling",
         "count+sample instrumentation(slow!)");
      break;
    default:
      jitter_fatal ("unknown instrumentation (this should not happen)");
    }
  jitterlisp_interactive_banner_feature ("Compiled primitive safety",
#if defined (JITTERLISP_UNSAFE)
                                         "no type or overflow checking (unsafe!)"
#else
                                         "run-time type and overflow checking"
#endif // safety
                                         );
  jitterlisp_interactive_banner_feature ("Heap memory handling",
#if defined (JITTERLISP_LITTER)
                                         "litter (heap memory leaked!)"
#elif defined (JITTERLISP_BOEHM_GC)
                                         "Boehm garbage collector"
#else
# error "unknown GC method"
#endif // GC
                                         );
  jitterlisp_interactive_banner_feature ("Line editing",
#if defined (JITTER_HAVE_READLINE)
                                         "GNU Readline"
#else
                                         "not available"
#endif // readline
                                         );
  const char *styling;
#if defined (JITTER_WITH_LIBTEXTSTYLE)
  if (jitterlisp_settings.colorize)
    styling = "GNU Libtextstyle";
  else
    styling = "GNU Libtextstyle (disabled)";
#else
  styling = "not available";
#endif
  jitterlisp_interactive_banner_feature ("Output styling", styling);

  jitter_print_char_star (jitterlisp_print_context, "\n");
}
