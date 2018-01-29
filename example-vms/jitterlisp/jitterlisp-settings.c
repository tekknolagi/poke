/* Jittery Lisp: global settings.

   Copyright (C) 2017, 2018 Luca Saiu
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


#include "jitterlisp-settings.h"




/* JitterLisp global settings.
 * ************************************************************************** */

/* The one global variable we define here.  This is initialized by the argp
   parser in main. */
struct jitterlisp_settings
jitterlisp_settings;

void
jitterlisp_settings_set_default (void)
{
  jitterlisp_settings.verbose = false;
  jitterlisp_settings.library = true;
  jitterlisp_settings.print_nothing_results = false;
  jitterlisp_settings.print_compact_uninterned_symbols = false;
  jitterlisp_settings.cross_disassembler = false;
  jitterlisp_settings.vm = true;
  jitterlisp_settings.colorize = false;
  jitterlisp_settings.sexps_string = NULL;
  jitter_dynamic_buffer_initialize
     (& jitterlisp_settings.input_file_path_names);
  jitterlisp_settings.repl = jitterlisp_run_repl_default;
}




/* Not fo the user: finalization.
 * ************************************************************************** */

void
jitterlisp_settings_finalize (void)
{
  jitter_dynamic_buffer_finalize (& jitterlisp_settings.input_file_path_names);
}
