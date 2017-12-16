/* Jittery Lisp: global settings: header.

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


#ifndef JITTERLISP_SETTINGS_H_
#define JITTERLISP_SETTINGS_H_

#include <stdbool.h>

#include <jitter/jitter-dynamic-buffer.h>


/* JitterLisp global settings.
 * ************************************************************************** */

/* Global settings for JitterLisp. */
struct jitterlisp_settings
{
  /* Non-false iff the output needs to be verbose. */
  bool verbose;

  /* If false don't print #<nothing> when it's the result of an evaluation to be
     printed.  This doesn't suppress explicit printing of values, for example by
     the display procedure. */
  bool print_nothing_results;

  /* Non-false iff we are to uses the Jittery VM, and not a naïf interpreter. */
  bool vm;

  /* Non-false iff output s-expressions need to be colorized with terminal
     escape sequences. */
  bool colorize;

  /* Some s-expressions provided from the command line to evaluate, or NULL. */
  char *sexps_string;

  /* A dynamic buffer with char * elements, each containing a filename.
     Possibly empty. */
  struct jitter_dynamic_buffer input_file_path_names;

  /* Provide an interactive REPL. */
  bool repl;
};

/* The one global variable holding JitterLisp settings.  This is initialized
   with reasonable values from jitterlisp_settings_set_default , then filled
   from the argp parser called in main and never changed after that point. */
extern struct jitterlisp_settings
jitterlisp_settings;

/* Fill jitterlisp_settings with default values. */
void
jitterlisp_settings_set_default (void);




/* Not fo the user: finalization.
 * ************************************************************************** */

/* The functionality here is called by JitterLisp's global finalization. */

/* Finalize the settings data structure, freeing resources. */
void
jitterlisp_settings_finalize (void);

#endif // #ifndef JITTERLISP_SETTINGS_H_
