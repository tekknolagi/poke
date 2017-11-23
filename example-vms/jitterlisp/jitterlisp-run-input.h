/* Jittery Lisp: running from files, C strings, REPL: header.

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


#ifndef JITTERLISP_RUN_INPUT_H_
#define JITTERLISP_RUN_INPUT_H_

#include <config.h>

#include <stdio.h>

#include "jitterlisp.h"




/* Eval stub, to be actually implemented and moved.
 * ************************************************************************** */

/* Return the s-expression which is the evaluation of the given s-expression
   in the global environment. */
jitterlisp_object
jitterlisp_eval_globally (jitterlisp_object o);




/* Run s-expressions parsed from a C string.
 * ************************************************************************** */

/* Parse s-expressions from the pointed string and run each of them. */
void
jitterlisp_run_from_string (const char *string)
  __attribute__ ((nonnull (1)));



/* Run s-expressions parsed from input streams.
 * ************************************************************************** */

/* Parse s-expressions from the pointed input stream and run each of them.  The
   steram must be already open at entry, and is not closed at exit. */
void
jitterlisp_run_from_stream (FILE *input)
  __attribute__ ((nonnull (1)));




/* Run s-expressions parsed from input files.
 * ************************************************************************** */

/* Parse s-expressions from the named file (opened for input, used and then
 closed by this function) and run each of them.  If the pointed string is "-"
 then read from the standard input instead of opening a file stream. */
void
jitterlisp_run_from_named_file (const char *path_name)
  __attribute__ ((nonnull (1)));

/* Parse s-expressions from the every input file specified in the command
   line, in the order.  This uses jitterlisp_run_from_named_file on the
   file names from input_file_path_names in jitterlisp_settings . */
void
jitterlisp_run_from_input_files (void);




/* Run s-expressions interactively from a REPL.
 * ************************************************************************** */

/* Run the JitterLisp interactive REPL, returning on EOF. */
void
jitterlisp_repl (void);

#endif // #ifndef JITTERLISP_RUN_INPUT_H_
