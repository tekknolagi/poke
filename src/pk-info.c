/* pk-info.c - `info' command.  */

/* Copyright (C) 2019, 2020 Jose E. Marchesi */

/* This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

#include <config.h>
#include "pk-cmd.h"

extern struct pk_cmd null_cmd;       /* pk-cmd.c  */
extern struct pk_cmd info_files_cmd; /* pk-file.c  */
extern struct pk_cmd info_var_cmd;   /* pk-def.c  */
extern struct pk_cmd info_fun_cmd;   /* pk-def.c  */

struct pk_cmd *info_cmds[] =
  {
    &info_files_cmd,
    &info_var_cmd,
    &info_fun_cmd,
    &null_cmd
  };

struct pk_trie *info_trie;

static char *
info_completion_function (const char *x, int state)
{
  static int idx = 0;
  if (state == 0)
    idx = 0;
  else
    ++idx;

  size_t len = strlen (x);
  while (1)
    {
      struct pk_cmd **c = info_cmds + idx;

      if (*c == &null_cmd)
	break;

      if (0 != strncmp ((*c)->name, x, len))
	{
	  idx++;
	  continue;
	}
      return strdup ((*c)->name);
    }

  return NULL;
}


struct pk_cmd info_cmd =
  {"info", "", "", 0, &info_trie, NULL, "info (files|variable|function)",
   info_completion_function};
