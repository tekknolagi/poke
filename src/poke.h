/* poke.h - Interactive editor for binary files.  */

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

#ifndef POKE_H
#define POKE_H

#include "pkl.h"

extern int poke_interactive_p;
extern int poke_quiet_p;
extern int poke_exit_p;
extern int poke_exit_code;
extern pkl_compiler poke_compiler;
extern pvm poke_vm;
extern char *poke_datadir;
extern int poke_obase;

void pk_print_version ();

#endif /* !POKE_H */
