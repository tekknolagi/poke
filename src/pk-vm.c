/* pk-vm.c - PVM related commands.  */

/* Copyright (C) 2018 Jose E. Marchesi */

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
#include <assert.h>

#include "poke.h"
#include "pk-cmd.h"
#include "pvm.h"

#define PK_VM_DIS_UFLAGS "n"
#define PK_VM_DIS_F_NAT 0x1

static int
pk_cmd_vm_disas (int argc, struct pk_cmd_arg argv[], uint64_t uflags)
{
  /* disassemble expression EXP.  */

  pvm_program prog;
  
  assert (argc == 1);
  assert (PK_CMD_ARG_TYPE (argv[0]) == PK_CMD_ARG_EXP);

  prog = PK_CMD_ARG_EXP (argv[0]);

  if (uflags & PK_VM_DIS_F_NAT)
    pvm_disassemble_program (prog, true,
                             JITTER_OBJDUMP, NULL);
  else
    pvm_print_program (stdout, prog);
  
  return 1;
}

extern struct pk_cmd null_cmd; /* pk-cmd.c  */

struct pk_cmd vm_disas_cmd =
  {"disassemble", "e", PK_VM_DIS_UFLAGS, 0, NULL, pk_cmd_vm_disas,
   "vm disassemble EXP\n\
Flags:\n\
  n (do a native disassemble)"};

struct pk_cmd *vm_cmds[] =
  {
    &vm_disas_cmd,
    &null_cmd
  };

struct pk_trie *vm_trie;

struct pk_cmd vm_cmd =
  {"vm", "", "", 0, &vm_trie, NULL, "vm (disassemble)"};
