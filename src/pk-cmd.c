/* pk-cmd.c - Poke commands.  */

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
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <fcntl.h>
#include <assert.h>
#include <wordexp.h> /* For tilde-expansion.  */
#include <xalloc.h>
#include <ctype.h>
#include <gettext.h>
#define _(str) dgettext (PACKAGE, str)

#include "poke.h"
#include "pkl.h" /* For pkl_compile_buffer */
#include "pkl-parser.h"
#include "ios.h"
#include "pk-cmd.h"
#include "pk-term.h"

/* Table of supported commands.  */

extern struct pk_cmd file_cmd; /* pk-file.c  */
extern struct pk_cmd close_cmd; /* pk-file.c */
extern struct pk_cmd load_cmd; /* pk-file.c */
extern struct pk_cmd info_cmd; /* pk-info.c  */
extern struct pk_cmd exit_cmd; /* pk-misc.c  */
extern struct pk_cmd version_cmd; /* pk-misc.c */
extern struct pk_cmd help_cmd; /* pk-help.c */
extern struct pk_cmd vm_cmd; /* pk-vm.c  */
extern struct pk_cmd set_cmd; /* pk-set.c */
extern struct pk_cmd editor_cmd; /* pk-editor.c */

struct pk_cmd null_cmd = {};

static struct pk_cmd *dot_cmds[] =
  {
    &file_cmd,
    &exit_cmd,
    &version_cmd,
    &info_cmd,
    &close_cmd,
    &load_cmd,
    &help_cmd,
    &vm_cmd,
    &set_cmd,
    &editor_cmd,
    &null_cmd
  };

/* Convenience macros and functions for parsing.  */

static inline char *
skip_blanks (char *p)
{
  if (p)
    while (isblank (*p))
      p++;
  return p;
}

static inline int
pk_atoi (char **p, int64_t *number)
{
  long int li;
  char *end;

  errno = 0;
  li = strtoll (*p, &end, 0);
  if ((errno != 0 && li == 0)
      || end == *p)
    return 0;

  *number = li;
  *p = end;
  return 1;
}

/* Little implementation of prefix trees, or tries.  This is used in
   order to support calling to commands and subcommands using
   unambiguous prefixes.  It is also a pretty efficient way to decode
   command names.  */

struct pk_trie
{
  char c;
  struct pk_trie *parent;
  int num_children;
  struct pk_trie *children[256];
  struct pk_cmd *cmd;
};

static struct pk_trie *
pk_trie_new (char c, struct pk_trie *parent)
{
  struct pk_trie *trie;
  size_t i;

  trie = xmalloc (sizeof (struct pk_trie));
  trie->c = c;
  trie->parent = parent;
  trie->cmd = NULL;
  trie->num_children = 0;
  for (i = 0; i < 256; i++)
    trie->children[i] = NULL;

  return trie;
}

static void
pk_trie_free (struct pk_trie *trie)
{
  int i;

  if (trie == NULL)
    return;

  for (i = 0; i < 256; i++)
    pk_trie_free (trie->children[i]);

  free (trie);
  return;
}

static void
pk_trie_expand_cmds (struct pk_trie *root,
                     struct pk_trie *trie)
{
  size_t i;
  struct pk_trie *t;

  if (trie->cmd != NULL)
    {
      t = trie->parent;
      while (t != root && t->num_children == 1)
        {
          t->cmd = trie->cmd;
          t = t->parent;
        }
    }
  else
    for (i = 0; i < 256; i++)
      {
        if (trie->children[i] != NULL)
          pk_trie_expand_cmds (root, trie->children[i]);
      }
}

static struct pk_trie *
pk_trie_from_cmds (struct pk_cmd *cmds[])
{
  size_t i;
  struct pk_trie *root;
  struct pk_trie *t;
  struct pk_cmd *cmd;

  root = pk_trie_new (' ', NULL);
  t = root;

  for (i = 0, cmd = cmds[0];
       cmd->name != NULL;
       cmd = cmds[++i])
    {
      const char *p;

      for (p = cmd->name; *p != '\0'; p++)
        {
          int c = *p;

          if (t->children[c] == NULL)
            {
              t->num_children++;
              t->children[c] = pk_trie_new (c, t);
            }
          t = t->children[c];
        }

      /* Note this assumes no commands with empty names.  */
      t->cmd = cmd;
      t = root;
    }

  pk_trie_expand_cmds (root, root);
  return root;
}

static struct pk_cmd *
pk_trie_get_cmd (struct pk_trie *trie, const char *str)
{
  const char *pc;

  for (pc = str; *pc; pc++)
    {
      int n = *pc;

      if (trie->children[n] == NULL)
        return NULL;

      trie = trie->children[n];
    }

  return trie->cmd;
}

#if 0
static void
pk_print_trie (int indent, struct pk_trie *trie)
{
  size_t i;

  for (i = 0; i < indent; i++)
    printf (" ");
  printf ("TRIE:: '%c' cmd='%s'\n",
          trie->c, trie->cmd != NULL ? trie->cmd->name : "NULL");

  for (i =0 ; i < 256; i++)
    if (trie->children[i] != NULL)
      pk_print_trie (indent + 2, trie->children[i]);
}
#endif

/* Routines to execute a command.  */

#define MAX_CMD_NAME 18

static int
pk_cmd_exec_1 (char *str, struct pk_trie *cmds_trie, char *prefix)
{
#define GOTO_USAGE()                                                           \
  do {                                                                         \
    besilent = 0;                                                              \
    ret = 0;                                                                   \
    goto usage;                                                                \
  } while (1)
  int ret = 1;
  size_t i;
  char cmd_name[MAX_CMD_NAME], *p;
  struct pk_cmd *cmd;
  int argc = 0;
  struct pk_cmd_arg argv[8];
  uint64_t uflags;
  const char *a;
  int besilent = 0;

  /* Note that the sole purpose of `pointers' is to serve as a root
     (in the stack) for the GC, to prevent the boxed values in the
     program returned by pkl_compile_expression below to be collected.
     XXX this should be hidden by a pkl_prog abstraction most
     probably.  */
  void *pointers;

  /* Skip blanks, and return if the command is composed by only blank
     characters.  */
  p = skip_blanks (str);
  if (*p == '\0')
    return 0;

  /* Get the command name.  */
  i = 0;
  while (isalnum (*p) || *p == '_' || *p == '-' || *p == ':')
    cmd_name[i++] = *(p++);
  cmd_name[i] = '\0';

  /* Look for the command in the prefix table.  */
  cmd = pk_trie_get_cmd (cmds_trie, cmd_name);
  if (cmd == NULL)
    {
      if (prefix != NULL)
        pk_printf ("%s ", prefix);
      pk_printf (_("%s: command not found.\n"), cmd_name);
      return 0;
    }

  /* Process user flags.  */
  uflags = 0;
  if (*p == '/')
    {
      p++;
      while (isalpha (*p))
        {
          int fi;
          for (fi = 0; cmd->uflags[fi]; fi++)
            if (cmd->uflags[fi] == *p)
              {
                uflags |= 1 << fi;
                break;
              }

          if (cmd->uflags[fi] == '\0')
            {
              pk_printf (_("%s: invalid flag `%c'\n"), cmd_name, *p);
              return 0;
            }

          p++;
        }
    }

  /* If this command has subcommands, process them and be done.  */
  if (cmd->subtrie != NULL)
    {
      p = skip_blanks (p);
      if (*p == '\0')
        GOTO_USAGE();
      return pk_cmd_exec_1 (p, *cmd->subtrie, cmd_name);
    }

  /* Parse arguments.  */
  a = cmd->arg_fmt;
  while (*a != '\0')
    {
      /* Handle an argument. */
      int match = 0;

      p = skip_blanks (p);
      if (*a == '?' && ((*p == ',' || *p == '\0')))
        {
          if (*p == ',')
            p++;
          argv[argc].type = PK_CMD_ARG_NULL;
          match = 1;
        }
      else
        {
          if (*a == '?')
            a++;

          /* Try the different options, in order, until one succeeds or
             the next argument or the end of the input is found.  */
          while (*a != ',' && *a != '\0')
            {
              char *beg = p;

              switch (*a)
                {
                case 'e':
                  {
                    /* Compile a poke program.  */
                    pvm_routine routine;
                    char *end;
                    char *program_string;

                    program_string = p;
                    routine = pkl_compile_expression (poke_compiler,
                                                      program_string, &end,
                                                      &pointers);
                    if (routine != NULL)
                      {
                        argv[argc].val.routine = routine;
                        match = 1;

                        argv[argc].type = PK_CMD_ARG_EXP;
                        p = end;
                      }
                    else
                      /* The compiler should have emitted diagnostic
                         messages, so don't bother the user with the
                         usage message.  */
                      besilent = 1;

                    break;
                  }
                case 'i':
                case 'n':
                  /* Parse an integer or natural.  */
                  p = skip_blanks (p);
                  if (pk_atoi (&p, &(argv[argc].val.integer))
                      && (*a == 'i' || argv[argc].val.integer >= 0))
                    {
                      p = skip_blanks (p);
                      if (*p == ',' || *p == '\0')
                        {
                          argv[argc].type = PK_CMD_ARG_INT;
                          match = 1;
                        }
                    }

                  break;
                case 'a':
                  /* Parse an address.  */
                  p = skip_blanks (p);
                  if (pk_atoi (&p, &(argv[argc].val.addr)))
                    {
                      p = skip_blanks (p);
                      if (*p == ',' || *p == '\0')
                        {
                          argv[argc].type = PK_CMD_ARG_ADDR;
                          match = 1;
                        }
                    }

                  break;
                case 't':
                  /* Parse a #N tag.  */
                  p = skip_blanks (p);
                  if (*p == '#'
                      && p++
                      && pk_atoi (&p, &(argv[argc].val.tag))
                      && argv[argc].val.tag >= 0)
                    {
                      if (*p == ',' || *p == '\0' || isblank (*p))
                        {
                          argv[argc].type = PK_CMD_ARG_TAG;
                          match = 1;
                        }
                    }

                  break;
                case 's':
                  {
                    /* Parse a string.  */

                    char *end, *str;
                    size_t size;

                    end = skip_blanks (p);
                    while (*end != '\0' && *end != ',')
                      end++;

                    size = end - p + 1;
                    assert (size > 0);
                    str = xmalloc (size);
                    strncpy (str, p, size);

                    /* Trim trailing space.  */
                    end = str + strlen (str) - 1;
                    while (end > str && isspace ((unsigned char) *end))
                      end--;
                    end++;
                    *end = '\0';

                    argv[argc].type = PK_CMD_ARG_STR;
                    argv[argc].val.str = str;
                    p = end;
                    match = 1;
                    break;
                  }
                case 'f':
                  {
                    /* Parse a filename, doing tilde expansion.  */
                    size_t i;
                    wordexp_t exp_result;
                    char *end;
                    char *filename = xmalloc (strlen (p) + 1);

                    p = skip_blanks (p);
                    i = 0;
                    while (*p != '\0' && *p != ',')
                      filename[i++] = *(p++);
                    filename[i] = '\0';

                    /* Trim trailing space.  */
                    end = filename + strlen (filename) - 1;
                    while (end > filename && isspace ((unsigned char) *end))
                      end--;
                    end++;
                    *end = '\0';

                    if (filename[0] == '\0')
                      GOTO_USAGE();

                    switch (wordexp (filename, &exp_result, 0))
                      {
                      case 0: /* Successful.  */
                        break;
                      case WRDE_NOSPACE:
                        wordfree (&exp_result);
                      default:
                        GOTO_USAGE();
                      }

                    if (exp_result.we_wordc != 1)
                      {
                        wordfree (&exp_result);
                        GOTO_USAGE();
                      }

                    filename = xrealloc (filename,
                                         strlen (exp_result.we_wordv[0]) + 1);
                    strcpy (filename, exp_result.we_wordv[0]);
                    wordfree (&exp_result);

                    if (*p == ',' || *p == '\0')
                      {
                        argv[argc].type = PK_CMD_ARG_STR;
                        argv[argc].val.str = filename;
                        match = 1;
                      }

                    break;
                  }
                default:
                  /* This should NOT happen.  */
                  assert (0);
                }

              if (match)
                break;

              /* Rewind input and try next option.  */
              p = beg;
              a++;
            }
        }

      /* Boo, could not find valid input for this argument.  */
      if (!match)
        GOTO_USAGE();

      if (*p == ',')
        p++;

      /* Skip any further options for this argument.  */
      while (*a != ',' && *a != '\0')
        a++;
      if (*a == ',')
        a++;

      /* Ok, next argument!  */
      argc++;
    }

  /* Make sure there is no trailer contents in the input.  */
  p = skip_blanks (p);
  if (*p != '\0')
    GOTO_USAGE();

  /* Process command flags.  */
  if (cmd->flags & PK_CMD_F_REQ_IO
      && ios_cur () == NULL)
    {
      pk_puts (_("This command requires an IO space.  Use the `file' command."));
      return 0;
    }

  if (cmd->flags & PK_CMD_F_REQ_W)
    {
      ios cur_io = ios_cur ();
      if (cur_io == NULL
          || !(ios_mode (cur_io) & IOS_M_RDWR))
        {
          pk_puts (_("This command requires a writable IO space."));
          return 0;
        }
    }

  /* Call the command handler, passing the arguments.  */
  ret = (*cmd->handler) (argc, argv, uflags);

  besilent = 1;
  usage:
  /* Free arguments occupying memory.  */
  for (i = 0; i < argc; ++i)
    {
      if (argv[i].type == PK_CMD_ARG_STR)
        free (argv[i].val.str);
      if (argv[i].type == PK_CMD_ARG_EXP
          || argv[i].type == PK_CMD_ARG_DEF
          || argv[i].type == PK_CMD_ARG_STMT)
        pvm_destroy_routine (argv[i].val.routine);
    }

  if (!besilent)
    pk_printf (_("Usage: %s\n"), cmd->usage);

  return ret;
#undef GOTO_USAGE
}

extern struct pk_cmd *info_cmds[]; /* pk-info.c  */
extern struct pk_trie *info_trie; /* pk-info.c  */

extern struct pk_cmd *help_cmds[]; /* pk-help.c */
extern struct pk_trie *help_trie; /* pk-help.c */

extern struct pk_cmd *vm_cmds[]; /* pk-vm.c  */
extern struct pk_trie *vm_trie;  /* pk-vm.c  */

extern struct pk_cmd *vm_disas_cmds[];  /* pk-vm.c */
extern struct pk_trie *vm_disas_trie; /* pk-vm.c */

extern struct pk_cmd *set_cmds[]; /* pk-set.c */
extern struct pk_trie *set_trie; /* pk-set.c */

static struct pk_trie *cmds_trie;

int
pk_cmd_exec (char *str)
{
  /* If the first non-blank character in STR is a dot ('.'), then this
     is a poke command.  Dispatch it with pk_cmd_exec_1.  Otherwise,
     compile a Poke declaration or a statement and execute it.  */

  char *cmd = skip_blanks (str);

  if (*cmd == '.')
    return pk_cmd_exec_1 (cmd + 1, cmds_trie, NULL);
  else
    {
      char *ecmd, *end;
      pvm_val val;
      int what; /* 0 -> declaration, 1 -> statement */
      int retval = 1;

      ecmd = xmalloc (strlen (cmd) + 2);
      strcpy (ecmd, cmd);

      if (strncmp (ecmd, "defun ", 6) == 0
          || strncmp (ecmd, "defun\t", 6) == 0)
        what = 0;
      else
        {
          if (strncmp (ecmd, "defvar ", 6) == 0
              || strncmp (ecmd, "defvar\t", 6) == 0
              || strncmp (ecmd, "deftype ", 8) == 0
              || strncmp (ecmd, "deftype\t", 8) == 0)
            what = 0;
          else
            what = 1;
        }

      if (strncmp (ecmd, "defun ", 6) != 0
          && strncmp (ecmd, "defun\t", 6) != 0)
        strcat (ecmd, ";");

      if (what == 0)
        {
          /* Declaration.  */
          if (!pkl_compile_buffer (poke_compiler, ecmd, &end)) {
            retval = 0;
            goto cleanup;
          }
        }
      else
        {
          /* Statement.  */
          if (!pkl_compile_statement (poke_compiler, ecmd, &end, &val)) {
            retval = 0;
            goto cleanup;
          }

          if (val != PVM_NULL)
            {
              pvm_print_val (val, poke_obase, 0);
              pk_puts ("\n");
            }
        }

    cleanup:
      free(ecmd);
      return retval;
    }
}

int
pk_cmd_exec_script (const char *filename)
{
  FILE *fd = fopen (filename, "r");

  if (fd == NULL)
    {
      perror (filename);
      return 1;
    }

  /* Read commands from FD, one per line, and execute them.  Lines
     starting with the '#' character are comments, and ignored.
     Likewise, empty lines are also ignored.  */

  char *line = NULL;
  size_t line_len = 0;
  while (1)
    {
      int ret;

      /* Read a line from the file.  */
      errno = 0;
      ssize_t n = getline (&line, &line_len, fd);

      if (n == -1)
	{
	  if (errno != 0)
	    perror (filename);
	  break;
	}

      if (line[n - 1] == '\n')
	line[n - 1] = '\0';

      /* If the line is empty, or it starts with '#', or it contains
         just blank characters, just ignore it.  */
      if (line[0] == '#' || line[0] == '\0')
        continue;
      else
        {
          char *c = line;
          while (*c != '\0' && (*c == ' ' || *c == '\t'))
            c++;
          if (*c == '\0')
            continue;
        }

      /* Execute the line.  */
      ret = pk_cmd_exec (line);
      if (!ret)
        goto error;
    }

  free (line);
  fclose (fd);
  return 1;

 error:
  free (line);
  fclose (fd);
  return 0;
}

void
pk_cmd_init (void)
{
  cmds_trie = pk_trie_from_cmds (dot_cmds);
  info_trie = pk_trie_from_cmds (info_cmds);
  help_trie = pk_trie_from_cmds (help_cmds);
  vm_trie = pk_trie_from_cmds (vm_cmds);
  vm_disas_trie = pk_trie_from_cmds (vm_disas_cmds);
  set_trie = pk_trie_from_cmds (set_cmds);

  /* Compile commands written in Poke.  */
  {
    char *poke_cmdfile;

#define LOAD_PK_FILE(filename)                                          \
    do                                                                  \
      {                                                                 \
        poke_cmdfile = xmalloc (strlen (poke_datadir) + strlen ("/" filename) + 1); \
        strcpy (poke_cmdfile, poke_datadir);                            \
        strcat (poke_cmdfile, "/" filename);                            \
        if (!pkl_compile_file (poke_compiler, poke_cmdfile))            \
          exit (EXIT_FAILURE);                                          \
        free (poke_cmdfile);                                            \
      }                                                                 \
    while (0)

    LOAD_PK_FILE ("pk-cmd.pk");
    LOAD_PK_FILE ("pk-dump.pk");

#undef LOAD_PK_FILE
  }
}

void
pk_cmd_shutdown (void)
{
  pk_trie_free (cmds_trie);
  pk_trie_free (info_trie);
  pk_trie_free (help_trie);
  pk_trie_free (vm_trie);
  pk_trie_free (vm_disas_trie);
  pk_trie_free (set_trie);
}


/*  Return the name of the next command that matches X,LEN.  IDX is  a
    pointer to in integer used  to index into the set of matches.
    Returns the name of the next command in the set, or NULL if there
    are no more. The returned value must be freed by the caller.  */
char *
pk_cmd_get_next_match (int *idx, const char *x, size_t len)
{
  /* Dot commands */
  for (;;)
    {
      struct pk_cmd **c = dot_cmds + *idx;
      if (*c == &null_cmd)
	break;

      /* don't forget the null terminator of name */
      const size_t name_len = strlen ((*c)->name);
      char *name = xmalloc (name_len + 2);
      name[0] = '.';
      strncpy (name+1, (*c)->name, name_len + 1);
      if (0 !=  strncmp (name, x, len))
	{
	  free (name);
	  (*idx)++;
	  continue;
	}
      return name;
    }
  return NULL;
}


/* Search for a command which matches cmdname.
 Returns NULL if no such command exists.  */
struct pk_cmd *
pk_cmd_find (const char *cmdname)
{
  if (cmdname != NULL)
    {
      struct pk_cmd **c;
      for (c = dot_cmds; *c != &null_cmd; ++c)
	{
	  /* Check if the command name matches.
	     +1 to skip the leading '.' */
	  if (STREQ ((*c)->name, cmdname + 1))
	    return *c;
	}
    }
  return NULL;
}
