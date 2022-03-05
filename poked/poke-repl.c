
#include <ctype.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <sys/types.h>

#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#include <err.h>

#include "linenoise.h"

static int
poke_connect (const char *path, uint8_t role)
{
  int fd = socket (AF_UNIX, SOCK_STREAM, 0);
  struct sockaddr_un adr;

  if (fd == -1)
    err (1, "socket() failed");
  memset (&adr, 0, sizeof (adr));
  adr.sun_family = AF_UNIX;
  snprintf (adr.sun_path, sizeof (adr.sun_path), path);
  if (connect (fd, (struct sockaddr *)&adr, sizeof (adr)) == -1)
    err (1, "connect() failed");

  if (write (fd, &role, 1) != 1)
    err (1, "write(role) failed");

  return fd;
}

static void
write_n_bytes (int fd, char *mem, size_t len)
{
  ssize_t n;
  size_t off = 0;

  while (off != len)
    {
      n = write (fd, mem + off, len - off);
      if (n == -1)
        err (1, "write() failed");
      off += n;
    }
}

static int
str_isspace_p (const char *s)
{
  for (; *s != '\0'; ++s)
    if (!isspace (*s))
      return 0;
  return 1;
}

static int
cmd_proc (char **linep, size_t *sizep, const char *cmd)
{
  int is_cmd_p = 0;
  const char *c = cmd;
  size_t l;

#define DELTA 80

  while (*c && isspace (*c))
    ++c;
  if (*c == ';')
    {
      is_cmd_p = 1;
      ++c;
    }
  l = strlen (c);
  if (*sizep < l + /*;*/ 1)
    {
      void *p;

      p = realloc (*linep, l + DELTA);
      if (p == NULL)
        err (1, "realloc() failed");
      *linep = p;
      *sizep = l + DELTA;
    }
  snprintf (*linep, *sizep, "%s;", c);
  return is_cmd_p;

#undef DELTA
}

int
main (int argc, char *argv[])
{
  int fd_code;
  int fd_cmd;
  const char *prompt = "#!poke!# ";
  char *newline = NULL;
  size_t len = 128;
  char *line = malloc (len);
  int is_cmd_p = 0;

  if (line == NULL)
    err (1, "malloc() failed");
  fd_code = poke_connect (argc > 1 ? argv[1] : "poked.ipc", /*code*/ 0x01);
  fd_cmd = poke_connect (argc > 1 ? argv[1] : "poked.ipc", /*cmd*/ 0x02);
  linenoiseSetMultiLine (1);
  for (; (newline = linenoise (prompt)) != NULL; free (newline))
    {
      uint8_t len16le[2];

      if (str_isspace_p (newline))
        continue;
      linenoiseHistoryAdd (newline);
      is_cmd_p = cmd_proc (&line, &len, newline);
      len16le[0] = len;
      len16le[1] = len >> 8;
      write_n_bytes (is_cmd_p ? fd_cmd : fd_code, (void *)len16le, 2);
      write_n_bytes (is_cmd_p ? fd_cmd : fd_code, line, len & 0xffffu);
    }
  free (line);
  return 0;
}
