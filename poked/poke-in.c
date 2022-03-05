
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <sys/types.h>

#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#include <err.h>

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

int
main (int argc, char *argv[])
{
  char *line = NULL;
  size_t len = 0;
  ssize_t n;
  int fd;
  uint8_t role = 1;

  if (argc > 1 && strcmp (argv[1], "-c") == 0)
    {
      role = 2;
      --argc;
      ++argv;
    }

  fd = poke_connect (argc > 1 ? argv[1] : "poked.ipc", role);
  assert (fd != -1);

  while ((n = getline (&line, &len, stdin)) != -1)
    {
      assert (n > 0);

      // For cmd channel
      if (role == 2)
        {
          if (line[n - 1] == '\n')
            line[n - 1] = ';';
          else if (n + 1 < (int)len)
            {
              line = realloc (line, len + 32);
              if (line == NULL)
                err (1, "realloc() failed");
              line[n] = ';';
              line[n + 1] = '\0';
              ++n;
            }
        }

      {
        uint16_t len16 = n;
        uint8_t len16le[2] = { n, n >> 8 };

        write_n_bytes (fd, (void *)len16le, sizeof (len16le));
        write_n_bytes (fd, line, len16);
      }
    }

  // FIXME write dtors
  return 0;
}
