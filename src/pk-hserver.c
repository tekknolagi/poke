/* pk-hserver.c - A terminal hyperlinks server for poke.  */

/* Copyright (C) 2019 Jose E. Marchesi */

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

#include <pthread.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#include <pk-cmd.h>
#include <pk-hserver.h>
#include <pk-term.h>
#include <pk-repl.h>

#define STREQ(a, b) (strcmp (a, b) == 0)

/* The app:// protocol defines a maximum length of messages of two
   kilobytes.  */
#define MAXMSG 2048

/* Thread that runs the server.  */
static pthread_t hserver_thread;

/* Socket used by the worker thread.  */
static int hserver_socket;

/* Port where the server listens for connections.  */
static int hserver_port = 0;
static char hserver_port_str[128];

/* hserver_finish is used to tell the server threads to terminate.  It
   is protected with a mutex.  */
static int hserver_finish;
static pthread_mutex_t hserver_mutex = PTHREAD_MUTEX_INITIALIZER;

/* The server maintains a table with tokens.  Each hyperlink uses its
   own unique token, which is included in the payload and checked upon
   connection.  */

#define NUM_TOKENS 2014
int hserver_tokens[NUM_TOKENS];

int
pk_hserver_get_token (void)
{
  int token;

  do
    token = rand () % NUM_TOKENS;
  while (hserver_tokens[token]);

  hserver_tokens[token] = 1;
  return token;
}

static int
make_socket (uint16_t port)
{
  int sock;
  struct sockaddr_in name;
  
  /* Create the socket. */
  sock = socket (PF_INET, SOCK_STREAM, 0);
  if (sock < 0)
    {
      perror ("socket");
      exit (EXIT_FAILURE);
    }

  /* Give the socket a name. */
  name.sin_family = AF_INET;
  name.sin_port = htons (port);
  name.sin_addr.s_addr = htonl (INADDR_ANY);
  if (bind (sock, (struct sockaddr *) &name, sizeof (name)) < 0)
    {
      perror ("bind");
      exit (EXIT_FAILURE);
    }

  return sock;
}

static int
parse_int (char **p, int *number)
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

static int
read_from_client (int filedes)
{
  char buffer[MAXMSG];
  int nbytes;
  
  nbytes = read (filedes, buffer, MAXMSG);
  if (nbytes < 0)
    {
      /* Read error. */
      perror ("read");
      exit (EXIT_FAILURE);
    }
  else if (nbytes == 0)
    /* End-of-file. */
    return -1;
  else
    {
      int token;
      char cmd;
      char *p = buffer;

      /* Remove the newline at the end.  */
      buffer[nbytes-2] = '\0';
      
      /* The format of the payload is:
         [0-9]+/{e,i}/.*  */

      /* Get the token and check it.  */
      if (!parse_int (&p, &token))
        {
          printf ("PARSING INT\n");
          return 0;
        }
        
      if (!hserver_tokens[token])
        return 0;
      
      if (*p != '/')
        return 0;
      p++;

      cmd = *p;
      if (cmd != 'i' && cmd != 'e')
        return 0;
      p++;

      if (*p != '/')
        return 0;
      p++;
      
      switch (cmd)
        {
        case 'e':
          /* Command 'execute'.  */
          pthread_mutex_lock (&hserver_mutex);
          pk_repl_display_begin ();
          pk_puts (p);
          pk_puts ("\n");
          pk_cmd_exec (p);
          pk_repl_display_end ();
          pthread_mutex_unlock (&hserver_mutex);
          break;
        case 'i':
          /* Command 'insert'.  */
          pthread_mutex_lock (&hserver_mutex);
          pk_repl_insert (p);
          pthread_mutex_unlock (&hserver_mutex);
          break;
        default:
          break;
        }

      return 0;
    }
}

static void *
hserver_thread_worker (void *data)
{
  fd_set active_fd_set, read_fd_set;
  int i;
  struct sockaddr_in clientname;
  socklen_t size;

  /* Initialize the set of active sockets. */
  FD_ZERO (&active_fd_set);
  FD_SET (hserver_socket, &active_fd_set);

  while (1)
    {
      struct timeval timeout = { 0, 200000 };

      /* Block until input arrives on one or more active sockets.  */
      read_fd_set = active_fd_set;
      if (select (FD_SETSIZE, &read_fd_set, NULL, NULL, &timeout) < 0)
        {
          perror ("select");
          exit (EXIT_FAILURE);
        }

      /* Service all the sockets with input pending. */
      for (i = 0; i < FD_SETSIZE; ++i)
        if (FD_ISSET (i, &read_fd_set))
          {
            if (i == hserver_socket)
              {
                /* Connection request on original socket. */
                int new;
                size = sizeof (clientname);
                new = accept (hserver_socket,
                              (struct sockaddr *) &clientname,
                              &size);
                if (new < 0)
                  {
                    perror ("accept");
                    exit (EXIT_FAILURE);
                  }
                FD_SET (new, &active_fd_set);
              }
            else
              {
                /* Data arriving on an already-connected socket. */
                if (read_from_client (i) < 0)
                  {
                    close (i);
                    FD_CLR (i, &active_fd_set);
                  }
              }
          }

      pthread_mutex_lock (&hserver_mutex);
      if (hserver_finish)
        {
          pthread_mutex_unlock (&hserver_mutex);
          pthread_exit (NULL);
        }
      pthread_mutex_unlock (&hserver_mutex);
    }
}

void
pk_hserver_init ()
{
  int ret;
  int i;
  struct sockaddr_in clientname;
  socklen_t size;

  for (i = 0; i < NUM_TOKENS; ++i)
    hserver_tokens[i] = 0;

  /* Create the socket and set it up to accept connections. */
  hserver_socket = make_socket (hserver_port);
  if (listen (hserver_socket, 1) < 0)
    {
      perror ("listen");
      exit (EXIT_FAILURE);
    }

  /* Get a suitable ephemeral port and initialize hserver_port and
     hserver_port_str.  These will be used until the server shuts
     down.  */
  size = sizeof (clientname);
  if (getsockname (hserver_socket, &clientname, &size) != 0)
    {
      perror ("getsockname");
      exit (EXIT_FAILURE);
    }
  hserver_port = ntohs (clientname.sin_port);
  sprintf (hserver_port_str, "%d", hserver_port);

  hserver_finish = 0;
  ret = pthread_create (&hserver_thread,
                        NULL /* attr */,
                        hserver_thread_worker,
                        NULL);

  if (ret != 0)
    {
      errno = ret;
      perror ("pthread_create");
      exit (EXIT_FAILURE);
    }
}

void
pk_hserver_shutdown ()
{
  int ret;
  void *res;

  pthread_mutex_lock (&hserver_mutex);
  hserver_finish = 1;
  pthread_mutex_unlock (&hserver_mutex);

  ret = pthread_join (hserver_thread, &res);
  if (ret != 0)
    {
      errno = ret;
      perror ("pthread_join");
      exit (EXIT_FAILURE);
    }
}

char *
pk_hserver_make_hyperlink (char type,
                           const char *cmd)
{
  int token;
  char *str, token_str[128], type_str[2];
  char hostname[128];
  
  assert (type == 'i' || type == 'e');
  type_str[0] = type;
  type_str[1] = '\0';

  /* XXX: check for maximum length 2048.  */
  token = pk_hserver_get_token ();
  sprintf (token_str, "%d", token);
  
  if (gethostname (hostname, 128) != 0)
    {
      perror ("gethostname");
      exit (EXIT_FAILURE);
    }

  str = xmalloc (strlen ("app://")
                 + strlen (hostname)
                 + 1 /* ':' */
                 + strlen (hserver_port_str)
                 + 1 /* '/' */
                 + strlen (token_str)
                 + 1 /* '/' */
                 + 1 /* type */
                 + 1 /* '/' */
                 + strlen (cmd)
                 + 1 /* '0' */);
  strcpy (str, "app://");
  strcat (str, hostname);
  strcat (str, ":");
  strcat (str, hserver_port_str);
  strcat (str, "/");
  strcat (str, token_str);
  strcat (str, "/");
  strcat (str, type_str);
  strcat (str, "/");
  strcat (str, cmd);

  return str;
}

int
pk_hserver_port (void)
{
  return hserver_port;
}
