/* Dirty/Initial proof-of-concept based on libfuse passthrough example */

/*
  FUSE: Filesystem in Userspace
  Copyright (C) 2001-2007  Miklos Szeredi <miklos@szeredi.hu>
  Copyright (C) 2011       Sebastian Pipping <sebastian@pipping.org>
  Copyright (C) 2022       Mohammad-Reza Nabipoor <mnabipoor@gnu.org>

  SPDX-License-Identifier: GPL-2.0
*/

/* gcc -Wall poke-fuse.c `pkg-config fuse3 --cflags --libs` -o poke-fuse
 */

#define FUSE_USE_VERSION 31

#ifdef linux
/* For pread()/pwrite()/utimensat() */
#define _XOPEN_SOURCE 700
#endif

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <fuse.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#ifdef __FreeBSD__
#include <sys/socket.h>
#include <sys/un.h>
#endif
#include <sys/time.h>
#ifdef HAVE_SETXATTR
#include <sys/xattr.h>
#endif

#include "poke-fuse_helpers.h"

#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <sys/types.h>

#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#include "read-file.h"
#include <err.h>

static int fill_dir_plus = 0;

static void*
xmp_init(struct fuse_conn_info* conn, struct fuse_config* cfg)
{
  (void)conn;
  cfg->use_ino = 1;

  /* Pick up changes from lower filesystem right away. This is
     also necessary for better hardlink support. When the kernel
     calls the unlink() handler, it does not know the inode of
     the to-be-removed entry and can therefore not invalidate
     the cache of the associated inode - resulting in an
     incorrect st_nlink value being reported for any remaining
     hardlinks to this inode. */
  cfg->entry_timeout = 0;
  cfg->attr_timeout = 0;
  cfg->negative_timeout = 0;

  return NULL;
}

static int
xmp_getattr(const char* path, struct stat* stbuf, struct fuse_file_info* fi)
{
  (void)fi;
  int res;

  res = lstat(path, stbuf);
  if (res == -1)
    return -errno;

  return 0;
}

static int
xmp_access(const char* path, int mask)
{
  int res;

  res = access(path, mask);
  if (res == -1)
    return -errno;

  return 0;
}

static int
xmp_readlink(const char* path, char* buf, size_t size)
{
  int res;

  res = readlink(path, buf, size - 1);
  if (res == -1)
    return -errno;

  buf[res] = '\0';
  return 0;
}

static int
xmp_readdir(const char* path,
            void* buf,
            fuse_fill_dir_t filler,
            off_t offset,
            struct fuse_file_info* fi,
            enum fuse_readdir_flags flags)
{
  DIR* dp;
  struct dirent* de;

  (void)offset;
  (void)fi;
  (void)flags;

  dp = opendir(path);
  if (dp == NULL)
    return -errno;

  while ((de = readdir(dp)) != NULL) {
    struct stat st;
    memset(&st, 0, sizeof(st));
    st.st_ino = de->d_ino;
    st.st_mode = de->d_type << 12;
    if (filler(buf, de->d_name, &st, 0, fill_dir_plus))
      break;
  }

  closedir(dp);
  return 0;
}

static int
xmp_mknod(const char* path, mode_t mode, dev_t rdev)
{
  int res;

  res = mknod_wrapper(AT_FDCWD, path, NULL, mode, rdev);
  if (res == -1)
    return -errno;

  return 0;
}

static int
xmp_mkdir(const char* path, mode_t mode)
{
  int res;

  res = mkdir(path, mode);
  if (res == -1)
    return -errno;

  return 0;
}

static int
xmp_unlink(const char* path)
{
  int res;

  res = unlink(path);
  if (res == -1)
    return -errno;

  return 0;
}

static int
xmp_rmdir(const char* path)
{
  int res;

  res = rmdir(path);
  if (res == -1)
    return -errno;

  return 0;
}

static int
xmp_symlink(const char* from, const char* to)
{
  int res;

  res = symlink(from, to);
  if (res == -1)
    return -errno;

  return 0;
}

static int
xmp_rename(const char* from, const char* to, unsigned int flags)
{
  int res;

  if (flags)
    return -EINVAL;

  res = rename(from, to);
  if (res == -1)
    return -errno;

  return 0;
}

static int
xmp_link(const char* from, const char* to)
{
  int res;

  res = link(from, to);
  if (res == -1)
    return -errno;

  return 0;
}

static int
xmp_chmod(const char* path, mode_t mode, struct fuse_file_info* fi)
{
  (void)fi;
  int res;

  res = chmod(path, mode);
  if (res == -1)
    return -errno;

  return 0;
}

static int
xmp_chown(const char* path, uid_t uid, gid_t gid, struct fuse_file_info* fi)
{
  (void)fi;
  int res;

  res = lchown(path, uid, gid);
  if (res == -1)
    return -errno;

  return 0;
}

static int
xmp_truncate(const char* path, off_t size, struct fuse_file_info* fi)
{
  int res;

  if (fi != NULL)
    res = ftruncate(fi->fh, size);
  else
    res = truncate(path, size);
  if (res == -1)
    return -errno;

  return 0;
}

#ifdef HAVE_UTIMENSAT
static int
xmp_utimens(const char* path,
            const struct timespec ts[2],
            struct fuse_file_info* fi)
{
  (void)fi;
  int res;

  /* don't use utime/utimes since they follow symlinks */
  res = utimensat(0, path, ts, AT_SYMLINK_NOFOLLOW);
  if (res == -1)
    return -errno;

  return 0;
}
#endif

static int
xmp_create(const char* path, mode_t mode, struct fuse_file_info* fi)
{
  int res;

  res = open(path, fi->flags, mode);
  if (res == -1)
    return -errno;

  fi->fh = res;
  return 0;
}

static int
xmp_open(const char* path, struct fuse_file_info* fi)
{
  int res;

  res = open(path, fi->flags);
  if (res == -1)
    return -errno;

  fi->fh = res;
  return 0;
}

static int
xmp_read(const char* path,
         char* buf,
         size_t size,
         off_t offset,
         struct fuse_file_info* fi)
{
  int fd;
  int res;

  if (fi == NULL)
    fd = open(path, O_RDONLY);
  else
    fd = fi->fh;

  if (fd == -1)
    return -errno;

  res = pread(fd, buf, size, offset);
  if (res == -1)
    res = -errno;

  if (fi == NULL)
    close(fd);
  return res;
}

static int poked_code_fd = -1;
static int poked_cmd_fd = -1;

static void
write_n_bytes(int fd, void* mem, size_t len)
{
  ssize_t n;
  size_t off = 0;

  while (off != len) {
    n = write(fd, mem + off, len - off);
    if (n == -1)
      err(1, "write() failed");
    off += n;
  }
}

static void
send_content(const char* path)
{
  const char* poked_txt;

  printf("[send_content] path:'%s'\n", path);
  if (poked_code_fd == -1 || poked_cmd_fd == -1)
    return;

  poked_txt = strstr(path, "/poked.txt");
  printf("[send_content] %s\n", poked_txt);
  if (poked_txt == NULL || poked_txt[10] != '\0')
    return;

  size_t len;
  uint8_t len16le[2];
  char* content = read_file(path, O_RDONLY, &len);
  char* p = content;
  char* q = p;
  int cmd_p = 0;

  if (p == NULL)
    goto done;

  // "\n//!" -> code section
  // "\n!"   -> command

  // find "\n//!" or "\n!"
  q += strlen(p);
  p = q;
  while (1) {
    for (; content != p && *p != '!'; --p)
      ;
    if (content == p)
      break;

    assert(*p == '!');

    if (p[-1] == '\n') {
      ++p;
      cmd_p = 1;
      break; // found "\n!", a command
    } else if ((p - content > 2) && strncmp(p - 3, "\n//!", 4) == 0) {
      p -= 2;
      break; // found "\n//!", a code section
    } else
      --p; // find the next '!'
  }
  len = q - p;

  printf("[send_content] p:'%s' len:%zu\n", p, len);
  if (len == 0 || len > 0xffff)
    goto done;
  if (cmd_p)
    p[len] = ';'; // replace '\0'
  len += cmd_p;
  len16le[0] = len;
  len16le[1] = len >> 8;
  write_n_bytes(cmd_p ? poked_cmd_fd : poked_code_fd, len16le, 2);
  write_n_bytes(cmd_p ? poked_cmd_fd : poked_code_fd, p, len);

done:
  free(content);
}

static int
xmp_write(const char* path,
          const char* buf,
          size_t size,
          off_t offset,
          struct fuse_file_info* fi)
{
  int fd;
  int res;

  (void)fi;
  if (fi == NULL)
    fd = open(path, O_WRONLY);
  else
    fd = fi->fh;

  if (fd == -1)
    return -errno;

  res = pwrite(fd, buf, size, offset);
  if (res == -1)
    res = -errno;

  if (fi == NULL)
    close(fd);

  send_content(path);
  return res;
}

static int
xmp_statfs(const char* path, struct statvfs* stbuf)
{
  int res;

  res = statvfs(path, stbuf);
  if (res == -1)
    return -errno;

  return 0;
}

static int
xmp_release(const char* path, struct fuse_file_info* fi)
{
  (void)path;
  close(fi->fh);
  return 0;
}

static int
xmp_fsync(const char* path, int isdatasync, struct fuse_file_info* fi)
{
  /* Just a stub.	 This method is optional and can safely be left
     unimplemented */

  (void)path;
  (void)isdatasync;
  (void)fi;
  return 0;
}

#ifdef HAVE_POSIX_FALLOCATE
static int
xmp_fallocate(const char* path,
              int mode,
              off_t offset,
              off_t length,
              struct fuse_file_info* fi)
{
  int fd;
  int res;

  (void)fi;

  if (mode)
    return -EOPNOTSUPP;

  if (fi == NULL)
    fd = open(path, O_WRONLY);
  else
    fd = fi->fh;

  if (fd == -1)
    return -errno;

  res = -posix_fallocate(fd, offset, length);

  if (fi == NULL)
    close(fd);
  return res;
}
#endif

#ifdef HAVE_SETXATTR
/* xattr operations are optional and can safely be left unimplemented */
static int
xmp_setxattr(const char* path,
             const char* name,
             const char* value,
             size_t size,
             int flags)
{
  int res = lsetxattr(path, name, value, size, flags);
  if (res == -1)
    return -errno;
  return 0;
}

static int
xmp_getxattr(const char* path, const char* name, char* value, size_t size)
{
  int res = lgetxattr(path, name, value, size);
  if (res == -1)
    return -errno;
  return res;
}

static int
xmp_listxattr(const char* path, char* list, size_t size)
{
  int res = llistxattr(path, list, size);
  if (res == -1)
    return -errno;
  return res;
}

static int
xmp_removexattr(const char* path, const char* name)
{
  int res = lremovexattr(path, name);
  if (res == -1)
    return -errno;
  return 0;
}
#endif /* HAVE_SETXATTR */

#ifdef HAVE_COPY_FILE_RANGE
static ssize_t
xmp_copy_file_range(const char* path_in,
                    struct fuse_file_info* fi_in,
                    off_t offset_in,
                    const char* path_out,
                    struct fuse_file_info* fi_out,
                    off_t offset_out,
                    size_t len,
                    int flags)
{
  int fd_in, fd_out;
  ssize_t res;

  if (fi_in == NULL)
    fd_in = open(path_in, O_RDONLY);
  else
    fd_in = fi_in->fh;

  if (fd_in == -1)
    return -errno;

  if (fi_out == NULL)
    fd_out = open(path_out, O_WRONLY);
  else
    fd_out = fi_out->fh;

  if (fd_out == -1) {
    close(fd_in);
    return -errno;
  }

  res = copy_file_range(fd_in, &offset_in, fd_out, &offset_out, len, flags);
  if (res == -1)
    res = -errno;

  if (fi_out == NULL)
    close(fd_out);
  if (fi_in == NULL)
    close(fd_in);

  return res;
}
#endif

static off_t
xmp_lseek(const char* path, off_t off, int whence, struct fuse_file_info* fi)
{
  int fd;
  off_t res;

  if (fi == NULL)
    fd = open(path, O_RDONLY);
  else
    fd = fi->fh;

  if (fd == -1)
    return -errno;

  res = lseek(fd, off, whence);
  if (res == -1)
    res = -errno;

  if (fi == NULL)
    close(fd);
  return res;
}

static const struct fuse_operations xmp_oper = {
  .init = xmp_init,
  .getattr = xmp_getattr,
  .access = xmp_access,
  .readlink = xmp_readlink,
  .readdir = xmp_readdir,
  .mknod = xmp_mknod,
  .mkdir = xmp_mkdir,
  .symlink = xmp_symlink,
  .unlink = xmp_unlink,
  .rmdir = xmp_rmdir,
  .rename = xmp_rename,
  .link = xmp_link,
  .chmod = xmp_chmod,
  .chown = xmp_chown,
  .truncate = xmp_truncate,
#ifdef HAVE_UTIMENSAT
  .utimens = xmp_utimens,
#endif
  .open = xmp_open,
  .create = xmp_create,
  .read = xmp_read,
  .write = xmp_write,
  .statfs = xmp_statfs,
  .release = xmp_release,
  .fsync = xmp_fsync,
#ifdef HAVE_POSIX_FALLOCATE
  .fallocate = xmp_fallocate,
#endif
#ifdef HAVE_SETXATTR
  .setxattr = xmp_setxattr,
  .getxattr = xmp_getxattr,
  .listxattr = xmp_listxattr,
  .removexattr = xmp_removexattr,
#endif
#ifdef HAVE_COPY_FILE_RANGE
  .copy_file_range = xmp_copy_file_range,
#endif
  .lseek = xmp_lseek,
};

static int
poke_connect(const char* path, uint8_t role)
{
  int fd = socket(AF_UNIX, SOCK_STREAM, 0);
  struct sockaddr_un adr;

  if (fd == -1)
    err(1, "socket() failed");
  memset(&adr, 0, sizeof(adr));
  adr.sun_family = AF_UNIX;
  snprintf(adr.sun_path, sizeof(adr.sun_path), path);
  if (connect(fd, (struct sockaddr*)&adr, sizeof(adr)) == -1)
    err(1, "connect() failed");

  if (write(fd, &role, 1) != 1)
    err(1, "write(role) failed");

  return fd;
}

int
main(int argc, char* argv[])
{
  enum
  {
    MAX_ARGS = 10
  };
  int i, new_argc;
  char* new_argv[MAX_ARGS];
  const char* poked_ipc = getenv("POKED");

  if (poked_ipc == NULL)
    poked_ipc = "poked.ipc";

  umask(0);
  /* Process the "--plus" option apart */
  for (i = 0, new_argc = 0; (i < argc) && (new_argc < MAX_ARGS); i++) {
    if (!strcmp(argv[i], "--plus")) {
      fill_dir_plus = FUSE_FILL_DIR_PLUS;
    } else {
      new_argv[new_argc++] = argv[i];
    }
  }

  poked_code_fd = poke_connect(poked_ipc, 0x01);
  if (poked_code_fd == -1)
    err(1, "poke_connect(0x01) failed");
  poked_cmd_fd = poke_connect(poked_ipc, 0x02);
  if (poked_code_fd == -1)
    err(1, "poke_connect(0x02) failed");

  return fuse_main(new_argc, new_argv, &xmp_oper, NULL);
}
