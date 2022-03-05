
#include <assert.h>
#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include <sys/types.h>

#include <sys/socket.h>
#include <sys/un.h>
#include <unistd.h>

#include <err.h>

#include <capstone/capstone.h>

#define CFG(F)                                                                 \
  F(x86_32, CS_ARCH_X86, CS_MODE_32)                                           \
  F(x86_64, CS_ARCH_X86, CS_MODE_64)                                           \
  F(arm, CS_ARCH_ARM, 0)                                                       \
  F(arm_thumb, CS_ARCH_ARM, CS_MODE_THUMB)                                     \
  F(aarch64, CS_ARCH_ARM64, 0)                                                 \
  F(m68k, CS_ARCH_M680X, CS_MODE_M68K_000)

enum
{
#define F(a, b, c) +1
  N_CFG = 0 + CFG(F),
#undef F
};
static const char* cfg_names[N_CFG] = {
#define F(a, b, c) #a,
  CFG(F)
#undef F
};
enum
{
#define F(a, b, c) m_##a,
  CFG(F)
#undef F
};
static csh cfg_handles[N_CFG];

static void
disasm(int idx, const uint8_t* code, size_t len, uint64_t adr);

static int
poke_connect(const char* path, uint8_t role);

static int
input_read(int fd, uint8_t** mem, size_t* len, uint64_t* adr);

int
main(int argc, char* argv[])
{
  int fd;
  int idx;
  uint8_t* mem = NULL;
  size_t len;
  uint64_t adr;

#define F(a, b, c)                                                             \
  if (cs_open(b, c, &cfg_handles[m_##a]) != CS_ERR_OK)                         \
    errx(1, "cs_open(" #b ", " #c ") failed");
  CFG(F)
#undef F

  fd = poke_connect(argc > 1 ? argv[1] : "poked.ipc", 0x83);
  assert(fd != -1);

  for (; 1; free(mem)) {
    idx = input_read(fd, &mem, &len, &adr);
    if (!(0 <= idx && idx < N_CFG))
      continue;
    disasm(idx, mem, len, adr);
  }

#define F(a, b, c) cs_close(&cfg_handles[m_##a]);
  CFG(F)
#undef F
  return 0;
}

static size_t N;

static void
disasm(int idx, const uint8_t* code, size_t len, uint64_t adr)
{
  cs_insn* insn;
  size_t count;

  printf("//--- %zu (%s)\n", N++, cfg_names[idx]);
  count = cs_disasm(cfg_handles[idx], code, len, adr, /*count*/ 0, &insn);
  if (count > 0) {
    size_t j;

    for (j = 0; j < count; j++) {
      printf("0x%" PRIx64 ":\t%s\t\t%s\n",
             insn[j].address,
             insn[j].mnemonic,
             insn[j].op_str);
    }

    cs_free(insn, count);
  } else
    printf("ERROR: Failed to disassemble given code!\n");
}

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

static void
read_n_bytes(int fd, void* mem, size_t len)
{
  ssize_t n;
  size_t off = 0;

  while (off != len) {
    n = read(fd, mem + off, len - off);
    if (n == 0)
      errx(1, "EOF");
    if (n == -1)
      err(1, "read() failed");
    off += n;
  }
}

static int
input_read(int fd, uint8_t** binary, size_t* len, uint64_t* adr)
{
  uint8_t* in = NULL;
  int inlen;
  char* name;
  uint8_t* p;
  int idx;

#define ensure(cond)                                                           \
  do {                                                                         \
    if (!(cond)) {                                                             \
      free(in);                                                                \
      return -1;                                                               \
    }                                                                          \
  } while (0)

  {
    uint8_t lbuf[2];

    read_n_bytes(fd, lbuf, 2);
    inlen = (size_t)lbuf[1] << 8 | lbuf[0];
  }
  in = malloc(inlen + 1);
  in[inlen] = '\0'; // safety net :)
  if (in == NULL)
    err(1, "malloc() failed");
  read_n_bytes(fd, in, inlen);

  name = (char*)in;
  p = in + strlen(name) + 1;
  ensure(p - in < inlen);
  ensure(p + 8 - in < inlen);

  // check validity of name
  idx = -1;
  for (int i = 0; i < N_CFG; ++i)
    if (strcmp(name, cfg_names[i]) == 0) {
      idx = i;
      break;
    }
  ensure(idx != -1);

  *adr = (uint64_t)p[7] << 56 | (uint64_t)p[6] << 48 | (uint64_t)p[5] << 40 |
         (uint64_t)p[4] << 32 | (uint64_t)p[3] << 24 | (uint64_t)p[2] << 16 |
         (uint64_t)p[1] << 8 | (uint64_t)p[0] << 0;
  p += 8;
  *len = inlen - (p - in);
  memmove(in, p, *len);

#undef ensure

  *binary = in;
  return idx;
}
