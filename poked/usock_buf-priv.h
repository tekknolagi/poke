/* usock_buf-priv.h - Private header for usock_buf.  */

/* Copyright (C) 2022 Mohammad-Reza Nabipoor */
/* SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef USOCK_BUF_PRIV_H_
#define USOCK_BUF_PRIV_H_

#include "usock_buf.h"

// small-buffer size
#define USOCK_BUF_SBUFSZ 23

struct usock_buf_data
{
  int refcount;
  unsigned char *bytes;
};

// Buffer structure.
//
// This buffer employs small-data optimization and put short data inside
// this structure and saves one extra memory allocation.
struct usock_buf
{
  struct usock_buf *next;
  struct usock_buf *prev;
  uint64_t tag; // user-defined interpretation
  size_t cap;
  size_t len;
  union
  {
    unsigned char bytes[USOCK_BUF_SBUFSZ + 1];
    struct usock_buf_data *data;
  };
};

// For developers to make sure the usock_buf is cache-friendly on 64-bit
// machines.
// _Static_assert(sizeof(void*) == 8 && sizeof(struct usock_buf) == 64, "");

#define USOCK_BUF_SHORTBUF_P(b) (b->cap <= sizeof (b->bytes))
#define USOCK_BUF_DATA(b)                                                     \
  (USOCK_BUF_SHORTBUF_P (b) ? (unsigned char *)b->bytes : b->data->bytes)

// Allocates a buffer which can hold CAP bytes of user data (plus one extra
// null byte at the end).
struct usock_buf *usock_buf_new_size (size_t cap);

// Duplicate current node of the buffer. If buffer has a remote part (if the
// data is not short enough to fit in the structure itself), the refcount of
// remote part will be incremented.
struct usock_buf *usock_buf_dup (struct usock_buf *b);

// Duplicate the current node and all the chains. See USOCK_BUF_DUP.
struct usock_buf *usock_buf_dup_chain (struct usock_buf *b);

// Allocate a new buffer and initialize the data with DATA of length LEN.
// Pre-condition: data != NULL
// Pre-condition: len != 0
struct usock_buf *usock_buf_new (const char *data, size_t len);

// Allocate a new buffer and initialize the data with PREFIX and DATA.
// Pre-condition: data != NULL
// Pre-condition: len != 0
struct usock_buf *usock_buf_new_prefix (const void *prefix, size_t prelen,
                                        const char *data, size_t len);

// Append buffer B to the end of buffer BS chain and return BS.
// If BS is null, return B.
struct usock_buf *__attribute__ ((warn_unused_result))
usock_buf_chain (struct usock_buf *bs, struct usock_buf *b);

// Prepend the whole chain of buffer B before buffer BS and return the B.
// Pre-condition: b != NULL
struct usock_buf *__attribute__ ((warn_unused_result))
usock_buf_chain_pre (struct usock_buf *bs, struct usock_buf *b);

// Reverse the chained buffers.
struct usock_buf *usock_buf_chain_rev (struct usock_buf *bs);

#endif
