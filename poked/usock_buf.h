/* usock_buf.h - Public header for usock_buf.  */

/* Copyright (C) 2022 Mohammad-Reza Nabipoor */
/* SPDX-License-Identifier: GPL-3.0-or-later */

#ifndef USOCK_BUF_H_
#define USOCK_BUF_H_

#include <stddef.h>
#include <stdint.h>

struct usock_buf;

// Get a pointer to buffer's data which has length LEN
unsigned char *usock_buf_data (struct usock_buf *b, size_t *len);

// Consider the buffer's data as null-terminated string.
// NOTE All buffers in usock_buf are null-terminated.
char *usock_buf_str (struct usock_buf *b);

// Give the TAG associated with buffer. The interpretation is up to the user.
uint64_t usock_buf_tag (struct usock_buf *b);

// Give the next buffer in the chain.
struct usock_buf *usock_buf_next (struct usock_buf *b);

// Free current node of buffer B and give the next buffer in the chain.
struct usock_buf *usock_buf_free (struct usock_buf *b);

// Free the buffer chain starting from the current node.
void usock_buf_free_chain (struct usock_buf *b);

#endif
