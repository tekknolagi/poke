#ifndef USOCK_H_
#define USOCK_H_

#include "usock_buf.h"

// Pre-defined channels
// (Channel is a 7-bit unsigned number)
#define USOCK_CHAN_IN_CODE 0x01
#define USOCK_CHAN_IN_CMD 0x02
#define USOCK_CHAN_OUT_OUT 0x01
#define USOCK_CHAN_OUT_VU 0x02
#define USOCK_CHAN_OUT_DISASM 0x03
#define USOCK_CHAN_OUT_TREEVU 0x04

struct usock;

// Allocates a new server over unix domain socket at PATH.
struct usock *usock_new (const char *path);

void usock_free (struct usock *u);

// Run the server loop.
// This should run on a separate thread.
void usock_serve (struct usock *u);

// Wake up the server thread to react to new data.
void usock_notify (struct usock *u);

// Signal the server thread to finish the server loop.
void usock_done (struct usock *u);

// Get all received data over chained buffers.
// This call is blocking and will block the calling thread.
struct usock_buf *usock_in (struct usock *u);

// Put DATA with length LEN in server's queue to be dispatched over
// channel CHAN.
// Non-zero KIND will be encoded as ULEB128 number before DATA.
// This call is non-blocking.
void usock_out (struct usock *u, uint32_t kind, uint8_t chan, const char *data,
                size_t len);

#endif
