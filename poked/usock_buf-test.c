
#include "usock_buf-priv.h"

#include <assert.h>
#include <string.h>

int
main ()
{
  struct usock_buf *b;
  struct usock_buf *b1;
  struct usock_buf *b2;
  struct usock_buf *b3;
  size_t len;

  b = usock_buf_new_size (100);
  assert (b);
  assert (b->next == NULL);
  assert (b->prev == NULL);
  assert (usock_buf_next (b) == NULL);
  assert (b->tag == (uint64_t)-1);
  assert (usock_buf_tag (b) == (uint64_t)-1);
  assert (b->cap == 100 + 1);
  assert (b->len == 0);
  assert (USOCK_BUF_DATA (b) == usock_buf_data (b, &len));
  assert (b->len == len);
  memset (USOCK_BUF_DATA (b), '1', 100);
  assert (strlen ((char *)USOCK_BUF_DATA (b)) == 100);
  usock_buf_free (b);

  b = usock_buf_new_prefix ("hello", 5, ", world!", 6 + /*nul*/ 1);
  assert (b);
  usock_buf_free_chain (b);

  b = usock_buf_new_prefix ("(1) hello", 9, ", world!", 8 + /*nul*/ 1);
  assert (b);
  assert (b->prev == NULL);
  assert (b->next == NULL);
  b1 = usock_buf_new ("(2) hello, world!", 17 + 1);
  assert (b1);
  assert (b1->prev == NULL);
  assert (b1->next == NULL);
  assert (usock_buf_chain (b, b1) == b);
  assert (b->prev == NULL);
  assert (b->next == b1);
  assert (b1->prev == b);
  assert (b1->next == NULL);

  assert (usock_buf_chain_rev (b) == b1);
  assert (b1->prev == NULL);
  assert (b1->next == b);
  assert (b->prev == b1);
  assert (b->next == NULL);

  assert (usock_buf_chain_rev (b1) == b);
  assert (b->prev == NULL);
  assert (b->next == b1);
  assert (b1->prev == b);
  assert (b1->next == NULL);

  b2 = usock_buf_new ("(3) hello, world!", 17 + 1);
  assert (b2);
  assert (b2->prev == NULL);
  assert (b2->next == NULL);
  assert (usock_buf_chain (b, b2) == b);
  assert (b2->prev == b1);
  assert (b2->next == NULL);
  assert (b1->prev == b);
  assert (b1->next == b2);
  assert (b->prev == NULL);
  assert (b->next == b1);

  {
    struct usock_buf *cur = b;

    assert (cur);
    assert (strcmp ((char *)usock_buf_data (cur, &len), "(1) hello, world!")
            == 0);
    assert (len == 17 + 1);

    cur = usock_buf_next (cur);
    assert (cur);
    assert (strcmp ((char *)usock_buf_data (cur, &len), "(2) hello, world!")
            == 0);
    assert (len == 17 + 1);

    cur = usock_buf_next (cur);
    assert (cur);
    assert (strcmp ((char *)usock_buf_data (cur, &len), "(3) hello, world!")
            == 0);
    assert (len == 17 + 1);

    cur = usock_buf_next (cur);
    assert (cur == NULL);
  }

  assert (usock_buf_chain_rev (b) == b2);
  assert (b2->prev == NULL);
  assert (b2->next == b1);
  assert (b1->prev == b2);
  assert (b1->next == b);
  assert (b->prev == b1);
  assert (b->next == NULL);

  {
    struct usock_buf *cur;

    cur = b2;
    assert (cur);
    assert (strcmp ((char *)usock_buf_data (cur, &len), "(3) hello, world!")
            == 0);
    assert (len == 17 + 1);

    cur = usock_buf_next (cur);
    assert (cur);
    assert (strcmp ((char *)usock_buf_data (cur, &len), "(2) hello, world!")
            == 0);
    assert (len == 17 + 1);

    cur = usock_buf_next (cur);
    assert (cur);
    assert (strcmp ((char *)usock_buf_data (cur, &len), "(1) hello, world!")
            == 0);
    assert (len == 17 + 1);

    cur = usock_buf_next (cur);
    assert (cur == NULL);
  }

  b3 = usock_buf_new ("(4) hello, world!", 17 + 1);
  assert (b3);
  assert (b3->prev == NULL);
  assert (b3->next == NULL);

  assert (usock_buf_chain (b3, b2) == b3);
  assert (b->prev == b1);
  assert (b->next == NULL);
  assert (b1->prev == b2);
  assert (b1->next == b);
  assert (b2->prev == b3);
  assert (b2->next == b1);
  assert (b3->prev == NULL);
  assert (b3->next == b2);

  {
    struct usock_buf *cur;

    cur = b3;
    assert (cur);
    assert (cur->prev == NULL);
    assert (strcmp ((char *)usock_buf_data (cur, &len), "(4) hello, world!")
            == 0);
    assert (len == 17 + 1);

    cur = usock_buf_next (cur);
    assert (cur);
    assert (cur->prev == b3);
    assert (strcmp ((char *)usock_buf_data (cur, &len), "(3) hello, world!")
            == 0);
    assert (len == 17 + 1);

    cur = usock_buf_next (cur);
    assert (cur);
    assert (cur->prev == b2);
    assert (strcmp ((char *)usock_buf_data (cur, &len), "(2) hello, world!")
            == 0);
    assert (len == 17 + 1);

    cur = usock_buf_next (cur);
    assert (cur);
    assert (cur->prev == b1);
    assert (strcmp ((char *)usock_buf_data (cur, &len), "(1) hello, world!")
            == 0);
    assert (len == 17 + 1);

    cur = usock_buf_next (cur);
    assert (cur == NULL);
  }

  assert (usock_buf_chain_rev (b3) == b);

  {
    struct usock_buf *cur;

    cur = b;
    assert (cur);
    assert (strcmp ((char *)usock_buf_data (cur, &len), "(1) hello, world!")
            == 0);
    assert (len == 17 + 1);

    cur = usock_buf_next (cur);
    assert (cur);
    assert (strcmp ((char *)usock_buf_data (cur, &len), "(2) hello, world!")
            == 0);
    assert (len == 17 + 1);

    cur = usock_buf_next (cur);
    assert (cur);
    assert (strcmp ((char *)usock_buf_data (cur, &len), "(3) hello, world!")
            == 0);
    assert (len == 17 + 1);

    cur = usock_buf_next (cur);
    assert (cur);
    assert (strcmp ((char *)usock_buf_data (cur, &len), "(4) hello, world!")
            == 0);
    assert (len == 17 + 1);

    cur = usock_buf_next (cur);
    assert (cur == NULL);
  }

  assert (usock_buf_chain_rev (b2) == b3);

  {
    struct usock_buf *cur;

    cur = b;
    assert (cur);
    assert (strcmp ((char *)usock_buf_data (cur, &len), "(1) hello, world!")
            == 0);
    assert (len == 17 + 1);

    cur = usock_buf_next (cur);
    assert (cur);
    assert (strcmp ((char *)usock_buf_data (cur, &len), "(2) hello, world!")
            == 0);
    assert (len == 17 + 1);

    cur = usock_buf_next (cur);
    assert (cur);
    assert (strcmp ((char *)usock_buf_data (cur, &len), "(4) hello, world!")
            == 0);
    assert (len == 17 + 1);

    cur = usock_buf_next (cur);
    assert (cur);
    assert (strcmp ((char *)usock_buf_data (cur, &len), "(3) hello, world!")
            == 0);
    assert (len == 17 + 1);

    cur = usock_buf_next (cur);
    assert (cur == NULL);
  }

  usock_buf_free_chain (b);

  return 0;
}
