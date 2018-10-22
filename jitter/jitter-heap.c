/* Jitter: memory heap data structure.

   Copyright (C) 2018 Luca Saiu
   Written by Luca Saiu

   This file is part of Jitter.

   Jitter is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Jitter is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jitter.  If not, see <http://www.gnu.org/licenses/>. */


#include <string.h> /* for memmove. */

#include "jitter-heap.h"
#include <jitter/jitter-fatal.h>


/* Heap utility.
 * ************************************************************************** */

/* Given a pointer to a thing header, its current tag (assumed to be correct)
   and a new tag, change the current tag with the new one.  This is more fragile
   but more efficient than a generic "set tag to" function: instead of and-ing
   off the old tag and then or-ing on a new one, the entire change can be done
   with just one arithmetic instruction, summing a known constant. */
inline static void
jitter_heap_change_tag (struct jitter_heap_thing *thing,
                        enum jitter_heap_thing_tag old_tag,
                        enum jitter_heap_thing_tag new_tag)
{
  thing->thing_on_the_left
    = JITTER_HEAP_RETAG_POINTER (thing->thing_on_the_left, old_tag, new_tag);
}

/* Given a thing payload size as requested by the user, return it rounded up so
   that it is usable, exactly, as a thing payload size.  The result respects the
   constraints on minimal size and alignment. */
inline static size_t
jitter_heap_payload_size_rounded_up (size_t payload_size_in_bytes)
{
  /* If the requested size is smaller than the minimum payload change it to the
     minimum, which is already correctly aligned; and we're done. */
  if (payload_size_in_bytes < JITTER_HEAP_MINIMUM_PAYLOAD_SIZE)
    return JITTER_HEAP_MINIMUM_PAYLOAD_SIZE;

  /* Otherwise, round up to a multiple of the alignment. */
  return (jitter_uint) JITTER_HEAP_ALIGN_RIGHT (payload_size_in_bytes);
}




/* Heap block initialization.
 * ************************************************************************** */

struct jitter_heap_block*
jitter_heap_initialize_block (void *allocated_space,
                              size_t allocated_size_in_bytes)
{
  /* The block header will be at the beginning of the allocated space, possibly
     skipping a few bytes if the allocated space is not correctly aligned. */
  struct jitter_heap_block *r
    = ((struct jitter_heap_block *)
       JITTER_HEAP_ALIGN_RIGHT(allocated_space));

  /* Compute usable block limits, on both sides. */
  char *past_block_end_unaligned
    = ((char *) allocated_space) + allocated_size_in_bytes;

  /* A just-initialized block will have exactly three things: one left
     terminator, one hole, and one right terminator.  Compute their header
     addresses, without storing anything into memory yet.  The left terminator
     is at a fixed offset from the block header beginning, so it is stored
     as a struct field within the block. */
  struct jitter_heap_thing *left_terminator_header
    = & r->left_terminator;
  struct jitter_heap_thing *hole_header
    = ((struct jitter_heap_thing *)
       JITTER_HEAP_ALIGN_RIGHT(left_terminator_header + 1));
  struct jitter_heap_thing *right_terminator_header
    = JITTER_HEAP_ALIGN_LEFT(past_block_end_unaligned
                             - sizeof (struct jitter_heap_thing));
  long hole_total_size
    = ((char*) right_terminator_header) - ((char*) hole_header);
  long hole_payload_size = hole_total_size - JITTER_HEAP_HEADER_OVERHEAD;

  /* Fail if the block is too small.  Block initialization should be infrequent
     enough for this check to be enabled unconditionally. */
  if (left_terminator_header >= hole_header
      || hole_header >= right_terminator_header)
    jitter_fatal ("initializing a block not large enough for initial blocks");
  if (hole_total_size <= JITTER_HEAP_MINIMUM_THING_SIZE)
    jitter_fatal ("initializing a block not large enough for one thing");
  if (hole_payload_size <= JITTER_HEAP_MINIMUM_PAYLOAD_SIZE)
    jitter_fatal ("initializing a block not large enough for one hole payload");

  /* Fill the left terminator header. */
  left_terminator_header->thing_on_the_left
    = JITTER_HEAP_TAG_POINTER(NULL, jitter_heap_thing_tag_terminator);
  left_terminator_header->payload_size_in_bytes = JITTER_HEAP_MINIMUM_PAYLOAD_SIZE;

  /* Fill the hole header. */
  hole_header->thing_on_the_left
    = JITTER_HEAP_TAG_POINTER(left_terminator_header,
                              jitter_heap_thing_tag_hole);
  hole_header->payload_size_in_bytes = hole_payload_size;

  /* Fill the right terminator header. */
  right_terminator_header->thing_on_the_left
    = JITTER_HEAP_TAG_POINTER(hole_header, jitter_heap_thing_tag_terminator);
  right_terminator_header->payload_size_in_bytes = JITTER_HEAP_MINIMUM_PAYLOAD_SIZE;

  /* Fill the block header. */
  r->allocated_space = allocated_space;
  r->allocated_space_size_in_bytes = allocated_size_in_bytes;
  /* Notice that I *don't* fill the links field here.  A block will be linked in
     the block list of a heap when it's part of a heap, but this doesn't happen
     automatically and doesn't need to happen every time.  Here we make no
     assumptions about even the existence of any heap. */

  /* Make the hole linked list.
     After we add both terminators as the first and last element, the list becomes
     and remains "always-nonempty" in the sense of jitter-list.h , which lets me
     use the more efficient _NONEMPTY list manipulation macros. */
  JITTER_LIST_INITIALIZE_HEADER (& r->hole_list);
  JITTER_LIST_LINK_FIRST (jitter_heap_thing, hole_links,
                          & (r->hole_list),
                          left_terminator_header);
  JITTER_LIST_LINK_LAST (jitter_heap_thing, hole_links,
                         & (r->hole_list),
                         right_terminator_header);
  JITTER_LIST_LINK_AFTER_NONEMPTY (jitter_heap_thing, hole_links,
                                   & (r->hole_list),
                                   left_terminator_header,
                                   hole_header);

  /* The block is now filled. */
  return r;
}




/* Object allocation, deallocation and reallocation from a given block.
 * ************************************************************************** */

/* Look for the first hole in the pointed block large enough to fit an object
   with the given payload size.  Return a pointer to the hole header if one
   exists, or NULL otherwise. */
static struct jitter_heap_thing*
jitter_heap_first_fit (struct jitter_heap_block *p,
                       size_t size_in_bytes)
{
  struct jitter_heap_thing *candidate = p->left_terminator.hole_links.next;
  while (JITTER_HEAP_GET_TAG (candidate) != jitter_heap_thing_tag_terminator)
    {
      if (candidate->payload_size_in_bytes >= size_in_bytes)
        return candidate;

      candidate = candidate->hole_links.next;
    }

  /* We've looked thru every hole in this block without finding any which was
     large enough. */
  return NULL;
}

void *
jitter_heap_allocate_from_block (struct jitter_heap_block *p,
                                 size_t object_payload_size_in_bytes)
{
  /* Round the size up, as required by the alignment. */
  object_payload_size_in_bytes
    = jitter_heap_payload_size_rounded_up (object_payload_size_in_bytes);

  /* Look for a hole.  If none large enough exists return NULL, and we're done. */
  struct jitter_heap_thing *hole
    = jitter_heap_first_fit (p, object_payload_size_in_bytes);
  if (hole == NULL)
    return NULL;

  /* If we arrived here we have found a suitable hole to fill, totally or in
     part.  Check if splitting the current hole into and object and a smaller
     hole would not violate the minimum size constraint.  The cast in the if
     condition is important here: without it JITTER_HEAP_MINIMUM_PAYLOAD_SIZE
     would be taken as unsigned. */
  long hole_payload_size_in_bytes = hole->payload_size_in_bytes;
  long smaller_hole_payload_size_in_bytes = (hole_payload_size_in_bytes
                                             - object_payload_size_in_bytes
                                             - JITTER_HEAP_HEADER_OVERHEAD);
  if (smaller_hole_payload_size_in_bytes >= (long) JITTER_HEAP_MINIMUM_PAYLOAD_SIZE)
    {
      /* The old hole is large enough to split. */

      /* It is more efficient to leave a hole on the left, so that we don't need
         to touch the hole list, instead only changing the payload size of the
         current hole.  The new object thing will fill the rightmost part of the
         old hole payload. */
      hole->payload_size_in_bytes = smaller_hole_payload_size_in_bytes;
      struct jitter_heap_thing *object =
        ((struct jitter_heap_thing *) (((char *) hole)
                                       + JITTER_HEAP_HEADER_OVERHEAD
                                       + smaller_hole_payload_size_in_bytes));
      object->thing_on_the_left
        = JITTER_HEAP_TAG_POINTER(hole, jitter_heap_thing_tag_object);
      object->payload_size_in_bytes = object_payload_size_in_bytes;
      struct jitter_heap_thing *object_on_the_right =
        ((struct jitter_heap_thing *) (((char *) object)
                                       + JITTER_HEAP_HEADER_OVERHEAD
                                       + object_payload_size_in_bytes));
      enum jitter_heap_thing_tag object_on_the_right_tag
        = JITTER_HEAP_GET_TAG (object_on_the_right);
      object_on_the_right->thing_on_the_left
        = JITTER_HEAP_TAG_POINTER (object, object_on_the_right_tag);
      return JITTER_HEAP_THING_TO_PAYLOAD (object);
    }
  else
    {
      /* The old hole is not large enough to split: I will replace it entirely
         with the new object. */

      /* The payload size remains the same since we are using it all, possibly
         introducing internal fragmentation.  The objects on the left and on the
         right of course remain the same; only the tag needs to change, and the
         thing, now no longer a hole, needs to be unlinked from the hole
         list. */
      jitter_heap_change_tag (hole, jitter_heap_thing_tag_hole,
                              jitter_heap_thing_tag_object);
      JITTER_LIST_UNLINK_NONEMPTY(jitter_heap_thing, hole_links,
                                  & (p->hole_list),
                                  hole);
      return JITTER_HEAP_THING_TO_PAYLOAD (hole);
    }
}

void
jitter_heap_free_from_block (struct jitter_heap_block *b,
                             void *payload)
{
  /* This function will either make a new hole or, whenever possible, turn one
     or two existing holes next to * thing into one bigger hole by coalescing.
     The new or updated hole will go to the beginning of the hole list. */

  /* From the payload, get a pointer to the thing we are freeing. */
  struct jitter_heap_thing *thing = JITTER_HEAP_PAYLOAD_TO_THING (payload);

  /* Get information about the thing on its left and on the right. */
  struct jitter_heap_thing *left = JITTER_HEAP_THING_ON_THE_LEFT_OF (thing);
  bool hole_on_the_left
    = JITTER_HEAP_GET_TAG (left) == jitter_heap_thing_tag_hole;
  struct jitter_heap_thing *right = JITTER_HEAP_THING_ON_THE_RIGHT_OF (thing);
  bool hole_on_the_right =
    JITTER_HEAP_GET_TAG (right) == jitter_heap_thing_tag_hole;

  /* The new hole will need to point to the thing on its left.  We will also
     need to change the thing_on_the_left pointer in the thing on the right of
     the hole we are making.  What that thing will be depends on whether the
     thing currently on the right ofthing is a hole (to be coalesced) or not.
     Unlink existing holes on the left and on the right, if any: they will be
     replaced with a new hole, in a different list position. */
  struct jitter_heap_thing *before_new_hole;
  struct jitter_heap_thing *new_hole;
  struct jitter_heap_thing *after_new_hole;
  if (hole_on_the_left)
    {
      new_hole = left;
      before_new_hole = JITTER_HEAP_THING_ON_THE_LEFT_OF (left);
      JITTER_LIST_UNLINK_NONEMPTY(jitter_heap_thing, hole_links, & (b->hole_list),
                                  left);
    }
  else
    {
      new_hole = thing;
      before_new_hole = left;
    }
  if (hole_on_the_right)
    {
      after_new_hole = JITTER_HEAP_THING_ON_THE_RIGHT_OF (right);
      JITTER_LIST_UNLINK_NONEMPTY(jitter_heap_thing, hole_links, & (b->hole_list),
                                  right);
    }
  else
    after_new_hole = right;
  enum jitter_heap_thing_tag after_new_hole_tag
    = JITTER_HEAP_GET_TAG (after_new_hole);

  /* Compute where the new hole begins and what its payload size is.  Once this
     is done I can fill in information about the new hole, and link it. */
  size_t new_hole_payload_size
    = ((((char *) after_new_hole) - ((char *) new_hole))
       - JITTER_HEAP_HEADER_OVERHEAD);
  new_hole->thing_on_the_left
    = JITTER_HEAP_TAG_POINTER (before_new_hole, jitter_heap_thing_tag_hole);
  new_hole->payload_size_in_bytes = new_hole_payload_size;

  /* Make the object after the new hole point to the hole by its
     thing_on_the_left field. */
  after_new_hole->thing_on_the_left
    = JITTER_HEAP_TAG_POINTER (new_hole, after_new_hole_tag);

  /* Link the new hole as the new first hole, right after the left
     terminator. */
  JITTER_LIST_LINK_AFTER_NONEMPTY (jitter_heap_thing, hole_links,
                                   & (b->hole_list),
                                   & b->left_terminator,
                                   new_hole);
}

/* The trivial version of reallocation: make a new thing, copy from the old
   thing, free the old thing.  This is called when it's difficult to do
   better. */
static void *
jitter_heap_reallocate_from_block_trivial (struct jitter_heap_block *b,
                                           void *old_payload,
                                           size_t user_new_size_in_bytes)
{
  /* Make a new object with the new size, or return NULL immediately if there is
     no space. */
  void *r = jitter_heap_allocate_from_block (b, user_new_size_in_bytes);
  if (r == NULL)
    return NULL;

  /* Look at the old object, and compute how many bytes we need to copy from it.
     Notice that the number of bytes to copy is exactly what the user passed to
     this function, and not the payload size rounded up by
     jitter_heap_allocate_from_block . */
  struct jitter_heap_thing *old_object
    = JITTER_HEAP_PAYLOAD_TO_THING (old_payload);
  size_t size_to_copy_in_bytes = old_object->payload_size_in_bytes;
  if (user_new_size_in_bytes < size_to_copy_in_bytes)
    size_to_copy_in_bytes = user_new_size_in_bytes;

  /* Fill the new object payload with a copy from the old one. */
  memmove (r, old_object->payload, size_to_copy_in_bytes);

  /* Free the old object. */
  jitter_heap_free_from_block (b, old_payload);
  return r;
}

/* Given a block, and object thing on the block and a hole thing immediately on
   its right, merge the object and the hole into a larger object.
   Of course no sanity check is performed: the given things must have the right
   tag and be in the right positions. */
static void
jitter_heap_merge_object_with_hole_on_its_right
   (struct jitter_heap_block *b,
    struct jitter_heap_thing *object,
    struct jitter_heap_thing *hole)
{
  /* Get a pointer to the object on the right of the hole (which must exist,
     possibly as a terminator): we will need to update its thing_on_the_right
     pointer. */
  struct jitter_heap_thing *after_hole
    = JITTER_HEAP_THING_ON_THE_RIGHT_OF (hole);
  enum jitter_heap_thing_tag after_hole_tag = JITTER_HEAP_GET_TAG (after_hole);

  /* Turn the object and the hole into one object. */
  size_t object_payload_size = object->payload_size_in_bytes;
  size_t hole_thing_size = (JITTER_HEAP_HEADER_OVERHEAD
                            + hole->payload_size_in_bytes);
  JITTER_LIST_UNLINK_NONEMPTY(jitter_heap_thing, hole_links,
                              & (b->hole_list), hole);
  object->payload_size_in_bytes = object_payload_size + hole_thing_size;

  /* Update the object after the removed hole, so that it can find what comes to
     its left. */
  after_hole->thing_on_the_left = JITTER_HEAP_TAG_POINTER (object,
                                                           after_hole_tag);
}

/* Given an existing object thing, shrink it in place by carving a hole on its
   right if there is sufficient space; when the thing on the right of the
   initial object is already a hole, coalesce it with the new one.
   The new object payload size must already respect the constraints on minimum
   size and alignment. */
static void
jitter_heap_shrink_object (struct jitter_heap_block *b,
                           struct jitter_heap_thing *object,
                           size_t new_object_payload_size_in_bytes)
{
  /* If the thing on the right of the object thing is a hole, remove the hole
     by having the object expand into it.  Otherwise just keep the information
     we have discovered about the thing on the right, to be used later. */
  struct jitter_heap_thing *thing_on_the_right
    = JITTER_HEAP_THING_ON_THE_RIGHT_OF (object);
  enum jitter_heap_thing_tag thing_on_the_right_tag
    = JITTER_HEAP_GET_TAG (thing_on_the_right);
  if (thing_on_the_right_tag == jitter_heap_thing_tag_hole)
    {
      jitter_heap_merge_object_with_hole_on_its_right (b, object,
                                                       thing_on_the_right);
      /* The thing on the right of object is now different. */
      thing_on_the_right
        = JITTER_HEAP_THING_ON_THE_RIGHT_OF (object);
      thing_on_the_right_tag
        = JITTER_HEAP_GET_TAG (thing_on_the_right);
    }
  /* From this point on we can assume that the object doesn't have a hole on its
     right. */

  /* Check if there is enough place for carving a new hole.  If not there is
     nothing to do: we have to live with internal fragmentation. */
  size_t old_object_payload_size_in_bytes = object->payload_size_in_bytes;
  size_t new_hole_thing_size_in_bytes
    = old_object_payload_size_in_bytes - new_object_payload_size_in_bytes;
  if (new_hole_thing_size_in_bytes < JITTER_HEAP_MINIMUM_THING_SIZE)
    return;

  /* Update the old object, which will keep existing.  The only field we
     need to change is its payload size.  */
  object->payload_size_in_bytes = new_object_payload_size_in_bytes;

  /* Make a new hole on its right. */
  char *old_payload = JITTER_HEAP_THING_TO_PAYLOAD (object);
  struct jitter_heap_thing *new_hole
    = ((struct jitter_heap_thing *)
       (((char *) old_payload) + new_object_payload_size_in_bytes));
  new_hole->payload_size_in_bytes
    = new_hole_thing_size_in_bytes - JITTER_HEAP_HEADER_OVERHEAD;
  new_hole->thing_on_the_left
    = JITTER_HEAP_TAG_POINTER (object, jitter_heap_thing_tag_hole);
  JITTER_LIST_LINK_AFTER_NONEMPTY (jitter_heap_thing, hole_links,
                                   & (b->hole_list),
                                   & b->left_terminator,
                                   new_hole);

  /* The thing which was on the right of the old object is now on the right
     of the new hole. */
  thing_on_the_right->thing_on_the_left
    = JITTER_HEAP_TAG_POINTER (new_hole, thing_on_the_right_tag);
}

void *
jitter_heap_reallocate_from_block (struct jitter_heap_block *b,
                                   void *old_payload,
                                   size_t user_new_size_in_bytes)
{
  /* Get information about the old thing. */
  struct jitter_heap_thing *old_object
    = JITTER_HEAP_PAYLOAD_TO_THING (old_payload);
  size_t old_payload_size_in_bytes = old_object->payload_size_in_bytes;

  /* Round the user-provided size up, as required by the minimum size and
     alignment constraints. */
  size_t new_payload_size_in_bytes
    = jitter_heap_payload_size_rounded_up (user_new_size_in_bytes);

  /* Are we shrinking or growing the thing? */
  if (new_payload_size_in_bytes <= old_payload_size_in_bytes)
    {
      /* We are shrinking.  This is always possible in an efficient way; in the
         worst case we will create internal fragmentation, but anyway we don't
         have to allocate or free new things, or copy data around. */

      /* Shrink the object, making a hole if possible. */
      jitter_heap_shrink_object (b, old_object, new_payload_size_in_bytes);

      /* Return the old payload: the pointer has not changed and we haven't
         moved the data, but we have created a new hole. */
      return old_payload;
    }
  else
    {
      /* We are growing an object.  For this to be possible in an efficient way,
         the object on the right of the old thing must be a sufficiently large
         hole.  If that is not the case we fall back to a trivial solution. */

      /* Check if we have a large enough hole on the right.  If not, fall back
         to the trivial solution. */
      struct jitter_heap_thing *thing_on_the_right
        = JITTER_HEAP_THING_ON_THE_RIGHT_OF(old_object);
      enum jitter_heap_thing_tag thing_on_the_right_tag
        = JITTER_HEAP_GET_TAG (thing_on_the_right);
      size_t thing_on_the_right_size_in_bytes
        = (thing_on_the_right->payload_size_in_bytes
           + JITTER_HEAP_HEADER_OVERHEAD);
      if (thing_on_the_right_tag != jitter_heap_thing_tag_hole
          || (old_payload_size_in_bytes + thing_on_the_right_size_in_bytes
              < new_payload_size_in_bytes))
        return jitter_heap_reallocate_from_block_trivial
                  (b, old_payload, user_new_size_in_bytes);

      /* If we arrived here we can merge the old object with the hole on the
         right.  This current implementation, while still O(1), is slightly
         suboptimal, as it simply fills the entire hole on the right and then
         shrinks the new larger object when possible. */
      jitter_heap_merge_object_with_hole_on_its_right (b, old_object,
                                                       thing_on_the_right);
      jitter_heap_shrink_object (b, old_object, new_payload_size_in_bytes);
      return old_payload;
    }
}




/* Heap initialization and finalization.
 * ************************************************************************** */

/* Initialize the pointed heap, except that the first block is set to NULL and
   the block list is set to empty. */
static void
jitter_heap_almost_initialize (struct jitter_heap *h,
                               const struct jitter_heap_descriptor *d)
{
  h->descriptor = * d;
  JITTER_LIST_INITIALIZE_HEADER (& (h->block_list));
  h->default_block = NULL;
}

/* Given a heap pointer, add a fresh block to it and return it.  The pointed
   heap must be already initialized, except that it's allowed to have a NULL
   default block; if that is the case, the default block is filled with the
   result of this function.  In every case the new block becomes the default
   one and the first in the list. */
static struct jitter_heap_block *
jitter_heap_add_fresh_block (struct jitter_heap *h)
{
  /* Make a fresh block. */
  size_t block_size = h->descriptor.block_size_and_alignment_in_bytes;
  struct jitter_heap_block *res = h->descriptor.make (block_size);
  if (res == NULL)
    jitter_fatal ("could not make block for heap");
  jitter_heap_initialize_block (res, block_size);

  /* Add it to the list and set it as the default.  We can't use
     jitter_heap_set_default_block here, as b doesn't belong to the list. */
  JITTER_LIST_LINK_FIRST (jitter_heap_block, links, & (h->block_list), res);
  h->default_block = res;

  return res;
}

/* Given a pointed heap and a pointed block already belonging to the heap, make
   the block the default one and the first on the list. */
static void
jitter_heap_set_default_block (struct jitter_heap *h,
                               struct jitter_heap_block *b)
{
  /* Unlink the block from its previous position on the list, whatever it was,
     and make it the first item instead. */
  JITTER_LIST_UNLINK (jitter_heap_block, links, & (h->block_list), b);
  JITTER_LIST_LINK_FIRST (jitter_heap_block, links, & (h->block_list), b);

  /* Set the default field. */
  h->default_block = b;
}

void
jitter_heap_initialize_from_descriptor (struct jitter_heap *h,
                                        const struct jitter_heap_descriptor *d)
{
  /* Almost-initialize the heap: only the first block is missing. */
  jitter_heap_almost_initialize (h, d);

  /* Add the first block. */
  jitter_heap_add_fresh_block (h);
}

void
jitter_heap_initialize (struct jitter_heap *h,
                        jitter_heap_make_block_function make,
                        jitter_heap_destroy_block_function destroy,
                        size_t block_size_and_alignment_in_bytes)
{
  /* Make a descriptor here, as an automatic variable.  It will be copied. */
  struct jitter_heap_descriptor d;
  d.make = make;
  d.destroy = destroy;
  d.block_size_and_alignment_in_bytes = block_size_and_alignment_in_bytes;
  d.block_bit_mask
    = ~ (((jitter_uint) block_size_and_alignment_in_bytes) - 1);

  /* Validate the block size.  This operation is infrequent enough to warrant a
     check. */
  if (! JITTER_IS_A_POWER_OF_TWO (block_size_and_alignment_in_bytes))
    jitter_fatal ("heap block size not a power of two");

  /* Use the descriptor I have just initialized to initialize the heap. */
  jitter_heap_initialize_from_descriptor (h, & d);
}

void
jitter_heap_finalize (struct jitter_heap *h)
{
  jitter_heap_destroy_block_function destroy = h->descriptor.destroy;
  size_t block_size = h->descriptor.block_size_and_alignment_in_bytes;

  /* Destroy every block in the list. */
  struct jitter_heap_block *b = h->block_list.first;
  while (b != NULL)
    {
      struct jitter_heap_block *next = b->links.next;
      destroy (b, block_size);
      b = next;
    }

  /* Do not destroy the default block: it is assumed to be in the list, so it's
     been destroyed already at this point. */
}




/* Object allocation, deallocation and reallocation from a given heap.
 * ************************************************************************** */

/* Given a heap pointer and an initial pointer to some object belonging to the heap,
   return the page containing the object. */
static struct jitter_heap_block *
jitter_heap_get_block (struct jitter_heap *h, void *p)
{
  jitter_uint mask = h->descriptor.block_bit_mask;
  return (void *) (((jitter_uint) p) & mask);
}

void *
jitter_heap_allocate (struct jitter_heap *h, size_t size_in_bytes)
{
  /* First try to allocate from the default page. */
  struct jitter_heap_block *initial_block = h->default_block;
  void *res = jitter_heap_allocate_from_block (initial_block, size_in_bytes);
  if (res != NULL)
    return res;

  /* If we arrived here the default block doesn't have enough space.  Try every
     other block in the list.  Here I can rely on b being the first element of
     the list: every other block will follow it, and no other list element will
     be equal to it. */
  struct jitter_heap_block *b = initial_block->links.next;
  while (b != NULL)
    {
      /* Since we failed with the default block, try again with b. */
      res = jitter_heap_allocate_from_block (b, size_in_bytes);
      if (res != NULL)
        {
          /* Allocation from b succeeded.  Make b the default block for the
             future, in the hope that it has more space available. */
          jitter_heap_set_default_block (h, b);
          return res;
        }
      b = b->links.next;
    }

  /* If we arrived here allocation failed from every block in the heap.  We have
     to make a new one.  If allocation failed from there as well, we fail. */
  struct jitter_heap_block *new_block = jitter_heap_add_fresh_block (h);
  res = jitter_heap_allocate_from_block (new_block, size_in_bytes);
  if (res == NULL)
    jitter_fatal ("could not allocate from heap");
  return res;
}

void *
jitter_heap_reallocate (struct jitter_heap *h, void *old_payload,
                        size_t new_size_in_bytes)
{
  /* Surprisingly, the logic here is quite different from jitter_heap_allocate .
     Efficient reallocation is only possible within a block; if that fails,
     fall back to a trivial solution: allocate a fresh block, copy, free. */

  /* Try the fast path. */
  struct jitter_heap_block *b = jitter_heap_get_block (h, old_payload);
  void *res
    = jitter_heap_reallocate_from_block (b, old_payload, new_size_in_bytes);
  if (res != NULL)
    {
      /* The block we used for reallocation had some space available.  Use it
         again in the future, in the hope that it has more. */
      jitter_heap_set_default_block (h, b);
      return res;
    }

  /* The fast path failed. */
  struct jitter_heap_thing *t = JITTER_HEAP_PAYLOAD_TO_THING(old_payload);
  size_t old_payload_size_in_bytes = t->payload_size_in_bytes;
  res = jitter_heap_allocate (h, new_size_in_bytes);
  memcpy (res, old_payload, old_payload_size_in_bytes);
  jitter_heap_free_from_block (b, old_payload);
  return res;
}

void
jitter_heap_free (struct jitter_heap *h, void *object_payload)
{
  struct jitter_heap_block *b = jitter_heap_get_block (h, object_payload);
  jitter_heap_free_from_block (b, object_payload);

  /* We have freed space from b.  Let's use it again by default. */
  jitter_heap_set_default_block (h, b);
}
