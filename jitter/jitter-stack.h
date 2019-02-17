/* Jitter: Forth-style stacks with optional TOS optimization: header.

   Copyright (C) 2017, 2018, 2019 Luca Saiu
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


#ifndef JITTER_STACK_H_
#define JITTER_STACK_H_

#include <stdlib.h> /* for size_t . */
#include <stdbool.h>

#include <jitter/jitter.h>
#include <jitter/jitter-malloc.h>
#include <jitter/jitter-cpp.h>


/* Stack backing data structures.
 * ************************************************************************** */

/* A stack @dfn{backing} points to the beginning of the memory buffer used to
   implement the stack, along with some debugging meta-information.

   The stack runtime data structure is more compact, and suitable to be stored
   efficiently in registers; in particular the runtime structure contains no
   pointer to the *beginning* of the memory or no information about the current
   stack height.  Only the runtime stack structure, and not the backing, is
   manipulated at run time after initialization. */

/* A stack may be either TOS-optimized or not. */
enum jitter_stack_optimization
  {
    /* The stack is TOS-optimized: the top of the stack is held in a separate
       field of the struct, hopefully allocated to a machine register in order
       to reduce memory accesses. */
    jitter_stack_optimization_tos,

    /* The stack is not TOS-optimized: the top element is held in memory just
       like any other.  This makes some operations slower (and some marginally
       faster), but has the advantage of freeing up one machine register. */
    jitter_stack_optimization_no_tos
  };

/* The stack @dfn{backing} is a data structure containing some information about
   it such as the number of elements and whether the stack is TOS-optimized or
   not; it also holds a pointer to the *beginning* of the memory used to back
   elements, which makes initialization and finalization easier. */

/* The stack backing. */
struct jitter_stack_backing
{
  /* The stack optimization, either TOS-optimized or not. */
  enum jitter_stack_optimization optimization;

  /* How many bytes each element takes. */
  size_t element_size_in_bytes;

  /* How many elements are allocated for this stack, not including the TOS
     element in the case of TOS optimization. */
  size_t element_no;

  /* A pointer to the beginning of the memory holding the stack elements.
     When stack elements are pushed or popped a pointer moves in the stack
     data structure, but this field keeps pointing the the beginning of the
     allocated space, making initialization and finalization easier.  The
     memory is heap-allocated, and its beginning is aligned like the result of
     malloc . */
  char *memory;
};

/* These operations should be not critical for performance on any reasonable
   application, and therefore their implementation can safely involve non-inline
   C functions. */

/* Initialize the pointed stack backing, allocating space from the heap for the
   given number of elements, each of the given size.  Also store the fact that
   the stack is TOS-optimized in the backing, for debugging purposes. */
void
jitter_stack_initialize_tos_backing (struct jitter_stack_backing *backing,
                                     size_t element_size_in_bytes,
                                     size_t element_no)
  __attribute__ ((nonnull (1)));

/* Initialize the pointed stack backing, allocating space from the heap for the
   given number of elements, each of the given size.  Also store the fact that
   the stack is not TOS-optimized in the backing, for debugging purposes. */
void
jitter_stack_initialize_ntos_backing (struct jitter_stack_backing *backing,
                                      size_t element_size_in_bytes,
                                      size_t element_no)
  __attribute__ ((nonnull (1)));

/* Finalize the pointed stack backing, releasing memory. */
void
jitter_stack_finalize_backing (struct jitter_stack_backing *backing)
  __attribute__ ((nonnull (1)));


/* FIXME: if it's acceptable to use three memory pages per stack backing we can
   use mmap to allocate two non-readable and non-writable pages around a
   readable and writable page used for the memory backing.  This would cause a
   segfault on overflow and underflow -- with a one-element delay if TOS
   optimization is enabled, but still useful.
   The implementation, relying on mmap , would be easy. */




/* Stack data structure definitions.
 * ************************************************************************** */

/* Since the stack element type is a parameter and adding one indirection level
   would be unacceptable for performance, we cannot use a C struct to define a
   TOS-optimized stack; we would need something like C++ templates.  Anyway it
   is quite easy to treat two separate variables as "the stack". */

/* The @var{stack_container} argument is meant to be part of an expression
   containing the stack variables, for example "mystruct." .  Of course it is
   allowed to be empty. */

/* Expand to the variable name or struct field holding the stack top, as an
   l-value. */
#define JITTER_STACK_TOS_TOP_NAME(type, stack_container, name)       \
  stack_container                                                    \
  JITTER_CONCATENATE_THREE(jitter_tos_optimized_stack_, name, _top)

/* Expand to the the variable name or struct field holding the stack
   under-top-pointer, as an l-value */
#define JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type, stack_container, name)  \
  stack_container                                                             \
  JITTER_CONCATENATE_THREE(jitter_tos_optimized_stack_, name, _under_top_p)
#define JITTER_STACK_NTOS_TOP_POINTER_NAME(type, stack_container, name)  \
  stack_container                                                             \
  JITTER_CONCATENATE_THREE(jitter_non_tos_optimized_stack_, name, _top_p)

/* Declare a stack.  This exapnds to one or more non-extern C variable
   declarations, suitable for automatic variables or struct members. */
#define JITTER_STACK_TOS_DECLARATION(type, name)                         \
  type JITTER_STACK_TOS_TOP_NAME(type, , name);                          \
  type * restrict JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type, , name)
#define JITTER_STACK_NTOS_DECLARATION(type, name)                        \
  type * restrict JITTER_STACK_NTOS_TOP_POINTER_NAME(type, , name)

/* Expand to a statement initializing a stack from a backing. */
#define JITTER_STACK_TOS_INITIALIZE(type, stack_container, name, backing)   \
  do                                                                        \
    {                                                                       \
      /* Initialize the under-top pointer to point to one element below     \
         the beginning of the backing for an "empty" stack, so that the     \
         first element to be pushed will increment the pointer to the       \
         first valid position, and store the (unspecified) initial TOS      \
         there.  This is as empty as a TOS-optimized stack can get.  */     \
      JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type, stack_container, name)  \
        = ((type *) ((backing).memory)) - 1;                                \
      /* No need to initialize the TOS. */                                  \
    }                                                                       \
  while (false)
#define JITTER_STACK_NTOS_INITIALIZE(type, stack_container, name, backing)  \
  do                                                                        \
    {                                                                       \
      /* Initialize the top pointer to point to one element below           \
         the beginning of the backing for an empty stack, so that the       \
         first element to be pushed will increment the pointer to the       \
         first valid position. */                                           \
      JITTER_STACK_NTOS_TOP_POINTER_NAME(type, stack_container, name)       \
        = ((type *) ((backing).memory)) - 1;                                \
      /* No need to initialize the TOS. */                                  \
    }                                                                       \
  while (false)

/* There is no finalization facility for stacks: stacks are meant to be either
   automatic variables or struct fields, so their storage is not heap-allocated.
   Stack *backings* have their own finalization function. */

/* FIXME: comment.

   Stacks are meant to be used in the style of Forth, for accessing the top
   elements only and not as arrays.

   Stacks do not perform any overflow, underflow or TOS-type checking; their
   operations are implemented as macros and are meant to be fast, but inherently
   unsafe.

   Stacks do *not* point to their backing: since every field of stack structures
   is meant to be allocated to a register it's important not to waste space. */




/* Fundamental stack operations.
 * ************************************************************************** */

/* FIXME: comment.

   The @var{stack} argument must be an l-value expression without side effects,
   of type struct jitter_stack_tos ; it is meant to be an automatic variable.

   By using GCC extensions it would be easy to have macros expanding to
   side-effecting expressions, altering the stack and evaluating to the new top;
   but that feels very difficult to do in a portable macro evaluating the stack
   only once, and here avoiding function overhead is paramount. */

/* Expand to an expression returning the top element of the given stack.
   Undefined behavior on empty stack.  The expression this macro expands to is
   an l-value. */
#define JITTER_STACK_TOS_TOP(type, stack_container, name)  \
  JITTER_STACK_TOS_TOP_NAME(type, stack_container, name)
#define JITTER_STACK_NTOS_TOP(type, stack_container, name) \
  (* JITTER_STACK_NTOS_TOP_POINTER_NAME(type, stack_container, name))

/* Expand to an expression returning the under-top element of the given stack.
   Undefined behavior on empty stack.  The expression this macro expands to is
   an l-value. */
#define JITTER_STACK_TOS_UNDER_TOP(type, stack_container, name)  \
  (* JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type, stack_container, name))
#define JITTER_STACK_NTOS_UNDER_TOP(type, stack_container, name)  \
  (JITTER_STACK_NTOS_TOP_POINTER_NAME(type, stack_container, name) [-1])

/* Expand to an expression returning the under-under-top element of the given
   stack.  Undefined behavior on underflow.  The expression this macro expands
   to is an l-value. */
#define JITTER_STACK_TOS_UNDER_UNDER_TOP(type, stack_container, name)          \
  (JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type, stack_container, name) [-1])
#define JITTER_STACK_NTOS_UNDER_UNDER_TOP(type, stack_container, name)  \
  (JITTER_STACK_NTOS_TOP_POINTER_NAME(type, stack_container, name) [-2])

/* Expand to an expression evaluating to the element distance positions below
   the top of the given stack.  Undefined behavior on underflow.  The expression
   this macro expands to is an r-value.
   The distance should be a constant expression for good performance. */
#define JITTER_STACK_TOS_AT_DEPTH(type, stack_container, name, distance)        \
  ((distance == 0)                                                              \
   ? (JITTER_STACK_TOS_TOP(type, stack_container, name))                        \
   : JITTER_STACK_TOS_AT_NONZERO_DEPTH(type, stack_container, name, distance))
#define JITTER_STACK_NTOS_AT_DEPTH(type, stack_container, name, distance)    \
  JITTER_STACK_NTOS_AT_NONZERO_DEPTH(type, stack_container, name, distance)

/* Like JITTER_STACK_TOS_AT_DEPTH and JITTER_STACK_NTOS_AT_DEPTH, but assume
   that the distance is strictly positive.  This expands to better code when
   the distance is known to be non-zero but its actual value is not known. */
#define JITTER_STACK_TOS_AT_NONZERO_DEPTH(type, stack_container, name,   \
                                          distance)                      \
  (JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type, stack_container, name)  \
     [- (distance - 1)])
#define JITTER_STACK_NTOS_AT_NONZERO_DEPTH(type, stack_container, name,  \
                                           distance)                     \
  (JITTER_STACK_NTOS_TOP_POINTER_NAME(type, stack_container, name)       \
     [- (distance)])

/* Expand to a statement updating an element the given stack at the given
   distance from the top (0 being the top, 1 being the undertop, and so on); the
   new element will be set to the result of the expansion of the given new element.
   Undefined behavior on underflow.
   The distance should be a constant expression for good performance. */
#define JITTER_STACK_TOS_SET_AT_DEPTH(type, stack_container, name, distance,  \
                                      new_element)                            \
  do                                                                          \
    {                                                                         \
      /* Keeping the distance in an unsigned variable might give a useful     \
         warning in some cases where the distance as supplied by the user is  \
         incorrect.  However the final array index computed based on the      \
         distance can be negative, so the expression evaluating it must not   \
         involve unsigned operands. */                                        \
      const unsigned _jitter_set_at_depth_distance_u = (distance);            \
      const int _jitter_set_at_depth_distance =                               \
        _jitter_set_at_depth_distance_u;                                      \
      if (_jitter_set_at_depth_distance == 0)                                 \
        JITTER_STACK_TOS_TOP_NAME(type, stack_container, name)                \
          = (new_element);                                                    \
      else                                                                    \
        JITTER_STACK_TOS_SET_AT_NONZERO_DEPTH(type, stack_container, name,    \
                                              _jitter_set_at_depth_distance,  \
                                              (new_element));                 \
    }                                                                         \
  while (false)
#define JITTER_STACK_NTOS_SET_AT_DEPTH(type, stack_container, name, distance,  \
                                       new_element)                            \
  do                                                                           \
    {                                                                          \
      (JITTER_STACK_NTOS_TOP_POINTER_NAME(type, stack_container, name)         \
          [- (distance)]) = (new_element);                                     \
    }                                                                          \
  while (false)

/* Like JITTER_STACK_TOS_SET_AT_DEPTH and JITTER_STACK_NTOS_SET_AT_DEPTH, but
   assume that the distance is strictly positive.  This expands to better code
   when the distance is known to be non-zero but its actual value is not
   known. */
#define JITTER_STACK_TOS_SET_AT_NONZERO_DEPTH(type, stack_container, name,   \
                                              distance, new_element)         \
  do                                                                         \
    {                                                                        \
      (JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type, stack_container, name)  \
          [1 - (distance)]) = (new_element);                                 \
    }                                                                        \
  while (false)
#define JITTER_STACK_NTOS_SET_AT_NONZERO_DEPTH(type, stack_container, name,  \
                                               distance, new_element)        \
  JITTER_STACK_NTOS_SET_AT_DEPTH(type, stack_container, name, distance,      \
                                 new_element)

/* Expand to a statement pushing an unspecified object on top of the given
   stack.  Undefined behavior on overflow.  There is no result.  This operation
   is more efficient than JITTER_STACK_*_PUSH , and is preferred when the top
   element is about to be replaced by another operation before it is first
   used. */
#define JITTER_STACK_TOS_PUSH_UNSPECIFIED(type, stack_container, name)  \
  do                                                                    \
    {                                                                   \
      JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type, stack_container,    \
                                              name) [1]                 \
        = JITTER_STACK_TOS_TOP_NAME(type, stack_container, name);       \
      JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type, stack_container,    \
                                              name) ++;                 \
      /* JITTER_STACK_TOS_TOP_NAME(type, stack_container, name) is */   \
      /* left with its previous value, which happens to be equal to */  \
      /* the new under-top -- but this might change in the future. */   \
    }                                                                   \
  while (false)
/* #define JITTER_STACK_TOS_PUSH_UNSPECIFIED(type, stack_container, name)      \ */
/*   do                                                                        \ */
/*     {                                                                       \ */
/*       * (++ JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type, stack_container,  \ */
/*                                                     name))                  \ */
/*         = JITTER_STACK_TOS_TOP_NAME(type, stack_container, name);           \ */
/*       /\* JITTER_STACK_TOS_TOP_NAME(type, stack_container, name) is *\/       \ */
/*       /\* left with its previous value, which happens to be equal to the *\/  \ */
/*       /\* new under-top -- but this might change in the future. *\/           \ */
/*    }                                                                        \ */
/*   while (false) */
/* For some reason this version generates one instruction too many on MIPS,
   and x86_64, but not on PowerPC:
   MIPS:
# 0x436a30: stackpushunspecified (12 bytes):
7676b04c 26e20004       addiu $2,$23,4
7676b050 aefe0004       sw $30,4($23)
7676b054 0040b825       or $23,$2,$0
   x86_64:
# 0xef01f0: stackpushunspecified (13 bytes):
00007fd95b54c045 4d 8d 44 24 08 leaq 0x8(%r12),%r8
00007fd95b54c04a 49 89 6c 24 08 movq %rbp,0x8(%r12)
00007fd95b54c04f 4d 89 c4       movq %r8,%r12
   PowerPC:
# 0x10033d18: stackpushunspecified (8 bytes):
f678c040 92 af 00 04    stw r21,4(r15)
f678c044 39 ef 00 04    addi r15,r15,4
  This seems to be the same problem affecting the commented-out version of
  JITTER_STACK_TOS_DROP .  I might be forgetting a restrict qualifier
  somewhere.  But why is PowerPC immune to it?  By the way, the uncommented
  definition above is better on PowerPC as well, as it uses a post-incrementing
  store-word-and-update instruction.  Why is this version different?
  Tested on GCC 8.0.0 20170430 , the same on every architecture. */
#define JITTER_STACK_NTOS_PUSH_UNSPECIFIED(type, stack_container, name)    \
  do                                                                       \
    {                                                                      \
      JITTER_STACK_NTOS_TOP_POINTER_NAME(type, stack_container, name) ++;  \
    }                                                                      \
  while (false)

/* Expand to a statement pushing the given new element on top of the given
   stack; the new element expression is evaluated only once.  Undefined behavior
   on overflow.  There is no result. */
#define JITTER_STACK_TOS_PUSH(type, stack_container, name, new_element)  \
  do                                                                     \
    {                                                                    \
      const type _jitter_stack_new_element_temp = (new_element);         \
      JITTER_STACK_TOS_PUSH_UNSPECIFIED(type, stack_container, name);    \
      JITTER_STACK_TOS_TOP_NAME(type, stack_container, name)             \
        = (type) _jitter_stack_new_element_temp;                         \
    }                                                                    \
  while (false)
#define JITTER_STACK_NTOS_PUSH(type, stack_container, name, new_element)  \
  do                                                                      \
    {                                                                     \
      const type _jitter_stack_new_element_temp = (new_element);          \
      (JITTER_STACK_NTOS_TOP_POINTER_NAME(type, stack_container, name))   \
         [1] = (type) (_jitter_stack_new_element_temp);                   \
      JITTER_STACK_NTOS_PUSH_UNSPECIFIED(type, stack_container, name);    \
    }                                                                     \
  while (false)

/* A stack height should be treated like an abstract type, only to be used with
   JITTER_STACK_*_HEIGHT and JITTER_STACK_*_SET_HEIGHT .  It represents the
   "height" of a stack, which is to say the distance from its bottom.  Given a
   stack it is possible to obtain its height---and then to restore the same
   height on the same (and only the same) stack, which restores its complete
   state including the top element.
   It is safe, and supported, to compare two jitter_stack_height objects for
   equality, in order to check whether one given stack has kept the same height
   in different program states; this may be useful as a form of defensive
   programming, perhaps in an assertion.  Heights taken from *distinct* stacks
   will always compare as different.
   Implementation note: the actual saved value is currently a pointer to the
   undertop element for a TOS-optimized stack, and a pointer to the top element
   for a non-TOS-optimized stack. */
typedef void * restrict
jitter_stack_height;

/* Return the height of the given stack as a jitter_stack_height object.  The
   result can be used in a JITTER_STACK_*_SET_HEIGHT operation. */
#define JITTER_STACK_TOS_HEIGHT(type, stack_container, name)                \
  ((const jitter_stack_height)                                              \
   (JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type, stack_container, name)))
#define JITTER_STACK_NTOS_HEIGHT(type, stack_container, name)        \
  ((const jitter_stack_height)                                       \
   JITTER_STACK_NTOS_TOP_POINTER_NAME(type, stack_container, name))

/* Restore the height of a stack to the given value, which must have been saved
   in advance.  A height should always be used to pop elements, never to push
   them back if the user wishes to reconstruct the state saved beefore in a
   consistent and deterministic way. */
#define JITTER_STACK_TOS_SET_HEIGHT(type, stack_container, name, height)        \
  do                                                                            \
    {                                                                           \
      const jitter_stack_height _jitter_new_height = (height);                  \
      const jitter_stack_height _jitter_old_height                              \
        = JITTER_STACK_TOS_HEIGHT(type, stack_container, name);                 \
      if (__builtin_expect (_jitter_old_height != _jitter_new_height,           \
                            true))                                              \
        {                                                                       \
          JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type, stack_container, name)  \
            = _jitter_new_height;                                               \
          JITTER_STACK_TOS_TOP_NAME(type, stack_container, name)                \
            = JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type, stack_container,    \
                                                      name) [1];                \
        }                                                                       \
    }                                                                           \
  while (false)
#define JITTER_STACK_NTOS_SET_HEIGHT(type, stack_container, name, height)  \
  do                                                                       \
    {                                                                      \
      JITTER_STACK_NTOS_TOP_POINTER_NAME(type, stack_container, name)      \
        = (const jitter_stack_height) (height);                            \
    }                                                                      \
  while (false)




/* Forth-style stack operations.
 * ************************************************************************** */

/* Push another copy of the top element to the stack; this expands to a
   statement, and there is no result.  Undefined behavior on overflow.  This
   operation is called "dup" in Forth. */
#define JITTER_STACK_TOS_DUP(type, stack_container, name)                       \
  do                                                                            \
    {                                                                           \
      JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type, stack_container, name) [1]  \
        = JITTER_STACK_TOS_TOP_NAME(type, stack_container, name);               \
      JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type, stack_container, name) ++;  \
    }                                                                           \
  while (false)
/* #define JITTER_STACK_TOS_DUP(type, stack_container, name)                   \ */
/*   do                                                                        \ */
/*     {                                                                       \ */
/*       * (++ JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type, stack_container,  \ */
/*                                                     name))                  \ */
/*         = JITTER_STACK_TOS_TOP_NAME(type, stack_container, name);           \ */
/*     }                                                                       \ */
/*   while (false) */
/* This version generates one instruction too many on MIPS, x86_64 and Aarch64
   but is good on PowerPC, ARM and SH.  Why?
   x86_64:
     # 0x10791f0: stackdup (11 bytes):
     00007fa1f6a3205b 49 8d 7e 08  leaq 0x8(%r14),%rdi
     00007fa1f6a3205f 4d 89 6e 08  movq %r13,0x8(%r14)
     00007fa1f6a32063 49 89 fe     movq %rdi,%r14
   MIPS:
     # 0x434a30: stackdup (12 bytes):
     7676b06c 26af0004     addiu $15,$21,4
     7676b070 aeb70004     sw $23,4($21)
     7676b074 01e0a825     or $21,$15,$0
   Aarch64:
     # 0x42f1f0: stackdup (12 bytes):
     00000040009ac050 91002345     add x5, x26, #0x8
     00000040009ac054 f900075b     str x27, [x26,#8]
     00000040009ac058 aa0503fa     mov x26, x5
   PowerPC:
     # 0x10032d18: stackdup (8 bytes):
     f678c05c 92 11 00 04  stw r16,4(r17)
     f678c060 3a 31 00 04  addi r17,r17,4
   ARM:
     # 0x3ba30: stackdup (8 bytes):
     f6661050 e5876004     str r6, [r7, #4]
     f6661054 e2877004     add r7, r7, #4
   SH:
     # 0x423a30: stackdup (4 bytes):
     f664e034 e1 1b        mov.l r14,@(4,r11)
     f664e036 04 7b        add #4,r11
  The uncommented version is better even on PowerPC and ARM, using a single
  post-incrementing store, like on Aarch64 which goes from three down to one.
  Tested on GCC 8.0.0 20170430 , the same on every architecture. */
#define JITTER_STACK_NTOS_DUP(type, stack_container, name)                     \
  do                                                                           \
    {                                                                          \
      const type _jitter_stack_top_element_copy                                \
        = * (JITTER_STACK_NTOS_TOP_POINTER_NAME(type, stack_container, name)); \
      * (++ JITTER_STACK_NTOS_TOP_POINTER_NAME(type, stack_container, name))   \
        = _jitter_stack_top_element_copy;                                      \
    }                                                                          \
  while (false)

/* Remove the top element from the stack; this expands to a statement, and there
   is no result.  Undefined behavior on empty stack.  This operation is called
   "drop" in Forth. */
#define JITTER_STACK_TOS_DROP(type, stack_container, name)             \
  do                                                                   \
    {                                                                  \
      JITTER_STACK_TOS_TOP_NAME(type, stack_container, name)           \
        = * (JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type,             \
                                                     stack_container,  \
                                                     name));           \
      JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type,                    \
                                              stack_container,         \
                                              name) --;                \
    }                                                                  \
  while (false)
/* #define JITTER_STACK_TOS_DROP(type, stack_container, name)             \ */
/*   do                                                                   \ */
/*     {                                                                  \ */
/*       JITTER_STACK_TOS_TOP_NAME(type, stack_container, name)           \ */
/*         = * (JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type,             \ */
/*                                                      stack_container,  \ */
/*                                                      name) --);        \ */
/*     }                                                                  \ */
/*   while (false) */
/* FIXME: why is this version generating three instructions?
   This yields (MIPS):
      # 0x436a00: stackdrop (12 bytes):
      7676b050 26eefffc addiu $14,$23,-4
      7676b054 8efe0000 lw $30,0($23)
      7676b058 01c0b825 or $23,$14,$0
   or (PowerPC):
      # 0x10033ce8: stackdrop (12 bytes):
      f678c040 38 af ff fc      addi r5,r15,-4
      f678c044 82 af 00 00      lwz r21,0(r15)
      f678c048 7c af 2b 78      mr r15,r5
   or (x86_64):
      # 0x15c11f0: stackdrop (12 bytes):
      00007fc8726cf04a 49 8d 54 24 f8   leaq -0x8(%r12),%rdx
      00007fc8726cf04f 49 8b 2c 24      movq (%r12),%rbp
      00007fc8726cf053 49 89 d4 movq %rdx,%r12
   or (SH -- here one of the four instructions is justified by SH's two-operand
       arithmetic, but the problem of keeping two different pointers is still
       the same):
      # 0x423a30: stackdrop (8 bytes):
      f664e02e b3 63    mov r11,r3
      f664e030 b2 6e    mov.l @r11,r14
      f664e032 fc 73    add #-4,r3
      f664e034 33 6b    mov r3,r11
  The uncommented version above does the right thing, yielding two
  instructions (or just one on Aarch64 and ARM, using a post-decrementing load).
  The under-top pointer is declared restrict , so GCC cannot assume
  that the under-top pointer may alias the top; and in fact the top is
  correctly held in a register, even with this three-instruction version.
  Tested on GCC 8.0.0 20170430 , the same on every architecture. */
#define JITTER_STACK_NTOS_DROP(type, stack_container, name)                \
  do                                                                       \
    {                                                                      \
      JITTER_STACK_NTOS_TOP_POINTER_NAME(type, stack_container, name) --;  \
    }                                                                      \
  while (false)

/* Push a copy of the under-top element.  Undefined behavior on empty
   stack.  This operation is called "over" in Forth. */
#define JITTER_STACK_TOS_OVER(type, stack_container, name)          \
  do                                                                \
    {                                                               \
      const type _jitter_stack_over_under_top_temp                  \
        = JITTER_STACK_TOS_UNDER_TOP(type, stack_container, name);  \
      JITTER_STACK_TOS_PUSH(type, stack_container, name,            \
                            _jitter_stack_over_under_top_temp);     \
    }                                                               \
  while (false)
#define JITTER_STACK_NTOS_OVER(type, stack_container, name)                     \
  do                                                                            \
    {                                                                           \
      const type _jitter_stack_old_under_top_element_copy                       \
        = JITTER_STACK_NTOS_TOP_POINTER_NAME(type, stack_container, name) [-1]; \
      * (++ JITTER_STACK_NTOS_TOP_POINTER_NAME(type, stack_container, name))    \
        = _jitter_stack_old_under_top_element_copy;                             \
    }                                                                           \
  while (false)

/* Swap the top and under-top element on the stack; this expands to a statement,
   and there is no result.  Undefined behavior on underflow.  This operation is
   called "swap" in Forth. */
#define JITTER_STACK_TOS_SWAP(type, stack_container, name)          \
  do                                                                \
    {                                                               \
      const type _jitter_stack_swap_under_top_temp                  \
        = JITTER_STACK_TOS_UNDER_TOP(type, stack_container, name);  \
      JITTER_STACK_TOS_UNDER_TOP(type, stack_container, name)       \
        = JITTER_STACK_TOS_TOP(type, stack_container, name);        \
      JITTER_STACK_TOS_TOP(type, stack_container, name)             \
        = _jitter_stack_swap_under_top_temp;                        \
    }                                                               \
  while (false)
#define JITTER_STACK_NTOS_SWAP(type, stack_container, name)          \
  do                                                                 \
    {                                                                \
      const type _jitter_stack_swap_top_temp                         \
        = JITTER_STACK_NTOS_TOP(type, stack_container, name);        \
      const type _jitter_stack_swap_under_top_temp                   \
        = JITTER_STACK_NTOS_UNDER_TOP(type, stack_container, name);  \
      JITTER_STACK_NTOS_UNDER_TOP(type, stack_container, name)       \
        = _jitter_stack_swap_top_temp;                               \
      JITTER_STACK_NTOS_TOP(type, stack_container, name)             \
        = _jitter_stack_swap_under_top_temp;                         \
    }                                                                \
  while (false)

/* Remove the under-top element from the stack, without affecting the top; this
   expands to a statement, and there is no result.  Undefined behavior on an
   empty or one-element stack.  This operation is called "nip" in Forth. */
#define JITTER_STACK_TOS_NIP(type, stack_container, name)                      \
  do                                                                           \
    {                                                                          \
      JITTER_STACK_TOS_UNDER_TOP_POINTER_NAME(type, stack_container, name) --; \
    }                                                                          \
  while (false)
#define JITTER_STACK_NTOS_NIP(type, stack_container, name)                    \
  do                                                                          \
    {                                                                         \
      const type _jitter_stack_nip_top_temp                                   \
        = JITTER_STACK_NTOS_TOP(type, stack_container, name);                 \
      * (-- JITTER_STACK_NTOS_TOP_POINTER_NAME(type, stack_container, name))  \
        = _jitter_stack_nip_top_temp;                                         \
    }                                                                         \
  while (false)




/* Inefficient stack operations.
 * ************************************************************************** */

/* The stack operations defined in this section may expand to inefficient code
   and are not recommended for production use, unless the arguments are known
   small constants.  Anyway they are convenient for testing. */

/* Expand to a statement destructively reversing the order of the topmost n
   elements of the given stack. */
#define JITTER_STACK_TOS_REVERSE(type, stack_container, name, element_no)      \
  do                                                                           \
    {                                                                          \
      /* Use an unsigned variable to get a warning in case the user passes a   \
         negative constant by mistake.  Then convert to int, so that we may    \
         safely compute negative indices from other index operands, as it may  \
         be needed fo termination conditions. */                               \
      unsigned jitter_reverse_element_nou = (element_no);                      \
      int jitter_reverse_element_no = jitter_reverse_element_nou;              \
      /* Do nothing unless we have at least two elements to swap.  If we do,   \
         then of course we have to handle the TOS specially, and swap it with  \
         the deepest element. */                                               \
      if (jitter_reverse_element_no < 2)                                       \
        break;                                                                 \
      JITTER_SWAP (type,                                                       \
                   JITTER_STACK_TOS_TOP_NAME(type, stack_container, name),     \
                   JITTER_STACK_TOS_AT_NONZERO_DEPTH                           \
                      (type, stack_container, name,                            \
                       (jitter_reverse_element_no - 1)));                      \
      /* Every other element to swap will be at a non-zero depth.  Have two    \
         indices moving towards one another; keep swapping element pairs       \
         until the two indices meet or cross. */                               \
      int jitter_reverse_up_depth = 1;                                         \
      int jitter_reverse_down_depth = jitter_reverse_element_no - 2;           \
      while (jitter_reverse_down_depth > jitter_reverse_up_depth)              \
        {                                                                      \
          JITTER_SWAP (type,                                                   \
                       JITTER_STACK_TOS_AT_NONZERO_DEPTH                       \
                          (type, stack_container, name,                        \
                           jitter_reverse_up_depth),                           \
                       JITTER_STACK_TOS_AT_NONZERO_DEPTH                       \
                          (type, stack_container, name,                        \
                           jitter_reverse_down_depth));                        \
          jitter_reverse_up_depth ++;                                          \
          jitter_reverse_down_depth --;                                        \
        }                                                                      \
    }                                                                          \
  while (false)
#define JITTER_STACK_NTOS_REVERSE(type, stack_container, name, element_no)      \
  do                                                                            \
    {                                                                           \
      /* Use an unsigned variable to get a warning in case the user passes a    \
         negative constant by mistake.  Then convert to int, so that we may     \
         safely compute negative indices from other index operands, as it may   \
         be needed fo termination conditions. */                                \
      unsigned jitter_reverse_element_nou = (element_no);                       \
      int jitter_reverse_element_no = jitter_reverse_element_nou;               \
      /* Have two depth indices moving into opposite directions.  Keep          \
         swapping elements at the dephts described by the two indices, until    \
         the indices meet or cross. */                                          \
      int jitter_reverse_up_depth = 0;                                          \
      int jitter_reverse_down_depth = jitter_reverse_element_no - 1;            \
      while (jitter_reverse_down_depth > jitter_reverse_up_depth)               \
        {                                                                       \
          JITTER_SWAP (type,                                                    \
                       JITTER_STACK_NTOS_AT_DEPTH (type, stack_container, name, \
                                                   jitter_reverse_up_depth),    \
                       JITTER_STACK_NTOS_AT_DEPTH (type, stack_container, name, \
                                                   jitter_reverse_down_depth)); \
          jitter_reverse_up_depth ++;                                           \
          jitter_reverse_down_depth --;                                         \
        }                                                                       \
    }                                                                           \
  while (false)




/* Generic stack operations with user-specified functions.
 * ************************************************************************** */

/* Compute the given "setter" on the top element of the stack, replacing it
   with the result.
   This expands to a statement, and there is no result.  Undefined behavior
   on an empty or one-element stack.
   The "setter" is supposed to be the name of a two-argument macro expanding
   to a statement which modifies its first argument (an l-value expression)
   using the other (an r-value expression).  The setter body may be
   protected with the usual do .. while (false) idiom.

   Example:
#define SETSUCC(res, a)          do { res.field = a.field + 1; } while (0)
   JITTER_STACK_TOS_UNARY(union my_union, stack, , SETSUCC);
#undef  SETSUCC */
#define JITTER_STACK_TOS_UNARY(type, stack_container, name, setter)  \
  do                                                                 \
    {                                                                \
      {                                                              \
        setter(JITTER_STACK_TOS_TOP(type, stack_container, name),    \
               JITTER_STACK_TOS_TOP(type, stack_container, name));   \
      }                                                              \
    }                                                                \
  while (false)
#define JITTER_STACK_NTOS_UNARY(type, stack_container, name, setter)  \
  do                                                                  \
    {                                                                 \
      {                                                               \
        setter(JITTER_STACK_NTOS_TOP(type, stack_container, name),    \
               JITTER_STACK_NTOS_TOP(type, stack_container, name));   \
      }                                                               \
    }                                                                 \
  while (false)

/* Compute the given "setter" on the under-top and top elements of the stack, in
   that order, replacing both with the result.
   This expands to a statement, and there is no result.  Undefined behavior
   on an empty or one-element stack.
   The "setter" is supposed to be the name of a three-argument macro expanding
   to a statement which modifies its first argument (an l-value expression)
   using the other two (r-value expressions), in order.  The setter body may be
   protected with the usual do .. while (false) idiom, but does not need to be.

   Example:
#define SETPLUS(res, a, b)      do { res.field = a.field + b.field; } while (0)
   JITTER_STACK_TOS_BINARY(union my_union, stack, , SETPLUS);
#undef  SETPLUS */
#define JITTER_STACK_TOS_BINARY(type, stack_container, name, setter)      \
  do                                                                      \
    {                                                                     \
      {                                                                   \
        setter(JITTER_STACK_TOS_TOP(type, stack_container, name),         \
               JITTER_STACK_TOS_UNDER_TOP(type, stack_container, name),   \
               JITTER_STACK_TOS_TOP(type, stack_container, name));        \
      }                                                                   \
      JITTER_STACK_TOS_NIP(type, stack_container, name);                  \
    }                                                                     \
  while (false)
#define JITTER_STACK_NTOS_BINARY(type, stack_container, name, setter)      \
  do                                                                       \
    {                                                                      \
      {                                                                    \
        setter(JITTER_STACK_NTOS_UNDER_TOP(type, stack_container, name),   \
               JITTER_STACK_NTOS_UNDER_TOP(type, stack_container, name),   \
               JITTER_STACK_NTOS_TOP(type, stack_container, name));        \
      }                                                                    \
      JITTER_STACK_NTOS_TOP_POINTER_NAME(type, stack_container, name) --;  \
    }                                                                      \
  while (false)




/* Helper macros, not for the user.
 * ************************************************************************** */

/* Expand to a statement destructively swapping the values of the two given
   l-values, which must have both the given type.  Either a or b may be
   evaluated twice. */
#define JITTER_SWAP(type, a, b)           \
  do                                      \
    {                                     \
      const type _jitter_swap_tmp = (a);  \
      (a) = (b);                          \
      (b) = (type) _jitter_swap_tmp;      \
    }                                     \
  while (false)

#endif // #ifndef JITTER_STACK_H_
