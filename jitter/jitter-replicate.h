/* Jitter: replication header.

   Copyright (C) 2016, 2017 Luca Saiu
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


#ifndef JITTER_REPLICATE_H_
#define JITTER_REPLICATE_H_

/* The functionality in this header is used internally, and is not meant for the
   user. */

#include <jitter/jitter-dispatch.h>
#include <jitter/jitter.h>




/* Do nothing if replication is not used.
 * ************************************************************************** */

/* This entire header file expands to nothing more if replication is
   disabled. */
#ifdef JITTER_REPLICATE




/* Include headers.
 * ************************************************************************** */

//#include <jitter/jitter-instruction.h>
#include <jitter/jitter-routine.h>




/* Declrations.
 * ************************************************************************** */

/* Replicate code in an already specialized program. */
void
jitter_replicate_program (struct jitter_routine *p);

/* Insert a !BEGINBASICBLOCK specialized instruction.  Such a specialized
   instruction serves to support threaded jumps, and is inserted at
   specialization time (only when replication is enabled) as the first
   specialized instruction of each basic block.  See the comment within
   vmprefix_specialize_program .  Not intended for the user. */
void
jitter_insert_beginbasicblock (struct jitter_routine *p);

#endif // #ifdef JITTER_REPLICATE
#endif // #ifndef JITTER_REPLICATE_H_
