/* Jitter: choose the dispatching model.

   Copyright (C) 2017 Luca Saiu
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


#ifndef JITTER_DISPATCH_H_
#define JITTER_DISPATCH_H_

/* This header does not include <jitter/jitter.h> : it can be included
   alone, and some definitions in the other files included by
   jitter/jitter.h may depend on this.

   The file is not meant for the user to include directly. */




/* Internal header inclusion.
 * ************************************************************************** */

/* We need some CPP machinery here, for conveninient stringification and token
   concatenation. */

#include <jitter/jitter-cpp.h>




/* Definitions depending on the dispatch model.
 * ************************************************************************** */

/* If there is no CPP definition for a dispatching model use the one identified
   as the best by configure. */
#if    ! defined(JITTER_DISPATCH_SWITCH)             \
    && ! defined(JITTER_DISPATCH_DIRECT_THREADING)   \
    && ! defined(JITTER_DISPATCH_MINIMAL_THREADING)  \
    && ! defined(JITTER_DISPATCH_NO_THREADING)
# if   defined(JITTER_BEST_DISPATCHING_MODEL_IS_SWITCH)
#   define JITTER_DISPATCH_SWITCH 1
# elif defined(JITTER_BEST_DISPATCHING_MODEL_IS_DIRECT_THREADING)
#   define JITTER_DISPATCH_DIRECT_THREADING 1
# elif defined(JITTER_BEST_DISPATCHING_MODEL_IS_MINIMAL_THREADING)
#   define JITTER_DISPATCH_MINIMAL_THREADING 1
# elif defined(JITTER_BEST_DISPATCHING_MODEL_IS_NO_THREADING)
#   define JITTER_DISPATCH_NO_THREADING 1
# else
#   error "no best dispatching model is defined.  This should never happen."
# endif // #if defined(JITTER_BEST_DISPATCHING_MODEL_IS_...)
#endif // no CPP definition for the dispatch model

/* Check that one dispatching model is defined with a CPP macro, and define
   JITTER_REPLICATE if needed.  Also define the JITTER_DISPATCH_NAME macro
   as the dispatching model name, usable as a C identifier. */
#if   defined(JITTER_DISPATCH_SWITCH)
# define JITTER_DISPATCH_NAME switch
#elif defined(JITTER_DISPATCH_DIRECT_THREADING)
# define JITTER_DISPATCH_NAME direct_threading
#elif defined(JITTER_DISPATCH_MINIMAL_THREADING)
# define JITTER_DISPATCH_NAME minimal_threading
  /* Minimal threading requires code replication. */
# define JITTER_REPLICATE 1
#elif defined(JITTER_DISPATCH_NO_THREADING)
# define JITTER_DISPATCH_NAME no_threading
  /* No-threading requires code replication. */
# define JITTER_REPLICATE 1
#else
# error "unknown dispatching model.  This should never happen."
#endif // #if defined(JITTER_DISPATCH_...)




/* The selected dispatching model name as a string or a C identifier.
 * ************************************************************************** */

/* Define the dispatching model name as a string literal. */
#define JITTER_DISPATCH_NAME_STRING       \
  JITTER_STRINGIFY(JITTER_DISPATCH_NAME)

/* Compute the name of a C global whose name depends on the dispatching model.
   Only the one for the selected model is defined, and this serves to prevent
   mistakes when linking jitterc-generated code to a runtime library; the
   definition is in jitter.c and the code using the global is in vm1.c . */
#define JITTER_DISPATCH_DEPENDENT_GLOBAL_NAME                \
  JITTER_CONCATENATE_THREE(jitter_this_is_the_runtime_for_,  \
                           JITTER_DISPATCH_NAME,             \
                           _dispatch)

#endif //#ifndef JITTER_DISPATCH_H_
