/* pk-map.h - Support for map files.  */

/* Copyright (C) 2020 Jose E. Marchesi */

/* This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/* Poke maintains a set of "named maps" associated with each open IO
   space.  These maps are collections of mapped variables which are
   defined in the top-level environment of the incremental compiler.

   This file provides services to access both the global map and the
   named maps.  */

#ifndef PK_MAP_H
#define PK_MAP_H

#include <config.h>

#include "libpoke.h"

/* Each map entry corresponds to a mapped value in the top-level
   environment.

   VARNAME is the name of a mapped variable defined in the poke
   top-level environment.  Therefore there cannot be two map entries
   with the same name.

   OFFSET is the offset where the entry is mapped.

   CHAIN is a pointer to another map entry, or NULL.  */

#define PK_MAP_ENTRY_VARNAME(ENTRY) ((ENTRY)->varname)
#define PK_MAP_ENTRY_OFFSET(ENTRY) ((ENTRY)->offset)
#define PK_MAP_ENTRY_CHAIN(ENTRY) ((ENTRY)->chain)

struct pk_map_entry
{
  char *varname;
  pk_val offset;
  struct pk_map_entry *chain;
};

typedef struct pk_map_entry *pk_map_entry;

/* Poke maps are ordered sets of map entries.

   NAME is the name of the map.

   SOURCE is a string describing the origin of this map.  For maps
   loaded from files, this contains the path of the file.  For maps
   created by the user using commands, this is NULL.

   ENTRIES is a list of chained map entries.

   CHAIN is a pointer to another pk map, or NULL.  */

#define PK_MAP_NAME(MAP) ((MAP)->name)
#define PK_MAP_SOURCE(MAP) ((MAP)->source)
#define PK_MAP_ENTRIES(MAP) ((MAP)->entries)
#define PK_MAP_CHAIN(MAP) ((MAP)->chain)

struct pk_map
{
  char *name;
  char *source;
  struct pk_map_entry *entries;
  struct pk_map *chain;
};

typedef struct pk_map *pk_map;

/* Status codes returned by the functions below.  */

#define PK_MAP_OK 0
#define PK_MAP_EINVNAME 1
#define PK_MAP_EINVIOS 2

/* Create a new empty map, associated to a given IO space.

   MAPNAME is a NULL-terminated string with the name of the map.

   SOURCE is a NULL-terminated string with the source of the map.
   This is either the path of the file from which the map was loaded,
   or NULL if the map was created interactively.

   IOS_ID is the id of an existing IO space.

   If there is already a map named MAPNAME associated with the given
   IO space, this function returns 0.  Otherwise this function returns
   1.  */

int pk_map_create (int ios_id, const char *mapname, const char *source);

/* Add a new entry to a map.

   IOS_ID is the id of the IO space associated with the map.
   MAPNAME is the name of the map to which the entry will be added.
   VARNAME is the name of a variable.
   OFFSET is the offset where the variable is mapped.

   If there is already an entry with the given VARNAME in the given
   map MAPNAME, then return 0.  Return 1 otherwise.  */

int pk_map_add_entry (int ios_id, const char *mapname,
                      const char *varname, pk_val offset);

/* Remove an entry from a map.

   IOS_ID si the id of the IO space associated with the map.

   MAPNAME is the name of the map from which the entry will be
   removed.

   VARNAME is the name of a variable, used to identify the entry to
   remove.

   If the map doesn't exist, or there is no entry for variable
   VARNAME, return 0.  Otherwise return 1.  */

int pk_map_remove_entry (int ios_id, const char *mapname,
                         const char *varname);

int pk_map_load_file (const char *path);
int pk_map_save_file (const char *path);

/* Initialize the global map.   */

void pk_map_init (void);

/* Free all the resources used by the global map.  */

void pk_map_shutdown (void);

/* Return a chained list of maps defined in the given IOS.  */

pk_map pk_map_get_maps (int ios_id);

/* Search for a map by name.

   IOS_ID is the ID of an IOS.
   NAME is the name of the map.

   Return the map if found.  NULL otherwise.  */

pk_map pk_map_search (int ios_id, const char *name);

#endif /* ! PK_MAP_H */