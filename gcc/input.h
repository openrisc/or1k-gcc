/* Declarations for variables relating to reading the source file.
   Used by parsers, lexical analyzers, and error message routines.
   Copyright (C) 1993, 1997, 1998, 2000, 2003, 2004 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef GCC_INPUT_H
#define GCC_INPUT_H

#include "line-map.h"
extern struct line_maps line_table;

/* The location for declarations in "<built-in>" */
#define BUILTINS_LOCATION ((source_location) 2)

typedef struct location_s GTY(())
{
  /* The name of the source file involved.  */
  const char *file;

  /* The line-location in the source file.  */
  int line;

  /* FUTURE (but confuses gentype): int column. */
} expanded_location;

#ifdef USE_MAPPED_LOCATION

extern expanded_location expand_location (source_location);

#define UNKNOWN_LOCATION ((source_location) 0)
typedef source_location location_t; /* deprecated typedef */
typedef source_location source_locus; /* to be removed */

#else /* ! USE_MAPPED_LOCATION */

typedef struct location_s location_t;
typedef location_t *source_locus;

#define expand_location(FILELINE) (FILELINE)
extern location_t unknown_location;
#define UNKNOWN_LOCATION unknown_location

#endif /* ! USE_MAPPED_LOCATION */

struct file_stack
{
  struct file_stack *next;
  location_t location;
};

/* Top-level source file.  */
extern const char *main_input_filename;

extern location_t input_location;
#ifdef USE_MAPPED_LOCATION
extern void push_srcloc (location_t);
#else /* ! USE_MAPPED_LOCATION */
extern void push_srcloc (const char *name, int line);
#endif /* ! USE_MAPPED_LOCATION */
extern void pop_srcloc (void);

#define LOCATION_FILE(LOC) ((expand_location (LOC)).file)
#define LOCATION_LINE(LOC) ((expand_location (LOC)).line)

#define input_line LOCATION_LINE(input_location)
#define input_filename LOCATION_FILE(input_location)

/* Stack of currently pending input files.
   The line member is not accurate for the innermost file on the stack.  */
extern struct file_stack *input_file_stack;

/* Incremented on each change to input_file_stack.  */
extern int input_file_stack_tick;

#endif
