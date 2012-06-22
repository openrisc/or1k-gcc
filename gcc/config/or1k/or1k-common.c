/* Common hooks for or1k
   Copyright (C) 1987, 1992, 1997, 1999, 2000, 2001, 2002, 2003, 2004,
   2005, 2006, 2007, 2008, 2009, 2010, 2011  Free Software Foundation, Inc
   Copyright (C) 2012 Peter Gavin

   This file is part of GNU CC.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option)
   any later version.
  
   This program is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
   more details.
  
   You should have received a copy of the GNU General Public License along
   with this program.  If not, see <http://www.gnu.org/licenses/>. */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "common/common-target.h"
#include "common/common-target-def.h"
#include "opts.h"
#include "flags.h"

static const struct default_options or1k_option_optimization_table[] =
  {
    { OPT_LEVELS_1_PLUS, OPT_fomit_frame_pointer, NULL, 1 },
    { OPT_LEVELS_NONE, 0, NULL, 0 }
  };

/* Implement TARGET_HANDLE_OPTION.  */


static bool
or1k_handle_option (struct gcc_options *opts,
                   struct gcc_options *opts_set ATTRIBUTE_UNUSED,
                   const struct cl_decoded_option *decoded,
                   location_t loc ATTRIBUTE_UNUSED)
{
  switch (code)
    {
    case OPT_mnewlib:
      or1k_libc = or1k_libc_newlib;
      return true;
    case OPT_muclibc:
      or1k_libc = or1k_libc_uclibc;
      return true;
    case OPT_mglibc:
      or1k_libc = or1k_libc_glibc;
      return false;
    default:
      return true;
    }
}

#undef  TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS TARGET_CPU_DEFAULT
#undef  TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION or1k_handle_option
#undef  TARGET_OPTION_OPTIMIZATION_TABLE
#define TARGET_OPTION_OPTIMIZATION_TABLE or1k_option_optimization_table

#undef  TARGET_EXCEPT_UNWIND_INFO
#define TARGET_EXCEPT_UNWIND_INFO              sjlj_except_unwind_info

struct gcc_targetm_common targetm_common = TARGETM_COMMON_INITIALIZER;
