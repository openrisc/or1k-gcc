/* Definitions for or1k running Linux-based GNU systems using ELF
   Copyright (C) 2002, 2005
   Free Software Foundation, Inc.
   Contributed by Marko Mlinar <markom@opencores.org>

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* elfos.h should have already been included.  Now just override
   any conflicting definitions and add any extras.  */

#define NO_IMPLICIT_EXTERN_C

#define TARGET_OS_CPP_BUILTINS() \
  GNU_USER_TARGET_OS_CPP_BUILTINS ()

#define GLIBC_DYNAMIC_LINKER "/lib/ld.so.1"

#undef MUSL_DYNAMIC_LINKER
#define MUSL_DYNAMIC_LINKER  "/lib/ld.so.1"

#undef LINK_SPEC
#define LINK_SPEC "%{h*}			\
   %{static:-Bstatic}				\
   %{shared:-shared}				\
   %{symbolic:-Bsymbolic}			\
   %{!static:					\
     %{rdynamic:-export-dynamic}		\
     %{!shared:-dynamic-linker " GNU_USER_DYNAMIC_LINKER "}}"
