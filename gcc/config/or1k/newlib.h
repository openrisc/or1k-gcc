/* Definitions for using newlib userspace.
   Copyright (C) 2016 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */


#undef CPP_SPEC
#define CPP_SPEC "%{!mnewlib:%{posix:-D_POSIX_SOURCE} %{pthread:-D_REENTRANT}}"

/* Make sure we pick up the crti.o, crtbegin.o, crtend.o and crtn.o files. */
#undef STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared:%{pie:Scrt0.o%s;:crt0.o%s}} crti.o%s \
   %{static:crtbeginT.o%s;shared|pie:crtbeginS.o%s;:crtbegin.o%s}"

#undef ENDFILE_SPEC
#define ENDFILE_SPEC "%{shared|pie:crtendS.o%s;:crtend.o%s} crtn.o%s"

#undef LINK_SPEC
#define LINK_SPEC "%{mnewlib:-entry 0x100} %{static:-static} %{shared:-shared}"

/* Override previous definitions (linux.h). Newlib doesn't have a profiling
   version of the library, but it does have a debugging version (libg.a) */
#undef LIB_SPEC
#define LIB_SPEC "%{!mnewlib:%{pthread:-lpthread}	\
		             %{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p}}"			\
                 "%{mnewlib:%{!g:-lc} %{g:-lg} -lor1k					\
                            %{mboard=*:-lboard-%*} %{!mboard=*:-lboard-or1ksim}		\
                            %{!g:-lc} %{g:-lg}						\
                            }"

#define DRIVER_SELF_SPECS "%{!mno-newlib:-mnewlib}"
