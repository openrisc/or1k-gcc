/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                           I N I T I A L I Z E                            *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2012, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/*  This unit provides default implementation for __gnat_initialize ()
    which is called before the elaboration of the partition. It is provided
    in a separate file/object so that users can replace it easily.
    The default implementation should be null on most targets.  */

#ifdef __cplusplus
extern "C" {
#endif

/* The following include is here to meet the published VxWorks requirement
   that the __vxworks header appear before any other include.  */
#ifdef __vxworks
#include "vxWorks.h"
#endif

#ifdef IN_RTS
#include "tconfig.h"
#include "tsystem.h"
/* We don't have libiberty, so use malloc.  */
#define xmalloc(S) malloc (S)
#define xrealloc(V,S) realloc (V,S)
#else
#include "config.h"
#include "system.h"
#endif

#include "raise.h"

/******************************************/
/* __gnat_initialize (NT-mingw32 Version) */
/******************************************/

#if defined (__MINGW32__)
#include "mingw32.h"
#include <windows.h>

extern void __gnat_init_float (void);
extern void __gnat_install_SEH_handler (void *);

extern int gnat_argc;
extern char **gnat_argv;

#ifdef GNAT_UNICODE_SUPPORT

#define EXPAND_ARGV_RATE 128

static void
append_arg (int *index, LPWSTR dir, LPWSTR value,
	    char ***argv, int *last, int quoted)
{
  int size;
  LPWSTR fullvalue;
  int vallen = _tcslen (value);
  int dirlen;

  if (dir == NULL)
    {
      /* no dir prefix */
      dirlen = 0;
      fullvalue = xmalloc ((vallen + 1) * sizeof(TCHAR));
    }
  else
    {
      /* Add dir first */
      dirlen = _tcslen (dir);

      fullvalue = xmalloc ((dirlen + vallen + 1) * sizeof(TCHAR));
      _tcscpy (fullvalue, dir);
    }

  /* Append value */

  if (quoted)
    {
      _tcsncpy (fullvalue + dirlen, value + 1, vallen - 1);
      fullvalue [dirlen + vallen - sizeof(TCHAR)] = _T('\0');
    }
  else
    _tcscpy (fullvalue + dirlen, value);

  if (*last <= *index)
    {
      *last += EXPAND_ARGV_RATE;
      *argv = (char **) xrealloc (*argv, (*last) * sizeof (char *));
    }

  size = WS2SC (NULL, fullvalue, 0);
  (*argv)[*index] = (char *) xmalloc (size + sizeof(TCHAR));
  WS2SC ((*argv)[*index], fullvalue, size);

  free (fullvalue);

  (*index)++;
}
#endif

void
__gnat_initialize (void *eh ATTRIBUTE_UNUSED)
{
   /* Initialize floating-point coprocessor. This call is needed because
      the MS libraries default to 64-bit precision instead of 80-bit
      precision, and we require the full precision for proper operation,
      given that we have set Max_Digits etc with this in mind */
   __gnat_init_float ();

#ifdef GNAT_UNICODE_SUPPORT
   /* Set current code page for filenames handling. */
   {
     char *codepage = getenv ("GNAT_CODE_PAGE");

     /* Default code page is UTF-8.  */
     CurrentCodePage = CP_UTF8;

     if (codepage != NULL)
       {
	 if (strcmp (codepage, "CP_ACP") == 0)
	   CurrentCodePage = CP_ACP;
	 else if (strcmp (codepage, "CP_UTF8") == 0)
	   CurrentCodePage = CP_UTF8;
       }
   }

   /* Adjust gnat_argv to support Unicode characters. */
   {
     LPWSTR *wargv;
     int wargc;
     int k;
     int last;
     int argc_expanded = 0;
     TCHAR result [MAX_PATH];
     int quoted;

     wargv = CommandLineToArgvW (GetCommandLineW(), &wargc);

     if (wargv != NULL)
       {
	 /* Set gnat_argv with arguments encoded in UTF-8. */
	 last = wargc + 1;
	 gnat_argv = (char **) xmalloc ((last) * sizeof (char *));

	 /* argv[0] is the executable full path-name. */

	 SearchPath (NULL, wargv[0], _T(".exe"), MAX_PATH, result, NULL);
	 append_arg (&argc_expanded, NULL, result, &gnat_argv, &last, 0);

	 for (k=1; k<wargc; k++)
	   {
	     quoted = (wargv[k][0] == _T('\''));

	     /* Check for wildcard expansion if the argument is not quoted. */
	     if (!quoted
		 && (_tcsstr (wargv[k], _T("?")) != 0 ||
		     _tcsstr (wargv[k], _T("*")) != 0))
	       {
		 /* Wilcards are present, append all corresponding matches. */
		 WIN32_FIND_DATA FileData;
		 HANDLE hDir = FindFirstFile (wargv[k], &FileData);
		 LPWSTR dir = NULL;
		 LPWSTR ldir = _tcsrchr (wargv[k], _T('\\'));

		 if (ldir == NULL)
		   ldir = _tcsrchr (wargv[k], _T('/'));

		 if (hDir == INVALID_HANDLE_VALUE)
		   {
		     /* No match, append arg as-is. */
		     append_arg (&argc_expanded, NULL, wargv[k],
				 &gnat_argv, &last, quoted);
		   }
		 else
		   {
		     if (ldir != NULL)
		       {
			 int n = ldir - wargv[k] + 1;
			 dir = xmalloc ((n + 1) * sizeof (TCHAR));
			 _tcsncpy (dir, wargv[k], n);
			 dir[n] = _T('\0');
		       }

		     /* Append first match and all remaining ones.  */

		     do {
		       /* Do not add . and .. special entries */

		       if (_tcscmp (FileData.cFileName, _T(".")) != 0
			   && _tcscmp (FileData.cFileName, _T("..")) != 0)
			 append_arg (&argc_expanded, dir, FileData.cFileName,
				     &gnat_argv, &last, 0);
		     } while (FindNextFile (hDir, &FileData));

		     FindClose (hDir);

		     if (dir != NULL)
		       free (dir);
		   }
	       }
	     else
	       {
		 /*  No wildcard. Store parameter as-is. Remove quote if
		     needed. */
		 append_arg (&argc_expanded, NULL, wargv[k],
			     &gnat_argv, &last, quoted);
	       }
	   }

	 LocalFree (wargv);
	 gnat_argc = argc_expanded;
	 gnat_argv = (char **) xrealloc
	   (gnat_argv, argc_expanded * sizeof (char *));
       }
   }
#endif

   /* Note that we do not activate this for the compiler itself to avoid a
      bootstrap path problem.  Older version of gnatbind will generate a call
      to __gnat_initialize() without argument. Therefore we cannot use eh in
      this case.  It will be possible to remove the following #ifdef at some
      point.  */
#ifdef IN_RTS
   /* Install the Structured Exception handler.  */
   if (eh)
     __gnat_install_SEH_handler (eh);
#endif
}

/******************************************/
/* __gnat_initialize (init_float version) */
/******************************************/

#elif defined (__Lynx__) || defined (__FreeBSD__) || defined(__NetBSD__) \
  || defined (__OpenBSD__)

extern void __gnat_init_float (void);

void
__gnat_initialize (void *eh ATTRIBUTE_UNUSED)
{
   __gnat_init_float ();
}

/***************************************/
/* __gnat_initialize (VxWorks Version) */
/***************************************/

#elif defined(__vxworks)

extern void __gnat_init_float (void);

void
__gnat_initialize (void *eh)
{
  __gnat_init_float ();
}

#elif defined(_T_HPUX10) || (!defined(IN_RTS) && defined(_X_HPUX10))

/************************************************/
/* __gnat_initialize (PA-RISC HP-UX 10 Version) */
/************************************************/

extern void __main (void);

void
__gnat_initialize (void *eh ATTRIBUTE_UNUSED)
{
  __main ();
}

#else

/* For all other versions of GNAT, the initialize routine and handler
   installation do nothing */

/***************************************/
/* __gnat_initialize (Default Version) */
/***************************************/

void
__gnat_initialize (void *eh ATTRIBUTE_UNUSED)
{
}

#endif

#ifdef __cplusplus
}
#endif
