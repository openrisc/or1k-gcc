/* go-caller.c -- runtime.Caller and runtime.FuncForPC for Go.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

/* Implement runtime.Caller.  */

#include <stdint.h>

#include "backtrace.h"

#include "runtime.h"

/* Get the function name, file name, and line number for a PC value.
   We use the backtrace library to get this.  */

/* Data structure to gather file/line information.  */

struct caller
{
  String fn;
  String file;
  intgo line;
};

/* Collect file/line information for a PC value.  If this is called
   more than once, due to inlined functions, we use the last call, as
   that is usually the most useful one.  */

static int
callback (void *data, uintptr_t pc __attribute__ ((unused)),
	  const char *filename, int lineno, const char *function)
{
  struct caller *c = (struct caller *) data;

  if (function == NULL)
    {
      c->fn.str = NULL;
      c->fn.len = 0;
    }
  else
    {
      byte *s;

      c->fn.len = __builtin_strlen (function);
      s = runtime_malloc (c->fn.len);
      __builtin_memcpy (s, function, c->fn.len);
      c->fn.str = s;
    }

  if (filename == NULL)
    {
      c->file.str = NULL;
      c->file.len = 0;
    }
  else
    {
      byte *s;

      c->file.len = __builtin_strlen (filename);
      s = runtime_malloc (c->file.len);
      __builtin_memcpy (s, filename, c->file.len);
      c->file.str = s;
    }

  c->line = lineno;

  return 0;
}

/* The error callback for backtrace_pcinfo and backtrace_syminfo.  */

static void
error_callback (void *data __attribute__ ((unused)),
		const char *msg, int errnum)
{
  if (errnum == -1)
    return;
  if (errnum > 0)
    runtime_printf ("%s errno %d\n", msg, errnum);
  runtime_throw (msg);
}

/* The backtrace library state.  */

static void *back_state;

/* A lock to control creating back_state.  */

static Lock back_state_lock;

/* Fetch back_state, creating it if necessary.  */

struct backtrace_state *
__go_get_backtrace_state ()
{
  runtime_lock (&back_state_lock);
  if (back_state == NULL)
    {
      const char *filename;

      filename = (const char *) runtime_progname ();
      back_state = backtrace_create_state (filename, 1, error_callback, NULL);
    }
  runtime_unlock (&back_state_lock);
  return back_state;
}

/* Return function/file/line information for PC.  */

_Bool
__go_file_line (uintptr pc, String *fn, String *file, intgo *line)
{
  struct caller c;

  runtime_memclr (&c, sizeof c);
  backtrace_pcinfo (__go_get_backtrace_state (), pc, callback,
		    error_callback, &c);
  *fn = c.fn;
  *file = c.file;
  *line = c.line;
  return c.file.len > 0;
}

/* Collect symbol information.  */

static void
syminfo_callback (void *data, uintptr_t pc __attribute__ ((unused)),
		  const char *symname __attribute__ ((unused)),
		  uintptr_t address)
{
  uintptr_t *pval = (uintptr_t *) data;

  *pval = address;
}

/* Set *VAL to the value of the symbol for PC.  */

static _Bool
__go_symbol_value (uintptr_t pc, uintptr_t *val)
{
  *val = 0;
  backtrace_syminfo (__go_get_backtrace_state (), pc, syminfo_callback,
		     error_callback, val);
  return *val != 0;
}

/* The values returned by runtime.Caller.  */

struct caller_ret
{
  uintptr_t pc;
  String file;
  intgo line;
  _Bool ok;
};

struct caller_ret Caller (int n) asm ("runtime.Caller");

Func *FuncForPC (uintptr_t) asm ("runtime.FuncForPC");

/* Implement runtime.Caller.  */

struct caller_ret
Caller (int skip)
{
  struct caller_ret ret;
  uintptr pc;
  int32 n;
  String fn;

  runtime_memclr (&ret, sizeof ret);
  n = runtime_callers (skip + 1, &pc, 1);
  if (n < 1)
    return ret;
  ret.pc = pc;
  __go_file_line (pc, &fn, &ret.file, &ret.line);
  ret.ok = 1;
  return ret;
}

/* Implement runtime.FuncForPC.  */

Func *
FuncForPC (uintptr_t pc)
{
  Func *ret;
  String fn;
  String file;
  intgo line;
  uintptr_t val;

  if (!__go_file_line (pc, &fn, &file, &line))
    return NULL;

  ret = (Func *) runtime_malloc (sizeof (*ret));
  ret->name = fn;

  if (__go_symbol_value (pc, &val))
    ret->entry = val;
  else
    ret->entry = 0;

  return ret;
}

/* Look up the file and line information for a PC within a
   function.  */

struct funcline_go_return
{
  String retfile;
  intgo retline;
};

struct funcline_go_return
runtime_funcline_go (Func *f, uintptr targetpc)
  __asm__ ("runtime.funcline_go");

struct funcline_go_return
runtime_funcline_go (Func *f __attribute__((unused)), uintptr targetpc)
{
  struct funcline_go_return ret;
  String fn;

  if (!__go_file_line (targetpc, &fn, &ret.retfile,  &ret.retline))
    runtime_memclr (&ret, sizeof ret);
  return ret;
}
