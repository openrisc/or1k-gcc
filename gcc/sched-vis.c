/* Printing of RTL in "slim", mnemonic like form.
   Copyright (C) 1992-2012
   Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@cygnus.com) Enhanced by,
   and currently maintained by, Jim Wilson (wilson@cygnus.com)

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Historically this form of RTL dumping was introduced along with
   the Haifa instruction scheduling pass, hence the name of this file.
   But there is nothing in this file left that is scheduler-specific.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"	/* FIXME: To dump INSN_VAR_LOCATION_DECL.  */
#include "obstack.h"
#include "hard-reg-set.h"
#include "basic-block.h"
#include "insn-attr.h"
#include "dumpfile.h"	/* for the TDF_* flags */

static char *safe_concat (char *, char *, const char *);

#define BUF_LEN 2048

static char *
safe_concat (char *buf, char *cur, const char *str)
{
  char *end = buf + BUF_LEN - 2;	/* Leave room for null.  */
  int c;

  if (cur > end)
    {
      *end = '\0';
      return end;
    }

  while (cur < end && (c = *str++) != '\0')
    *cur++ = c;

  *cur = '\0';
  return cur;
}

/* This recognizes rtx, I classified as expressions.  These are always
   represent some action on values or results of other expression, that
   may be stored in objects representing values.  */

static void
print_exp (char *buf, const_rtx x, int verbose)
{
  char tmp[BUF_LEN];
  const char *st[4];
  char *cur = buf;
  const char *fun = (char *) 0;
  const char *sep;
  rtx op[4];
  int i;

  for (i = 0; i < 4; i++)
    {
      st[i] = (char *) 0;
      op[i] = NULL_RTX;
    }

  switch (GET_CODE (x))
    {
    case PLUS:
      op[0] = XEXP (x, 0);
      if (CONST_INT_P (XEXP (x, 1))
	  && INTVAL (XEXP (x, 1)) < 0)
	{
	  st[1] = "-";
	  op[1] = GEN_INT (-INTVAL (XEXP (x, 1)));
	}
      else
	{
	  st[1] = "+";
	  op[1] = XEXP (x, 1);
	}
      break;
    case LO_SUM:
      op[0] = XEXP (x, 0);
      st[1] = "+low(";
      op[1] = XEXP (x, 1);
      st[2] = ")";
      break;
    case MINUS:
      op[0] = XEXP (x, 0);
      st[1] = "-";
      op[1] = XEXP (x, 1);
      break;
    case COMPARE:
      fun = "cmp";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case NEG:
      st[0] = "-";
      op[0] = XEXP (x, 0);
      break;
    case FMA:
      st[0] = "{";
      op[0] = XEXP (x, 0);
      st[1] = "*";
      op[1] = XEXP (x, 1);
      st[2] = "+";
      op[2] = XEXP (x, 2);
      st[3] = "}";
      break;
    case MULT:
      op[0] = XEXP (x, 0);
      st[1] = "*";
      op[1] = XEXP (x, 1);
      break;
    case DIV:
      op[0] = XEXP (x, 0);
      st[1] = "/";
      op[1] = XEXP (x, 1);
      break;
    case UDIV:
      fun = "udiv";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case MOD:
      op[0] = XEXP (x, 0);
      st[1] = "%";
      op[1] = XEXP (x, 1);
      break;
    case UMOD:
      fun = "umod";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case SMIN:
      fun = "smin";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case SMAX:
      fun = "smax";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case UMIN:
      fun = "umin";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case UMAX:
      fun = "umax";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      break;
    case NOT:
      st[0] = "!";
      op[0] = XEXP (x, 0);
      break;
    case AND:
      op[0] = XEXP (x, 0);
      st[1] = "&";
      op[1] = XEXP (x, 1);
      break;
    case IOR:
      op[0] = XEXP (x, 0);
      st[1] = "|";
      op[1] = XEXP (x, 1);
      break;
    case XOR:
      op[0] = XEXP (x, 0);
      st[1] = "^";
      op[1] = XEXP (x, 1);
      break;
    case ASHIFT:
      op[0] = XEXP (x, 0);
      st[1] = "<<";
      op[1] = XEXP (x, 1);
      break;
    case LSHIFTRT:
      op[0] = XEXP (x, 0);
      st[1] = " 0>>";
      op[1] = XEXP (x, 1);
      break;
    case ASHIFTRT:
      op[0] = XEXP (x, 0);
      st[1] = ">>";
      op[1] = XEXP (x, 1);
      break;
    case ROTATE:
      op[0] = XEXP (x, 0);
      st[1] = "<-<";
      op[1] = XEXP (x, 1);
      break;
    case ROTATERT:
      op[0] = XEXP (x, 0);
      st[1] = ">->";
      op[1] = XEXP (x, 1);
      break;
    case NE:
      op[0] = XEXP (x, 0);
      st[1] = "!=";
      op[1] = XEXP (x, 1);
      break;
    case EQ:
      op[0] = XEXP (x, 0);
      st[1] = "==";
      op[1] = XEXP (x, 1);
      break;
    case GE:
      op[0] = XEXP (x, 0);
      st[1] = ">=";
      op[1] = XEXP (x, 1);
      break;
    case GT:
      op[0] = XEXP (x, 0);
      st[1] = ">";
      op[1] = XEXP (x, 1);
      break;
    case LE:
      op[0] = XEXP (x, 0);
      st[1] = "<=";
      op[1] = XEXP (x, 1);
      break;
    case LT:
      op[0] = XEXP (x, 0);
      st[1] = "<";
      op[1] = XEXP (x, 1);
      break;
    case SIGN_EXTRACT:
      fun = (verbose) ? "sign_extract" : "sxt";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      op[2] = XEXP (x, 2);
      break;
    case ZERO_EXTRACT:
      fun = (verbose) ? "zero_extract" : "zxt";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      op[2] = XEXP (x, 2);
      break;
    case SIGN_EXTEND:
      fun = (verbose) ? "sign_extend" : "sxn";
      op[0] = XEXP (x, 0);
      break;
    case ZERO_EXTEND:
      fun = (verbose) ? "zero_extend" : "zxn";
      op[0] = XEXP (x, 0);
      break;
    case FLOAT_EXTEND:
      fun = (verbose) ? "float_extend" : "fxn";
      op[0] = XEXP (x, 0);
      break;
    case TRUNCATE:
      fun = (verbose) ? "trunc" : "trn";
      op[0] = XEXP (x, 0);
      break;
    case FLOAT_TRUNCATE:
      fun = (verbose) ? "float_trunc" : "ftr";
      op[0] = XEXP (x, 0);
      break;
    case FLOAT:
      fun = (verbose) ? "float" : "flt";
      op[0] = XEXP (x, 0);
      break;
    case UNSIGNED_FLOAT:
      fun = (verbose) ? "uns_float" : "ufl";
      op[0] = XEXP (x, 0);
      break;
    case FIX:
      fun = "fix";
      op[0] = XEXP (x, 0);
      break;
    case UNSIGNED_FIX:
      fun = (verbose) ? "uns_fix" : "ufx";
      op[0] = XEXP (x, 0);
      break;
    case PRE_DEC:
      st[0] = "--";
      op[0] = XEXP (x, 0);
      break;
    case PRE_INC:
      st[0] = "++";
      op[0] = XEXP (x, 0);
      break;
    case POST_DEC:
      op[0] = XEXP (x, 0);
      st[1] = "--";
      break;
    case POST_INC:
      op[0] = XEXP (x, 0);
      st[1] = "++";
      break;
    case PRE_MODIFY:
      st[0] = "pre ";
      op[0] = XEXP (XEXP (x, 1), 0);
      st[1] = "+=";
      op[1] = XEXP (XEXP (x, 1), 1);
      break;
    case POST_MODIFY:
      st[0] = "post ";
      op[0] = XEXP (XEXP (x, 1), 0);
      st[1] = "+=";
      op[1] = XEXP (XEXP (x, 1), 1);
      break;
    case CALL:
      st[0] = "call ";
      op[0] = XEXP (x, 0);
      if (verbose)
	{
	  st[1] = " argc:";
	  op[1] = XEXP (x, 1);
	}
      break;
    case IF_THEN_ELSE:
      st[0] = "{(";
      op[0] = XEXP (x, 0);
      st[1] = ")?";
      op[1] = XEXP (x, 1);
      st[2] = ":";
      op[2] = XEXP (x, 2);
      st[3] = "}";
      break;
    case TRAP_IF:
      fun = "trap_if";
      op[0] = TRAP_CONDITION (x);
      break;
    case PREFETCH:
      fun = "prefetch";
      op[0] = XEXP (x, 0);
      op[1] = XEXP (x, 1);
      op[2] = XEXP (x, 2);
      break;
    case UNSPEC:
    case UNSPEC_VOLATILE:
      {
	cur = safe_concat (buf, cur, "unspec");
	if (GET_CODE (x) == UNSPEC_VOLATILE)
	  cur = safe_concat (buf, cur, "/v");
	cur = safe_concat (buf, cur, "[");
	sep = "";
	for (i = 0; i < XVECLEN (x, 0); i++)
	  {
	    print_pattern (tmp, XVECEXP (x, 0, i), verbose);
	    cur = safe_concat (buf, cur, sep);
	    cur = safe_concat (buf, cur, tmp);
	    sep = ",";
	  }
	cur = safe_concat (buf, cur, "] ");
	sprintf (tmp, "%d", XINT (x, 1));
	cur = safe_concat (buf, cur, tmp);
      }
      break;
    default:
      {
	/* Most unhandled codes can be printed as pseudo-functions.  */
        if (GET_RTX_CLASS (GET_CODE (x)) == RTX_UNARY)
	  {
	    fun = GET_RTX_NAME (GET_CODE (x));
	    op[0] = XEXP (x, 0);
	  }
        else if (GET_RTX_CLASS (GET_CODE (x)) == RTX_COMPARE
		 || GET_RTX_CLASS (GET_CODE (x)) == RTX_COMM_COMPARE
		 || GET_RTX_CLASS (GET_CODE (x)) == RTX_BIN_ARITH
		 || GET_RTX_CLASS (GET_CODE (x)) == RTX_COMM_ARITH)
	  {
	    fun = GET_RTX_NAME (GET_CODE (x));
	    op[0] = XEXP (x, 0);
	    op[1] = XEXP (x, 1);
	  }
        else if (GET_RTX_CLASS (GET_CODE (x)) == RTX_TERNARY)
	  {
	    fun = GET_RTX_NAME (GET_CODE (x));
	    op[0] = XEXP (x, 0);
	    op[1] = XEXP (x, 1);
	    op[2] = XEXP (x, 2);
	  }
	else
	  /* Give up, just print the RTX name.  */
	  st[0] = GET_RTX_NAME (GET_CODE (x));
      }
      break;
    }

  /* Print this as a function?  */
  if (fun)
    {
      cur = safe_concat (buf, cur, fun);
      cur = safe_concat (buf, cur, "(");
    }

  for (i = 0; i < 4; i++)
    {
      if (st[i])
	cur = safe_concat (buf, cur, st[i]);

      if (op[i])
	{
	  if (fun && i != 0)
	    cur = safe_concat (buf, cur, ",");

	  print_value (tmp, op[i], verbose);
	  cur = safe_concat (buf, cur, tmp);
	}
    }

  if (fun)
    cur = safe_concat (buf, cur, ")");
}		/* print_exp */

/* Prints rtxes, I customarily classified as values.  They're constants,
   registers, labels, symbols and memory accesses.  */

void
print_value (char *buf, const_rtx x, int verbose)
{
  char t[BUF_LEN];
  char *cur = buf;

  if (!x)
    {
      safe_concat (buf, buf, "(nil)");
      return;
    }
  switch (GET_CODE (x))
    {
    case CONST_INT:
      sprintf (t, HOST_WIDE_INT_PRINT_HEX,
	       (unsigned HOST_WIDE_INT) INTVAL (x));
      cur = safe_concat (buf, cur, t);
      break;
    case CONST_DOUBLE:
      if (FLOAT_MODE_P (GET_MODE (x)))
	real_to_decimal (t, CONST_DOUBLE_REAL_VALUE (x), sizeof (t), 0, 1);
      else
	sprintf (t,
		 "<" HOST_WIDE_INT_PRINT_HEX "," HOST_WIDE_INT_PRINT_HEX ">",
		 (unsigned HOST_WIDE_INT) CONST_DOUBLE_LOW (x),
		 (unsigned HOST_WIDE_INT) CONST_DOUBLE_HIGH (x));
      cur = safe_concat (buf, cur, t);
      break;
    case CONST_FIXED:
      fixed_to_decimal (t, CONST_FIXED_VALUE (x), sizeof (t));
      cur = safe_concat (buf, cur, t);
      break;
    case CONST_STRING:
      cur = safe_concat (buf, cur, "\"");
      cur = safe_concat (buf, cur, XSTR (x, 0));
      cur = safe_concat (buf, cur, "\"");
      break;
    case SYMBOL_REF:
      cur = safe_concat (buf, cur, "`");
      cur = safe_concat (buf, cur, XSTR (x, 0));
      cur = safe_concat (buf, cur, "'");
      break;
    case LABEL_REF:
      sprintf (t, "L%d", INSN_UID (XEXP (x, 0)));
      cur = safe_concat (buf, cur, t);
      break;
    case CONST:
      print_value (t, XEXP (x, 0), verbose);
      cur = safe_concat (buf, cur, "const(");
      cur = safe_concat (buf, cur, t);
      cur = safe_concat (buf, cur, ")");
      break;
    case HIGH:
      print_value (t, XEXP (x, 0), verbose);
      cur = safe_concat (buf, cur, "high(");
      cur = safe_concat (buf, cur, t);
      cur = safe_concat (buf, cur, ")");
      break;
    case REG:
      if (REGNO (x) < FIRST_PSEUDO_REGISTER)
	{
	  int c = reg_names[REGNO (x)][0];
	  if (ISDIGIT (c))
	    cur = safe_concat (buf, cur, "%");

	  cur = safe_concat (buf, cur, reg_names[REGNO (x)]);
	}
      else
	{
	  sprintf (t, "r%d", REGNO (x));
	  cur = safe_concat (buf, cur, t);
	}
      if (verbose)
	{
	  sprintf (t, ":%s", GET_MODE_NAME (GET_MODE (x)));
	  cur = safe_concat (buf, cur, t);
	}
      break;
    case SUBREG:
      print_value (t, SUBREG_REG (x), verbose);
      cur = safe_concat (buf, cur, t);
      sprintf (t, "#%d", SUBREG_BYTE (x));
      cur = safe_concat (buf, cur, t);
      break;
    case STRICT_LOW_PART:
      print_value (t, XEXP (x, 0), verbose);
      cur = safe_concat (buf, cur, "strict_low_part(");
      cur = safe_concat (buf, cur, t);
      cur = safe_concat (buf, cur, ")");
      break;
    case SCRATCH:
      cur = safe_concat (buf, cur, "scratch");
      break;
    case CC0:
      cur = safe_concat (buf, cur, "cc0");
      break;
    case PC:
      cur = safe_concat (buf, cur, "pc");
      break;
    case MEM:
      print_value (t, XEXP (x, 0), verbose);
      cur = safe_concat (buf, cur, "[");
      cur = safe_concat (buf, cur, t);
      cur = safe_concat (buf, cur, "]");
      break;
    case DEBUG_EXPR:
      sprintf (t, "D#%i", DEBUG_TEMP_UID (DEBUG_EXPR_TREE_DECL (x)));
      cur = safe_concat (buf, cur, t);
      break;
    default:
      print_exp (t, x, verbose);
      cur = safe_concat (buf, cur, t);
      break;
    }
}				/* print_value */

/* Print X, an RTL value node, to file F in slim format.  Include
   additional information if VERBOSE is nonzero.

   Value nodes are constants, registers, labels, symbols and
   memory.  */

void
dump_value_slim (FILE *f, const_rtx x, int verbose)
{
  char buf[BUF_LEN];

  print_value (buf, x, verbose);
  fprintf (f, "%s", buf);
}

/* The next step in insn detalization, its pattern recognition.  */

void
print_pattern (char *buf, const_rtx x, int verbose)
{
  char t1[BUF_LEN], t2[BUF_LEN], t3[BUF_LEN];

  if (! x)
    {
      sprintf (buf, "(nil)");
      return;
    }

  switch (GET_CODE (x))
    {
    case SET:
      print_value (t1, SET_DEST (x), verbose);
      print_value (t2, SET_SRC (x), verbose);
      sprintf (buf, "%s=%s", t1, t2);
      break;
    case RETURN:
    case SIMPLE_RETURN:
    case EH_RETURN:
      sprintf (buf, GET_RTX_NAME (GET_CODE (x)));
      break;
    case CALL:
      print_exp (buf, x, verbose);
      break;
    case CLOBBER:
    case USE:
      print_value (t1, XEXP (x, 0), verbose);
      sprintf (buf, "%s %s", GET_RTX_NAME (GET_CODE (x)), t1);
      break;
    case VAR_LOCATION:
      print_value (t1, PAT_VAR_LOCATION_LOC (x), verbose);
      sprintf (buf, "loc %s", t1);
      break;
    case COND_EXEC:
      if (GET_CODE (COND_EXEC_TEST (x)) == NE
	  && XEXP (COND_EXEC_TEST (x), 1) == const0_rtx)
	print_value (t1, XEXP (COND_EXEC_TEST (x), 0), verbose);
      else if (GET_CODE (COND_EXEC_TEST (x)) == EQ
	       && XEXP (COND_EXEC_TEST (x), 1) == const0_rtx)
	{
	  t1[0] = '!';
	  print_value (t1 + 1, XEXP (COND_EXEC_TEST (x), 0), verbose);
	}
      else
	print_value (t1, COND_EXEC_TEST (x), verbose);
      print_pattern (t2, COND_EXEC_CODE (x), verbose);
      sprintf (buf, "(%s) %s", t1, t2);
      break;
    case PARALLEL:
      {
	int i;

	sprintf (t1, "{");
	for (i = 0; i < XVECLEN (x, 0); i++)
	  {
	    print_pattern (t2, XVECEXP (x, 0, i), verbose);
	    sprintf (t3, "%s%s;", t1, t2);
	    strcpy (t1, t3);
	  }
	sprintf (buf, "%s}", t1);
      }
      break;
    case SEQUENCE:
      {
	int i;

	sprintf (t1, "sequence{");
	for (i = 0; i < XVECLEN (x, 0); i++)
	  {
	    print_pattern (t2, XVECEXP (x, 0, i), verbose);
	    sprintf (t3, "%s%s;", t1, t2);
	    strcpy (t1, t3);
	  }
	sprintf (buf, "%s}", t1);
      }
      break;
    case ASM_INPUT:
      sprintf (buf, "asm {%s}", XSTR (x, 0));
      break;
    case ADDR_VEC:
      /* Fall through.  */
    case ADDR_DIFF_VEC:
      print_value (buf, XEXP (x, 0), verbose);
      break;
    case TRAP_IF:
      print_value (t1, TRAP_CONDITION (x), verbose);
      sprintf (buf, "trap_if %s", t1);
      break;
    case UNSPEC:
    case UNSPEC_VOLATILE:
      /* Fallthru -- leave UNSPECs to print_exp.  */
    default:
      print_value (buf, x, verbose);
    }
}				/* print_pattern */

/* This is the main function in slim rtl visualization mechanism.

   X is an insn, to be printed into BUF.

   This function tries to print it properly in human-readable form,
   resembling assembler mnemonics (instead of the older Lisp-style
   form).

   If VERBOSE is TRUE, insns are printed with more complete (but
   longer) pattern names and with extra information, and prefixed
   with their INSN_UIDs.  */

void
print_insn (char *buf, const_rtx x, int verbose)
{
  /* Collect the string to output for X in t1.  t2 is a scratch area.  */
  char t1[BUF_LEN], t2[BUF_LEN];

  switch (GET_CODE (x))
    {
    case INSN:
      print_pattern (t1, PATTERN (x), verbose);
      break;

    case DEBUG_INSN:
      {
	const char *name = "?";

	if (DECL_P (INSN_VAR_LOCATION_DECL (x)))
	  {
	    tree id = DECL_NAME (INSN_VAR_LOCATION_DECL (x));
	    char idbuf[32];
	    if (id)
	      name = IDENTIFIER_POINTER (id);
	    else if (TREE_CODE (INSN_VAR_LOCATION_DECL (x))
		     == DEBUG_EXPR_DECL)
	      {
		sprintf (idbuf, "D#%i",
			 DEBUG_TEMP_UID (INSN_VAR_LOCATION_DECL (x)));
		name = idbuf;
	      }
	    else
	      {
		sprintf (idbuf, "D.%i",
			 DECL_UID (INSN_VAR_LOCATION_DECL (x)));
		name = idbuf;
	      }
	  }
	if (VAR_LOC_UNKNOWN_P (INSN_VAR_LOCATION_LOC (x)))
	  sprintf (t1, "debug %s optimized away", name);
	else
	  {
	    print_pattern (t2, INSN_VAR_LOCATION_LOC (x), verbose);
	    sprintf (t1, "debug %s => %s", name, t2);
	  }
      }
      break;

    case JUMP_INSN:
      print_pattern (t1, PATTERN (x), verbose);
      break;
    case CALL_INSN:
      if (GET_CODE (PATTERN (x)) == PARALLEL)
        print_pattern (t1, XVECEXP (PATTERN (x), 0, 0), verbose);
      else
	print_pattern (t1, PATTERN (x), verbose);
      break;
    case CODE_LABEL:
      sprintf (t1, "L%d:", INSN_UID (x));
      break;
    case BARRIER:
      sprintf (t1, "barrier");
      break;
    case NOTE:
      {
	switch (NOTE_KIND (x))
	  {
	  case NOTE_INSN_EH_REGION_BEG:
	  case NOTE_INSN_EH_REGION_END:
	    sprintf (t2, "%d", NOTE_EH_HANDLER (x));
	    break;

	  case NOTE_INSN_BLOCK_BEG:
	  case NOTE_INSN_BLOCK_END:
	    sprintf (t2, "%d", BLOCK_NUMBER (NOTE_BLOCK (x)));
	    break;

	  case NOTE_INSN_BASIC_BLOCK:
	    sprintf (t2, "%d", NOTE_BASIC_BLOCK (x)->index);
	    break;

	  case NOTE_INSN_DELETED_LABEL:
	  case NOTE_INSN_DELETED_DEBUG_LABEL:
	    {
	      const char *label = NOTE_DELETED_LABEL_NAME (x);
	      if (label == NULL)
		label = "";
	      sprintf (t2, "(\"%s\")", label);
	    }
	    break;

	  case NOTE_INSN_VAR_LOCATION:
	  case NOTE_INSN_CALL_ARG_LOCATION:
	    /* It's safe here to use t1 for scratch because the output
	       is printed in t2 and put back in t1 at the bottom of
	       the inner switch statement.  */
	    print_pattern (t1, NOTE_VAR_LOCATION (x), verbose);
	    sprintf (t2, "{%s}", t1);
	    break;

	  default:
	    t2[0] = '\0';
	    break;
	  }
	sprintf (t1, "%s %s", GET_NOTE_INSN_NAME (NOTE_KIND (x)), t2);
	break;
      }
    default:
      sprintf (t1, "<What %s?>", GET_RTX_NAME (GET_CODE (x)));
      break;
    }

  if (verbose)
    sprintf (buf, " %4d: %s", INSN_UID (x), t1);
  else
    sprintf (buf, "%s", t1);
}				/* print_insn */

/* Emit a slim dump of X (an insn) to the file F, including any register
   note attached to the instruction.  */
void
dump_insn_slim (FILE *f, const_rtx x)
{
  char t[BUF_LEN + 32];
  rtx note;

  print_insn (t, x, 1);
  fputs (print_rtx_head, f);
  fputs (t, f);
  putc ('\n', f);
  if (INSN_P (x) && REG_NOTES (x))
    for (note = REG_NOTES (x); note; note = XEXP (note, 1))
      {
	fputs (print_rtx_head, f);
        print_pattern (t, XEXP (note, 0), 1);
	fprintf (f, "      %s: %s\n",
		 GET_REG_NOTE_NAME (REG_NOTE_KIND (note)), t);
      }
}

/* Same as above, but stop at LAST or when COUNT == 0.
   If COUNT < 0 it will stop only at LAST or NULL rtx.  */

void
dump_rtl_slim (FILE *f, const_rtx first, const_rtx last,
	       int count, int flags ATTRIBUTE_UNUSED)
{
  const_rtx insn, tail;

  tail = last ? NEXT_INSN (last) : NULL_RTX;
  for (insn = first;
       (insn != NULL) && (insn != tail) && (count != 0);
       insn = NEXT_INSN (insn))
    {
      dump_insn_slim (f, insn);
      if (count > 0)
        count--;
    }
}

/* Emit a slim dump of X (an insn) to stderr.  */
extern void debug_insn_slim (const_rtx);
DEBUG_FUNCTION void
debug_insn_slim (const_rtx x)
{
  dump_insn_slim (stderr, x);
}

/* Same as above, but using dump_rtl_slim.  */
extern void debug_rtl_slim (FILE *, const_rtx, const_rtx, int, int);
DEBUG_FUNCTION void
debug_rtl_slim (const_rtx first, const_rtx last, int count, int flags)
{
  dump_rtl_slim (stderr, first, last, count, flags);
}

extern void debug_bb_slim (basic_block);
DEBUG_FUNCTION void
debug_bb_slim (basic_block bb)
{
  dump_bb (stderr, bb, 0, TDF_SLIM | TDF_BLOCKS);
}

extern void debug_bb_n_slim (int);
DEBUG_FUNCTION void
debug_bb_n_slim (int n)
{
  basic_block bb = BASIC_BLOCK (n);
  debug_bb_slim (bb);
}

