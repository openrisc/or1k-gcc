/* Subroutines for insn-output.c for GNU compiler.  OpenRISC 1000 version.
   Copyright (C) 1987, 1992, 1997, 1999, 2000, 2001, 2002, 2003, 2004,
   2005, 2006, 2007, 2008, 2009, 2010, 2011  Free Software Foundation, Inc
   Copyright (C) 2010 Embecosm Limited

   Contributed by Damjan Lampret <damjanl@bsemi.com> in 1999.
   Major optimizations by Matjaz Breskvar <matjazb@bsemi.com> in 2005.
   Updated for GCC 4.5 by Jeremy Bennett <jeremy.bennett@embecoms.com>
   and Joern Rennecke <joern.rennecke@embecosm.com> in 2010.

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
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "stor-layout.h"
#include "memmodel.h"
#include "gimple.h"
#include "tm_p.h"
#include "varasm.h"
#include "obstack.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "reload.h"
#include "function.h"
#include "explow.h"
#include "emit-rtl.h"
#include "expr.h"
#include "toplev.h"
#include "recog.h"
#include "ggc.h"
#include "except.h"
#include "calls.h"
#include "target-def.h"
#include "debug.h"
#include "langhooks.h"
#include "predict.h"
#include "basic-block.h"
#include "df.h"
#include "optabs.h"
#include "dwarf2.h"
#include "ansidecl.h"
#include "builtins.h"
#include "sbitmap.h"
#include "tm-constrs.h"

/* ========================================================================== */
/* Local macros                                                               */

/* Construct a l.movhi instruction for the given reg and value */
#define OR1K_MOVHI(rd, k)						\
  ((0x6 << 26) | ((rd) << 21) | (k))

/* Construct a l.ori instruction for the given two regs and value */
#define OR1K_ORI(rd, ra, k)						\
  ((0x2a << 26) | ((rd) << 21) | ((ra) << 16) | (k))

/* Construct a l.lwz instruction for the given two registers and offset */
#define OR1K_LWZ(rd, ra, i)						\
  ((0x21 << 26) | ((rd) << 21) | ((ra) << 16) | (i))

/* Construct a l.jr instruction for the given register */
#define OR1K_JR(rb)							\
  ((0x11 << 26) | ((rb) << 11))

#define OR1K_NOP                                \
  (0x15 << 24)

/* ========================================================================== */
/* Static variables (i.e. global to this file only.                           */


/* Stack layout we use for the function, as documented in
   the Architecture Manual's ABI section:

   Incoming arguments
                      <- AP, HFP
   Saved LR           \           \
   Saved HFP          | save_size |
   Other saved regs   /           |
                      <- FP       | total_size
   Local Stack frame              |
   Outgoing arguments             /
                      <- SP

   Note that there is also (by default) 128 bytes of "red zone"
   which can be used by leaf functions.  In that case, save_size
   may be non-zero while total_size is zero, and the registers
   will be saved below SP.
*/

static struct
{
  HOST_WIDE_INT total_size;
  int save_size;
  unsigned int save_mask;
} frame_info;

/* Return true if REGNO must be saved for the current function.  */

static bool
or1k_save_reg_p (unsigned int regno)
{
  /* Check call-saved registers.  */
  if (!call_used_regs[regno] && df_regs_ever_live_p (regno))
    return true;

  switch (regno)
    {
    case HARD_FRAME_POINTER_REGNUM:
      return frame_pointer_needed;

    case LINK_REGNUM:
      if (!crtl->is_leaf
	  || cfun->machine->force_lr_save
	  || df_regs_ever_live_p (regno))
	return true;
      /* FALLTHRU -- setting up PIC requires LR clobber.  */

    case PIC_OFFSET_TABLE_REGNUM:
      return crtl->uses_pic_offset_table;

    case 25: case 27: case 29: case 31:
      /* See EH_RETURN_DATA_REGNO.  */
      return crtl->calls_eh_return;

    default:
      return false;
    }
}

/* Compute the stack layout for the current function, filling in
   FRAME_INFO.  Return the total size of the stack frame.  */

static HOST_WIDE_INT
or1k_compute_frame_size ()
{
  HOST_WIDE_INT vars_size, args_size, total_size;
  unsigned int save_mask = 0;
  int save_size = 0;

  for (int regno = 0; regno <= OR1K_LAST_ACTUAL_REG; regno++)
    if (or1k_save_reg_p (regno))
      {
	save_size += UNITS_PER_WORD;
	save_mask |= 1U << regno;
      }

  vars_size = get_frame_size ();
  vars_size = OR1K_ALIGN (vars_size, UNITS_PER_WORD);
  args_size = crtl->outgoing_args_size;
  total_size = vars_size + save_size + args_size;
  if (crtl->is_leaf && !cfun->calls_alloca)
    {
      if (total_size > or1k_redzone)
	total_size -= or1k_redzone;
      else
	total_size = 0;
    }
  frame_info.total_size = total_size;
  frame_info.save_size = save_size;
  frame_info.save_mask = save_mask;

  return total_size;
}

/* Eliminate AP and FP by returning the displacement to HFP or SP.  */

HOST_WIDE_INT
or1k_initial_elimination_offset (unsigned int from, unsigned int to)
{
  HOST_WIDE_INT offset = 0;
  or1k_compute_frame_size ();

  /* For FP, move up (negative) to the top of the frame.  */
  if (from == FRAME_POINTER_REGNUM)
    offset = -frame_info.save_size;
  else
    gcc_assert (from == ARG_POINTER_REGNUM);

  /* For SP, move down (positive) to the bottom of the frame.  */
  if (to == STACK_POINTER_REGNUM)
    offset += frame_info.total_size;
  else
    gcc_assert (to == HARD_FRAME_POINTER_REGNUM);

  return offset;
}

void
or1k_expand_compare (rtx *operands)
{
  rtx sr_f = gen_rtx_REG (BImode, SR_F_REG);

  /* Emit the given comparison into the Flag bit.  */
  PUT_MODE (operands[0], BImode);
  emit_insn (gen_rtx_SET (sr_f, operands[0]));

  /* Adjust the operands for use in the caller.  */
  operands[0] = gen_rtx_NE (VOIDmode, sr_f, const0_rtx);
  operands[1] = sr_f;
  operands[2] = const0_rtx;
}

/* TODO(bluecmd): Write documentation for this function */
void
or1k_expand_cmpxchg_qihi (rtx bval, rtx retval, rtx mem, rtx oldval, rtx newval,
                          int  __attribute__ ((unused)) is_weak,
                          enum memmodel __attribute__ ((unused)) success_mode,
                          enum memmodel __attribute__ ((unused)) failure_mode)
{
  rtx addr1 = force_reg (Pmode, XEXP (mem, 0));
  rtx addr = gen_reg_rtx (Pmode);
  rtx off = gen_reg_rtx (SImode);
  rtx shifter = gen_reg_rtx (SImode);
  rtx retword = gen_reg_rtx (SImode);
  rtx mask = gen_reg_rtx (SImode);
  rtx shifted_oldval = gen_reg_rtx (SImode);
  rtx shifted_newval = gen_reg_rtx (SImode);
  rtx shifted_mask = gen_reg_rtx (SImode);
  rtx mask_const;
  rtx memsi;
  enum machine_mode mode = GET_MODE (mem);

  oldval = gen_lowpart_common (SImode, oldval);
  newval = gen_lowpart_common (SImode, newval);

  mask_const = gen_rtx_CONST_INT (VOIDmode,
                                  mode == QImode ? 0xff : 0xffff);
  emit_insn (gen_rtx_SET (mask, mask_const));

  /* align address and retrieve the offset. */
  emit_insn (gen_rtx_SET (addr,
			  gen_rtx_AND (Pmode, addr1, GEN_INT (-4))));
  emit_insn (gen_rtx_SET (off,
			  gen_rtx_AND (SImode, addr1, GEN_INT (3))));
  emit_insn (gen_rtx_SET (off,
                          gen_rtx_XOR (SImode, off,
                                       GEN_INT (GET_MODE (mem) == QImode
                                                ? 3 : 2))));

  memsi = gen_rtx_MEM (SImode, addr);

  /* shift all arguments to be aligned to where the data we want
   * to operate on is located. */
  emit_insn (gen_rtx_SET (shifter,
			  gen_rtx_ASHIFT (SImode, off, GEN_INT (3))));

  emit_insn (gen_ashlsi3 (shifted_oldval, oldval, shifter));
  emit_insn (gen_ashlsi3 (shifted_newval, newval, shifter));
  emit_insn (gen_ashlsi3 (shifted_mask, mask, shifter));

  emit_insn (gen_cmpxchg_mask (bval, retword, memsi, shifted_oldval,
                               shifted_newval, shifted_mask));

  /* shift the data we care about to the lower end. */
  emit_insn (gen_lshrsi3 (retword, retword, shifter));

  emit_move_insn (retval, gen_lowpart (GET_MODE (retval), retword));
}

/* TODO(bluecmd): Write documentation for this function */
void
or1k_expand_fetch_op_qihi (rtx oldval, rtx mem, rtx operand, rtx newval,
                           rtx (*generator)(rtx, rtx, rtx, rtx, rtx))
{
  rtx addr1 = force_reg (Pmode, XEXP (mem, 0));
  rtx addr = gen_reg_rtx (Pmode);
  rtx off = gen_reg_rtx (SImode);
  rtx shifter = gen_reg_rtx (SImode);
  rtx mask = gen_reg_rtx (SImode);
  rtx shifted_oldval = gen_reg_rtx (SImode);
  rtx shifted_newval = gen_reg_rtx (SImode);
  rtx shifted_operand = gen_reg_rtx (SImode);
  rtx shifted_mask = gen_reg_rtx (SImode);
  rtx mask_const;
  rtx memsi;
  enum machine_mode mode = GET_MODE (mem);

  /* TODO(bluecmd): A lot of code is shared between cmpxchg and this. We should
   * move it to nice functions. */
  operand = gen_lowpart_common (SImode, operand);

  mask_const = GEN_INT (mode == QImode ? 0xff : 0xffff);
  emit_insn (gen_rtx_SET (mask, mask_const));

  /* align address and retrieve the offset. */
  emit_insn (gen_rtx_SET (addr,
			  gen_rtx_AND (Pmode, addr1, GEN_INT (-4))));
  emit_insn (gen_rtx_SET (off,
			  gen_rtx_AND (SImode, addr1, GEN_INT (3))));
  emit_insn (gen_rtx_SET (off,
                          gen_rtx_XOR (SImode, off,
                                       GEN_INT (GET_MODE (mem) == QImode
                                                ? 3 : 2))));

  memsi = gen_rtx_MEM (SImode, addr);

  /* shift all arguments to be aligned to where the data we want
   * to operate on is located. */
  emit_insn (gen_rtx_SET (shifter,
			  gen_rtx_ASHIFT (SImode, off, GEN_INT (3))));

  emit_insn (gen_ashlsi3 (shifted_operand, operand, shifter));
  emit_insn (gen_ashlsi3 (shifted_mask, mask, shifter));

  emit_insn (generator (shifted_oldval, memsi, shifted_operand,
                        shifted_newval, shifted_mask));

  /* shift the data we care about to the lower end. */
  emit_insn (gen_lshrsi3 (shifted_oldval, shifted_oldval, shifter));
  emit_insn (gen_lshrsi3 (shifted_newval, shifted_newval, shifter));
  emit_move_insn (oldval, gen_lowpart (GET_MODE (oldval), shifted_oldval));
  emit_move_insn (newval, gen_lowpart (GET_MODE (newval), shifted_newval));
}

/* Implement the TARGET_PRINT_OPERAND_PUNCT_VALID_P hook.  */

static bool
or1k_print_operand_punct_valid_p (unsigned char code)
{
  return code == '(';
}

/* Prettify the assembly.  Indicate an instruction is filling a delay slot
   by indenting it one space.  */

static bool or1k_indent_opcode;

void
or1k_output_opcode (FILE *stream)
{
  if (or1k_indent_opcode)
    {
      putc (' ', stream);
      or1k_indent_opcode = false;
    }
}

/* Implement the TARGET_PRINT_OPERAND hook.  */
static void
or1k_print_operand (FILE *stream, rtx x, int code)
{
  switch (code)
    {
    case 'r':
      if (REG_P (x))
	fputs (reg_names[REGNO (x)], stream);
      else if (x == CONST0_RTX (GET_MODE (x)))
	fputs ("r0", stream);
      else
	output_operand_lossage ("invalid %%r value");
      break;

    case 'H':
      if (REG_P (x))
	fputs (reg_names[REGNO (x) + 1], stream);
      else
	output_operand_lossage ("invalid %%H value");
      break;

    case 'C':
      switch (GET_CODE (x))
	{
        case EQ:
	  fputs ("eq", stream);
	  break;
	case NE:
	  fputs ("ne", stream);
	  break;
	case GT:
	  fputs ("gts", stream);
	  break;
        case GE:
	  fputs ("ges", stream);
	  break;
	case LT:
	  fputs ("lts", stream);
	  break;
	case LE:
	  fputs ("les", stream);
	  break;
	case GTU:
	  fputs ("gtu", stream);
	  break;
	case GEU:
	  fputs ("geu", stream);
	  break;
	case LTU:
	  fputs ("ltu", stream);
	  break;
	case LEU:
	  fputs ("leu", stream);
	  break;
	default:
	  output_operand_lossage ("invalid %%C value");
	}
      break;

    case '(':
      /* Output a 'nop' if there is nothing for the delay slot.  Or remember
	 to add a space at the beginning of the next insn to indicate that
	 it is filling the delay slot.  */
      if (TARGET_DELAY_ON && dbr_sequence_length ())
	or1k_indent_opcode = true;
      else if (!TARGET_DELAY_OFF)
	fprintf (stream, "\n\t l.nop");
      break;

    case 0:
    case 'S': /* ??? calls */
      if (REG_P (x))
	fputs (reg_names[REGNO (x)], stream);
      else if (MEM_P (x))
	output_address (GET_MODE(x), XEXP (x, 0));
      else
	output_addr_const (stream, x);
      break;

    default:
      output_operand_lossage ("invalid %%xn code");
    }
}

static void
or1k_print_operand_address (FILE *stream, machine_mode mode, rtx addr)
{
  rtx offset;

  switch (GET_CODE (addr))
    {
    case MEM:
      if (GET_CODE (XEXP (addr, 0)) == REG)
        fprintf (stream, "%s", reg_names[REGNO (addr)]);
      else
        abort ();
      break;

    case REG:
      fprintf (stream, "0(%s)", reg_names[REGNO (addr)]);
      break;

    case PLUS:
      offset = 0;

      if (GET_CODE (XEXP (addr, 0)) == REG)
        {
          offset = XEXP (addr, 1);
          addr   = XEXP (addr, 0);
        }
      else if (GET_CODE (XEXP (addr, 1)) == REG)
        {
          offset = XEXP (addr, 0);
          addr   = XEXP (addr, 1);
        }
      output_address (mode, offset);
      fprintf (stream, "(%s)", reg_names[REGNO (addr)]);
      break;

    case SYMBOL_REF:
      if (SYMBOL_REF_DECL (addr))
        assemble_external (SYMBOL_REF_DECL (addr));

      if (XSTR (addr, 0)[0] == '*')
        fputs (&XSTR (addr, 0)[1], stream);
      else
        {
          asm_fprintf (stream, "%U%s", XSTR (addr, 0));
        }
      break;

    default:
      output_addr_const (stream, addr);
    }
}

/* -------------------------------------------------------------------------- */
/*!Can this register be used as a base register?

   We need a strict version, for which the register must either be a hard
   register, or already renumbered to a hard register.

   For the non-strict version, any register (other than the flag register will
   do).

   @todo The code from the old port does not allow r0 as a base when strict,
         and does when non-strict. Surely it is always a valid register?

   @param[in] regno   The register to test
   @param[in] strict  Non-zero (TRUE) if this is a strict check, zero (FALSE)
                      otherwise.

   @return  Non-zero (TRUE) if this register can be used as a base register,
            zero (FALSE) otherwise.                                           */
/* -------------------------------------------------------------------------- */
static bool
or1k_regnum_ok_for_base_p (HOST_WIDE_INT  num,
			   bool           strict)
{
  if (strict)
    {
      return (num < FIRST_PSEUDO_REGISTER)
	? (num > 0) && (num <= OR1K_LAST_INT_REG)
	: (reg_renumber[num] > 0) && (reg_renumber[num] <= OR1K_LAST_INT_REG);
    }
  else
    {
      return (num <= OR1K_LAST_INT_REG) || (num >= FIRST_PSEUDO_REGISTER);
    }
}	/* or1k_regnum_ok_for_base_p () */

int
or1k_legitimate_pic_operand_p (rtx x)
{
  if (GET_CODE (x) == CONST
      && GET_CODE (XEXP (x, 0)) == PLUS
      && ((GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
	   && (!SYMBOL_REF_LOCAL_P (XEXP (XEXP (x, 0), 0))
	       || SYMBOL_REF_WEAK (XEXP (XEXP (x, 0), 0))))
	  || GET_CODE (XEXP (XEXP (x, 0), 0)) == LABEL_REF)
      && CONST_INT_P (XEXP (XEXP (x, 0), 1)))
    return satisfies_constraint_I (XEXP (XEXP (x, 0), 1));

  return 1;
}

static bool
or1k_expand_pic_symbol_ref (enum machine_mode mode ATTRIBUTE_UNUSED,
			    rtx operands[])
{
  if (GET_CODE (operands[1]) == LABEL_REF
      || (GET_CODE (operands[1]) == SYMBOL_REF
	  && SYMBOL_REF_LOCAL_P (operands[1])
	  && !SYMBOL_REF_WEAK (operands[1])))
    {
      crtl->uses_pic_offset_table = 1;
      emit_insn (gen_movsi_gotoffhi (operands[0], operands[1]));
      emit_insn (gen_movsi_gotofflo (operands[0], operands[0],
				     operands[1]));
      emit_insn (gen_add3_insn(operands[0], operands[0],
			       pic_offset_table_rtx));
      return true;
    }
  else if (GET_CODE (operands[1]) == SYMBOL_REF)
    {
      crtl->uses_pic_offset_table = 1;
      emit_insn (gen_movsi_got (operands[0], operands[1]));
      return true;
    }
  else if (GET_CODE (operands[1]) == CONST
	   && GET_CODE (XEXP (operands[1], 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (operands[1], 0), 0)) == SYMBOL_REF
	   && GET_CODE (XEXP (XEXP (operands[1], 0), 1)) == CONST_INT)
    {
      rtx symbolref = XEXP (XEXP (operands[1], 0), 0);
      crtl->uses_pic_offset_table = 1;

      if (SYMBOL_REF_LOCAL_P (symbolref)
	  && !SYMBOL_REF_WEAK (symbolref))
	{
	  emit_insn (gen_movsi_gotoffhi (operands[0], operands[1]));
	  emit_insn (gen_movsi_gotofflo (operands[0], operands[0],
					 operands[1]));
	  emit_insn (gen_add3_insn(operands[0], operands[0],
				   pic_offset_table_rtx));
	}
      else
	{
	  rtx const_int = XEXP (XEXP (operands[1], 0), 1);

	  /* Expand the constant into a register if it doesn't
	     fit directly as an 16-bit immediate in the add below.
	     Note that the reg allocation is allowed here since
	     we are guarded by LEGITIMATE_PIC_OPERAND_P. */
	  if (!satisfies_constraint_I (const_int))
	    {
	      rtx scratch = gen_reg_rtx (mode);

	      or1k_emit_set_const32 (scratch, const_int);
	      const_int = scratch;
	    }

	  emit_insn (gen_movsi_got (operands[0], symbolref));
	  emit_insn (gen_add3_insn(operands[0], operands[0], const_int));
	}
      return true;
    }
  return false;
}

/* Return the TLS type for TLS symbols, 0 otherwise.  */
enum tls_model
or1k_tls_symbolic_operand (rtx op)
{
  if (GET_CODE (op) == CONST)
    {
      rtx sym, addend;
      split_const (op, &sym, &addend);
      if (GET_CODE (sym) == SYMBOL_REF)
	return SYMBOL_REF_TLS_MODEL (sym);
    }
  else if (GET_CODE (op) == SYMBOL_REF)
    return SYMBOL_REF_TLS_MODEL (op);

  return TLS_MODEL_NONE;
}

static GTY(()) rtx gen_tls_tga;

/* Get reference to the '__tls_get_addr' symbol */
static rtx
gen_tls_get_addr (void)
{
  if (!gen_tls_tga)
    gen_tls_tga = init_one_libfunc ("__tls_get_addr");
  return gen_tls_tga;
}

/* Emit call to '__tls_get_addr' */
static void
or1k_tls_call (rtx dest, rtx arg)
{
  emit_library_call_value (gen_tls_get_addr(), dest,
      LCT_CONST, Pmode, 1, arg, Pmode);
}

static rtx
or1k_legitimize_tls_address (rtx dest, rtx x)
{
  rtx sym;
  rtx tp = gen_rtx_REG(Pmode, THREAD_PTR_REGNUM);
  rtx addend = NULL_RTX;
  rtx result = dest;

  enum tls_model tls_kind = or1k_tls_symbolic_operand (x);

  if (GET_CODE (x) == SYMBOL_REF)
    sym = gen_rtx_SYMBOL_REF(Pmode, XSTR(x, 0));
  else if (GET_CODE (x) == CONST)
    {
      result = gen_reg_rtx (Pmode);
      split_const (x, &sym, &addend);
      sym = gen_rtx_SYMBOL_REF(Pmode, XSTR(sym, 0));
    }
  else
    gcc_unreachable ();

  switch (tls_kind) {
    case TLS_MODEL_GLOBAL_DYNAMIC:
    case TLS_MODEL_LOCAL_DYNAMIC:
      {
        /* TODO: For now, treat LD as GD */
        rtx hi = gen_reg_rtx (Pmode);
        rtx offset = gen_reg_rtx (Pmode);
        rtx addr = gen_reg_rtx (Pmode);
        crtl->uses_pic_offset_table = 1;
        /* Generate a new symbol ref that is not marked as TLS or we will recurse
         * in or1k_legitimate_constant_p. */
        emit_insn (gen_movsi_tlsgdhi (hi, sym));
        emit_insn (gen_movsi_tlsgdlo (offset, hi, sym));
        emit_insn (gen_add3_insn (addr, offset, pic_offset_table_rtx));
        or1k_tls_call (result, addr);
        break;
      }
    case TLS_MODEL_INITIAL_EXEC:
      {
        rtx hi = gen_reg_rtx (Pmode);
        rtx offset = gen_reg_rtx (Pmode);
        rtx addr = gen_reg_rtx (Pmode);
        rtx tpoffset = gen_reg_rtx (Pmode);
        crtl->uses_pic_offset_table = 1;
        emit_insn (gen_movsi_gottpoffhi (hi, sym));
        emit_insn (gen_movsi_gottpofflo (offset, hi, sym));
        emit_insn (gen_add3_insn (addr, offset, pic_offset_table_rtx));
        emit_insn (gen_load_gottpoff (tpoffset, addr));
        emit_insn (gen_add3_insn (result, tpoffset, tp));
        break;
      }
    case TLS_MODEL_LOCAL_EXEC:
      {
        rtx hi = gen_reg_rtx (Pmode);
        rtx addr = gen_reg_rtx (Pmode);
        emit_insn (gen_movsi_tpoffhi (hi, sym));
        emit_insn (gen_movsi_tpofflo (addr, hi, sym));
        emit_insn (gen_add3_insn (result, addr, tp));
        break;
      }
    default:
      gcc_unreachable ();
  }

  if (addend != NULL_RTX)
    emit_insn (gen_add3_insn (dest, result, addend));

  return dest;
}

static rtx
or1k_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
                         enum machine_mode mode ATTRIBUTE_UNUSED)
{
  if (or1k_tls_symbolic_operand (x) != TLS_MODEL_NONE)
    return or1k_legitimize_tls_address (gen_reg_rtx (Pmode), x);

  return x;
}

static bool
or1k_cannot_force_const_mem (enum machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  return or1k_tls_symbolic_operand (x) != TLS_MODEL_NONE;
}

bool
or1k_expand_symbol_ref(enum machine_mode mode, rtx operands[])
{
  if (flag_pic && or1k_expand_pic_symbol_ref(mode, operands))
    return true;

  return false;
}

bool
or1k_expand_move (enum machine_mode mode, rtx operands[])
{
  if (can_create_pseudo_p ())
    {
      if (GET_CODE (operands[0]) == MEM
	  || (GET_CODE (operands[0]) == SUBREG
	      && GET_CODE (SUBREG_REG (operands[0])) == MEM))
        {
          /* Source operand for store must be in a register.  */
          operands[1] = force_reg (SImode, operands[1]);
        }
    }

  if (or1k_tls_symbolic_operand (operands[1]) != TLS_MODEL_NONE)
    {
      or1k_legitimize_tls_address (force_reg (Pmode, operands[0]),
          operands[1]);
      return true;
    }

  if (or1k_expand_symbol_ref (mode, operands))
    return true;

  /* Working with CONST_INTs is easier, so convert
     a double if needed.  */

  if (GET_CODE (operands[1]) == CONST_DOUBLE) {
    operands[1] = GEN_INT (CONST_DOUBLE_LOW (operands[1]));
  }

  /* Handle sets of MEM first.  */
  if (GET_CODE (operands[0]) == MEM)
    {
      if (register_operand(operands[1], SImode)
          || (operands[1] == const0_rtx))
        goto movsi_is_ok;

      if (! reload_in_progress)
        {
          operands[0] = validize_mem (operands[0]);
          operands[1] = force_reg (SImode, operands[1]);
        }
    }

  /* This makes sure we will not get rematched due to splittage.  */
  if (! CONSTANT_P (operands[1]) || input_operand (operands[1], SImode))
    ;
  else if (CONSTANT_P (operands[1])
           && GET_CODE (operands[1]) != HIGH
           && GET_CODE (operands[1]) != LO_SUM)
    {
      or1k_emit_set_const32 (operands[0], operands[1]);
      return true;
    }
 movsi_is_ok:
  ;

  return false;
}

/* -------------------------------------------------------------------------- */
/*!Emit a move from SRC to DEST.

   Assume that the move expanders can handle all moves if !can_create_pseudo_p
   ().  The distinction is important because, unlike emit_move_insn, the move
   expanders know how to force Pmode objects into the constant pool even when
   the constant pool address is not itself legitimate.

   @param[in] dest  Destination of the move.
   @param[in] src   Source for the move.

   @return  RTX for the move.                                                 */
/* -------------------------------------------------------------------------- */
static rtx
or1k_emit_move (rtx dest, rtx src)
{
  return (can_create_pseudo_p ()
	  ? emit_move_insn (dest, src)
	  : emit_move_insn_1 (dest, src));

}	/* or1k_emit_move () */


/* -------------------------------------------------------------------------- */
/*!Emit an instruction of the form (set TARGET (CODE OP0 OP1)).

   @param[in] code    The code for the operation.
   @param[in] target  Destination for the set operation.
   @param[in] op0     First operand.
   @param[in] op1     Second operand.                                         */
/* -------------------------------------------------------------------------- */
static void
or1k_emit_binary (enum rtx_code  code,
		  rtx            target,
		  rtx            op0,
		  rtx            op1)
{
  emit_insn (gen_rtx_SET (target,
			  gen_rtx_fmt_ee (code, GET_MODE (target), op0, op1)));

}	/* or1k_emit_binary () */


/* -------------------------------------------------------------------------- */
/*!Compute the result of an operation into a new register.

   Compute ("code" "op0" "op1") and store the result in a new register of mode
   "mode".

   @param[in] mode  Mode of the result
   @parma[in] code  RTX for the operation to perform
   @param[in] op0   RTX for the first operand
   @param[in] op1   RTX for the second operand
   
   @return  The RTX for the new register.                                     */
/* -------------------------------------------------------------------------- */
static rtx
or1k_force_binary (enum machine_mode  mode,
		   enum rtx_code      code,
		   rtx                op0,
		   rtx                op1)
{
  rtx  reg;

  reg = gen_reg_rtx (mode);
  or1k_emit_binary (code, reg, op0, op1);

  return reg;

}	/* or1k_force_binary () */


/* ========================================================================== */
/* Global support functions                                                   */

static int
or1k_trampoline_code_words (void)
{
  int words = 5;

  /* need one more word in TARGET_DELAY_COMPAT mode to hold l.nop in delay slot */
  if (TARGET_DELAY_COMPAT)
    words++;
  
  return words;
}

/* -------------------------------------------------------------------------- */
/* Return the size in bytes of the trampoline code.

   Padded to TRAMPOLINE_ALIGNMENT bits. The code sequence is documented in
   or1k_trampoline_init ().

   This is just the code size. the static chain pointer and target function
   address immediately follow.

   @return  The size of the trampoline code in bytes.                         */
/* -------------------------------------------------------------------------- */
int
or1k_trampoline_code_size (void)
{
  const int  TRAMP_BYTE_ALIGN = TRAMPOLINE_ALIGNMENT / 8;

  return (or1k_trampoline_code_words() * 4 + TRAMP_BYTE_ALIGN - 1) / TRAMP_BYTE_ALIGN * TRAMP_BYTE_ALIGN;

}	/* or1k_trampoline_code_size () */


/* ========================================================================== */
/* Functions to support the Machine Description                               */

/* Create a mem in the "frame" alias set at BASE+OFF.  */
static rtx
stack_disp_mem (rtx base, HOST_WIDE_INT off)
{
  rtx x = plus_constant (Pmode, base, off);
  return gen_frame_mem (word_mode, x);
}

/* Save a register for the prologue, marking it for unwind info.  */
static void
prologue_save_reg (unsigned regno, rtx base, HOST_WIDE_INT off)
{
  rtx mem = stack_disp_mem (base, off);
  rtx reg = gen_rtx_REG (word_mode, regno);
  rtx insn = emit_insn (gen_rtx_SET (mem, reg));
  RTX_FRAME_RELATED_P (insn) = 1;
}

/* Save all required registers for the prologue.
   The stores happen in decending order below BASE+OFF.  */
static void
prologue_save_registers (unsigned int mask, rtx base, HOST_WIDE_INT off)
{
  /* The ABI specifies LR and FP saved immediately below the
     hard frame pointer.  We might as well use the same layout
     even when not using a frame pointer.  */
  if ((mask >> LINK_REGNUM) & 1)
    {
      off -= UNITS_PER_WORD;
      prologue_save_reg (LINK_REGNUM, base, off);
    }
  if ((mask >> HARD_FRAME_POINTER_REGNUM) & 1)
    {
      off -= UNITS_PER_WORD;
      prologue_save_reg (HARD_FRAME_POINTER_REGNUM, base, off);
    }

  for (int regno = 0; regno <= OR1K_LAST_ACTUAL_REG; regno++)
    {
      if (regno == LINK_REGNUM || regno == HARD_FRAME_POINTER_REGNUM)
	continue;
      if ((mask >> regno) & 1)
	{
	  off -= UNITS_PER_WORD;
	  prologue_save_reg (regno, base, off);
	}
    }
}

/* True if the entire local stack frame is in the redzone.  */
static inline bool
or1k_frame_in_redzone (void)
{
  return frame_info.total_size <= or1k_redzone;
}

/* True if the entire register save area is in the redzone.  */
static inline bool
or1k_regs_in_redzone (void)
{
  return frame_info.save_size <= or1k_redzone;
}

/* Adjust the stack by +SIZE.  If MAYBE_FP_PROTECT, then we are allocating
   the bulk of the stack frame and we might need to clobber the local frame.
   If CFA_P, then emit unwind info for the adjustment.  Return the insn
   that makes the adjustment.  */
static rtx
pro_epi_adjust_stack (HOST_WIDE_INT size, bool fp_protect, bool cfa_p)
{
  rtx size_rtx, x, insn;

  if (size == 0)
    return NULL_RTX;

  x = size_rtx = GEN_INT (size);
  if (!satisfies_constraint_I (x))
    {
      rtx r = gen_rtx_REG (Pmode, PROLOGUE_TMP);
      emit_move_insn (r, x);
      x = r;
    }

  if (fp_protect)
    x = gen_frame_alloc (stack_pointer_rtx, stack_pointer_rtx, x);
  else
    x = gen_add2_insn (stack_pointer_rtx, x);
  insn = emit_insn (x);
  if (cfa_p)
    {
      rtx t = gen_rtx_PLUS (Pmode, stack_pointer_rtx, size_rtx);
      t = gen_rtx_SET (stack_pointer_rtx, t);
      REG_NOTES (insn) = alloc_reg_note (REG_CFA_ADJUST_CFA, t, NULL);
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  return insn;
}

/* Return true if we can return directly from the function.  */
bool
or1k_direct_return (void)
{
  return frame_info.total_size == 0 && frame_info.save_mask == 0;
}

/* Expand code to construct the local stack frame.  */
void
or1k_expand_prologue (void)
{
  HOST_WIDE_INT total_size = frame_info.total_size;
  unsigned int save_mask = frame_info.save_mask;
  rtx insn;

  if (total_size == 0 && save_mask == 0)
    return;

  if (or1k_regs_in_redzone ())
    {
      /* When the register save area is in the redzone, we can
	 save the registers before allocating any stack space.
	 This avoids a dependency on the stack register subtraction.  */
      prologue_save_registers (save_mask, stack_pointer_rtx, 0);
      save_mask = 0;

      /* When a frame pointer is needed, we've not yet adjusted
	 the stack pointer, so it's a straght copy.  */
      if (frame_pointer_needed)
	{
	  insn = emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
    }
  else if (frame_pointer_needed || save_mask != 0)
    {
      /* When the register save area exceeds the redzone, we must
	 allcate stack before saving.  In this first step, allocate
	 no more than we can use as a memory offset.  */
      HOST_WIDE_INT step = MIN (32760, total_size);
      pro_epi_adjust_stack (-step, step == total_size, true);
      prologue_save_registers (save_mask, stack_pointer_rtx, step);

      /* When a frame pointer is needed, we must add back what we
	 just subtracted from the stack pointer.  */
      if (frame_pointer_needed)
	{
	  insn = emit_insn (gen_add3_insn (hard_frame_pointer_rtx,
					   stack_pointer_rtx,
					   GEN_INT (step)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      total_size -= step;
    }

  /* Allocate the balance of the stack frame.  */
  pro_epi_adjust_stack (-total_size, frame_pointer_needed,
			!frame_pointer_needed);

  /* Build PIC register, if needed.  */
  if (crtl->uses_pic_offset_table)
    emit_insn (gen_set_got (pic_offset_table_rtx));
}

/* Load a register for the epilogue.  Add the unwind info to *DWARF.  */
static rtx
epilogue_load_reg (unsigned regno, rtx base, HOST_WIDE_INT off, rtx *dwarf)
{
  rtx mem = stack_disp_mem (base, off);
  rtx reg = gen_rtx_REG (word_mode, regno);
  rtx insn = emit_insn (gen_rtx_SET (reg, mem));
  if (dwarf)
    *dwarf = alloc_reg_note (REG_CFA_RESTORE, reg, *dwarf);
  return insn;
}

/* Load all required registers for the epilogue.  The loads happen in
   decending order below BASE+OFF.  Add the unwind info to *DWARF.  */
static rtx
epilogue_load_registers (unsigned int mask, rtx base, HOST_WIDE_INT off,
			 rtx *dwarf)
{
  /* Since OFF will always be aligned, -1 is safe for "unused".  */
  HOST_WIDE_INT fp = -1;

  if ((mask >> LINK_REGNUM) & 1)
    {
      off -= UNITS_PER_WORD;
      /* Note that eh_return sets the LR -- do not overwrite it.  */
      if (!crtl->calls_eh_return)
	epilogue_load_reg (LINK_REGNUM, base, off, dwarf);
    }
  if ((mask >> HARD_FRAME_POINTER_REGNUM) & 1)
    {
      off -= UNITS_PER_WORD;
      /* If we're using the frame pointer to do the restores,
	 then restore it last.  */
      if (base == hard_frame_pointer_rtx)
	fp = off;
      else
	epilogue_load_reg (HARD_FRAME_POINTER_REGNUM, base, off, dwarf);
    }

  for (int regno = 0; regno <= OR1K_LAST_ACTUAL_REG; regno++)
    {
      if (regno == LINK_REGNUM || regno == HARD_FRAME_POINTER_REGNUM)
	continue;
      if ((mask >> regno) & 1)
	{
	  off -= UNITS_PER_WORD;
	  epilogue_load_reg (regno, base, off, dwarf);
	}
    }

  if (fp != -1)
    return epilogue_load_reg (HARD_FRAME_POINTER_REGNUM, base, fp, dwarf);
  else
    return NULL_RTX;
}

/* Expand code to deconstruct the local stack frame.  */
void
or1k_expand_epilogue (void)
{
  HOST_WIDE_INT total_size = frame_info.total_size;
  unsigned int save_mask = frame_info.save_mask;
  rtx insn, dwarf = NULL_RTX, x;
  bool regs_in_redzone = or1k_regs_in_redzone ();

  if (frame_pointer_needed)
    {
      /* Deallocate (most of) the local stack frame.  */
      if (total_size != 0)
	{
	  /* When the save registers are in the redzone, we can deallocate
	     all of the stack now.  Otherwise, deallocate all but the saved
	     registers.  In either case, this can be done via a simple
	     addition from the HFP.  Be careful to block local frame accesses
	     from crossing this boundary.  */
	  if (regs_in_redzone)
	    total_size = 0;
	  else
	    total_size = frame_info.save_size;
	  emit_insn (gen_frame_alloc (stack_pointer_rtx,
				      hard_frame_pointer_rtx,
				      GEN_INT (-total_size)));
	}

      insn = epilogue_load_registers (save_mask, hard_frame_pointer_rtx,
				      0, &dwarf);

      /* Since we just restored the frame pointer, we must
	 switch the CFA back to being based on the stack pointer.  */
      x = plus_constant (Pmode, stack_pointer_rtx, total_size);
      dwarf = alloc_reg_note (REG_CFA_DEF_CFA, x, dwarf);
      REG_NOTES (insn) = dwarf;
      RTX_FRAME_RELATED_P (insn) = 1;
      dwarf = NULL_RTX;
    }
  else
    {
      if (regs_in_redzone)
	{
	  pro_epi_adjust_stack (total_size, false, true);
	  total_size = 0;
	}
      else if (total_size >= 32768)
	{
	  HOST_WIDE_INT save_size = frame_info.save_size;
	  pro_epi_adjust_stack (total_size - save_size, false, true);
	  total_size = save_size;
	}

      epilogue_load_registers (save_mask, stack_pointer_rtx,
			       total_size, &dwarf);
    }

  /* Finish stack frame deallocation.  This will only be needed if
     the register save area is not in the redzone.  */
  if (total_size != 0)
    {
      insn = pro_epi_adjust_stack (total_size, false, false);
      dwarf = alloc_reg_note (REG_CFA_DEF_CFA, stack_pointer_rtx, dwarf);
      REG_NOTES (insn) = dwarf;
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Move up to the stack frame of an exception handler.  */
  if (crtl->calls_eh_return)
    emit_insn (gen_add2_insn (stack_pointer_rtx, EH_RETURN_STACKADJ_RTX));

  /* Jump!  */
  emit_jump_insn (gen_return ());
}

/* -------------------------------------------------------------------------- */
/*!Generate assembler code for a movdi/movdf pattern

   @param[in] operands  Operands to the movdx pattern.

   @return  The assembler string to output (always "", since we've done the
            output here).                                                     */
/* -------------------------------------------------------------------------- */
const char *
or1k_output_move_double (rtx *operands)
{
  rtx xoperands[3];

  switch (GET_CODE (operands[0]))
    {
    case REG:
      if (GET_CODE (operands[1]) == REG)
	{
	  if (REGNO (operands[0]) == REGNO (operands[1]) + 1)
	    {
	      output_asm_insn ("\tl.or    \t%H0, %H1, r0", operands);
	      output_asm_insn ("\tl.or    \t%0, %1, r0", operands);
	      return "";
	    }
	  else
	    {
	      output_asm_insn ("\tl.or    \t%0, %1, r0", operands);
	      output_asm_insn ("\tl.or    \t%H0, %H1, r0", operands);
	      return "";
	    }
	}
      else if (GET_CODE (operands[1]) == MEM)
	{
	  xoperands[1] = XEXP (operands[1], 0);
	  if (GET_CODE (xoperands[1]) == REG)
	    {
	      xoperands[0] = operands[0];
	      if (REGNO (xoperands[0]) == REGNO (xoperands[1]))
		{
		  output_asm_insn ("\tl.lwz   \t%H0, 4(%1)", xoperands);
		  output_asm_insn ("\tl.lwz   \t%0, 0(%1)", xoperands);
		  return "";
		}
	      else
		{
		  output_asm_insn ("\tl.lwz   \t%0, 0(%1)", xoperands);
		  output_asm_insn ("\tl.lwz   \t%H0, 4(%1)", xoperands);
		  return "";
		}
	    }
	  else if (GET_CODE (xoperands[1]) == PLUS)
	    {
	      if (GET_CODE (xoperands[2] = XEXP (xoperands[1], 1)) == REG)
		{
		  xoperands[0] = operands[0];
		  xoperands[1] = XEXP (xoperands[1], 0);
		  if (REGNO (xoperands[0]) == REGNO (xoperands[2]))
		    {
		      output_asm_insn ("\tl.lwz   \t%H0, %1+4(%2)",
				       xoperands);
		      output_asm_insn ("\tl.lwz   \t%0, %1(%2)", xoperands);
		      return "";
		    }
		  else
		    {
		      output_asm_insn ("\tl.lwz   \t%0, %1(%2)", xoperands);
		      output_asm_insn ("\tl.lwz   \t%H0, %1+4(%2)",
				       xoperands);
		      return "";
		    }
		}
	      else if (GET_CODE (xoperands[2] = XEXP (xoperands[1], 0)) ==
		       REG)
		{
		  xoperands[0] = operands[0];
		  xoperands[1] = XEXP (xoperands[1], 1);
		  if (REGNO (xoperands[0]) == REGNO (xoperands[2]))
		    {
		      output_asm_insn ("\tl.lwz   \t%H0, %1+4(%2)",
				       xoperands);
		      output_asm_insn ("\tl.lwz   \t%0, %1(%2)", xoperands);
		      return "";
		    }
		  else
		    {
		      output_asm_insn ("\tl.lwz   \t%0, %1(%2)", xoperands);
		      output_asm_insn ("\tl.lwz   \t%H0, %1+4(%2)",
				       xoperands);
		      return "";
		    }
		}
	      else
		abort ();
	    }
	  else
	    abort ();
	}
      else
	abort ();
    case MEM:
      xoperands[0] = XEXP (operands[0], 0);
      if (GET_CODE (xoperands[0]) == REG)
	{
	  xoperands[1] = operands[1];
	  output_asm_insn ("\tl.sw    \t0(%0), %1", xoperands);
	  output_asm_insn ("\tl.sw    \t4(%0), %H1", xoperands);
	  return "";
	}
      else if (GET_CODE (xoperands[0]) == PLUS)
	{
	  if (GET_CODE (xoperands[1] = XEXP (xoperands[0], 1)) == REG)
	    {
	      xoperands[0] = XEXP (xoperands[0], 0);
	      xoperands[2] = operands[1];
	      output_asm_insn ("\tl.sw    \t%0(%1), %2", xoperands);
	      output_asm_insn ("\tl.sw    \t%0+4(%1), %H2", xoperands);
	      return "";
	    }
	  else if (GET_CODE (xoperands[1] = XEXP (xoperands[0], 0)) == REG)
	    {
	      xoperands[0] = XEXP (xoperands[0], 1);
	      xoperands[2] = operands[1];
	      output_asm_insn ("\tl.sw    \t%0(%1), %2", xoperands);
	      output_asm_insn ("\tl.sw    \t%0+4(%1), %H2", xoperands);
	      return "";
	    }
	  else
	    abort ();
	}
      else
	{
	  fprintf (stderr, "  O/p error %s\n",
		   GET_RTX_NAME (GET_CODE (xoperands[0])));
	  return "";
	  /* abort (); */
	}
    default:
      abort ();
    }
}	/* or1k_output_move_double () */


/* -------------------------------------------------------------------------- */
/*!Load a 32-bit constant.

   We know it can't be done in one insn when we get here, the movsi expander
   guarantees this.

   @param[in] op0  RTX for the destination.
   @param[in] op1  RTX for the (constant) source.                             */
/* -------------------------------------------------------------------------- */
void
or1k_emit_set_const32 (rtx  op0,
		       rtx  op1)
{
  enum machine_mode mode = GET_MODE (op0);
  rtx temp;

  /* Sanity check that we really can't do it in one instruction. I.e that we
     don't have a 16-bit constant. */
  if (GET_CODE (op1) == CONST_INT)
    {
      HOST_WIDE_INT val = INTVAL (op1) & GET_MODE_MASK (mode);

      if ((-32768 <= val) && (val <= 32767))
	{
	  abort ();
	}
    }

  /* Full 2-insn decomposition is needed.  */
  if (reload_in_progress || reload_completed)
    temp = op0;
  else
    temp = gen_reg_rtx (mode);

  if (GET_CODE (op1) == CONST_INT)
    {
      /* Emit them as real moves instead of a HIGH/LO_SUM,
         this way CSE can see everything and reuse intermediate
         values if it wants.  */
      emit_insn (gen_rtx_SET (temp,
			      GEN_INT (INTVAL (op1)
				       & ~(HOST_WIDE_INT) 0xffff)));

      emit_insn (gen_rtx_SET (op0,
			      gen_rtx_IOR (mode, temp,
					   GEN_INT (INTVAL (op1) & 0xffff))));
    }
  else
    {
      /* since or1k bfd can not deal with relocs that are not of type
         OR1K_CONSTH_RELOC + OR1K_CONST_RELOC (ie move high must be
         followed by exactly one lo_sum)
       */
      emit_insn (gen_movsi_insn_big (op0, op1));
    }
}	/* or1k_emit_set_const32 () */


/* ========================================================================== */
/* Target hook functions.

   These are initialized at the end of this file, to avoid having to
   predeclare all the functions. They are only needed here, so are static.    */




/* -------------------------------------------------------------------------- */
/*!Define where a function returns values.

   Define this to return an RTX representing the place where a function
   returns or receives a value of data type ret type, a tree node representing
   a data type.  "func" is a tree node representing FUNCTION_DECL or
   FUNCTION_TYPE of a function being called. If "outgoing" is false, the hook
   should compute the register in which the caller will see the return
   value. Otherwise, the hook should return an RTX representing the place
   where a function returns a value.

   On many machines, only TYPE_MODE ("ret_type") is relevant. (Actually, on
   most machines, scalar values are returned in the same place regardless of
   mode.) The value of the expression is usually a reg RTX for the hard
   register where the return value is stored. The value can also be a parallel
   RTX, if the return value is in multiple places. See FUNCTION_ARG for an
   explanation of the parallel form. Note that the callee will populate every
   location specified in the parallel, but if the first element of the
   parallel contains the whole return value, callers will use that element as
   the canonical location and ignore the others. The m68k port uses this type
   of parallel to return pointers in both ‘%a0’ (the canonical location) and
   ‘%d0’.

   If TARGET_PROMOTE_FUNCTION_RETURN returns true, you must apply the same
   promotion rules specified in PROMOTE_MODE if valtype is a scalar type.

   If the precise function being called is known, "func" is a tree node
   (FUNCTION_DECL) for it; otherwise, "func" is a null pointer. This makes it
   possible to use a different value-returning convention for specific
   functions when all their calls are known.

   Some target machines have "register windows" so that the register in which
   a function returns its value is not the same as the one in which the caller
   sees the value. For such machines, you should return different RTX
   depending on outgoing.

   TARGET_FUNCTION_VALUE is not used for return values with aggregate data
   types, because these are returned in another way. See
   TARGET_STRUCT_VALUE_RTX and related macros.

   For the OR1K, we can just use the result of LIBCALL_VALUE, since all
   functions return their result in the same place (register rv = r11).

   JPB 30-Aug-10: What about 64-bit scalar returns (long long int, double),
                  which also use rvh (=r12)?

   @param[in] ret_type  The return type of the function.
   @param[in] func      Tree representing function being called.
   @param[in] outgoing  Non-zero (TRUE) if the result represents where the
                        function places the results, zero (FALSE) if the
                        result represents where the caller sees the result.

   @return  A RTX representing where the result can be found.                 */
/* -------------------------------------------------------------------------- */
static rtx
or1k_function_value (const_tree  ret_type,
		     const_tree  func ATTRIBUTE_UNUSED,
                     bool        outgoing ATTRIBUTE_UNUSED)
{
  return LIBCALL_VALUE (TYPE_MODE(ret_type));

}	/* or1k_function_value () */


/* -------------------------------------------------------------------------- */
/*!Check if a function is suitable for tail call optimization.

   True if it is OK to do sibling call optimization for the specified call
   expression "exp". "decl" will be the called function, or NULL if this is an
   indirect call.

   It is not uncommon for limitations of calling conventions to prevent tail
   calls to functions outside the current unit of translation, or during PIC
   compilation. The hook is used to enforce these restrictions, as the sibcall
   md pattern can not fail, or fall over to a “normal” call. The criteria for
   successful sibling call optimization may vary greatly between different
   architectures.

   For the OR1K, we currently don't allow sibcalls.

   @param[in] decl  The function for which we may optimize
   @param[in] exp   The call expression which is candidate for optimization.

   @return  Non-zero (TRUE) if sibcall optimization is permitted, zero (FALSE)
            otherwise.                                                        */
/* -------------------------------------------------------------------------- */
static bool
or1k_function_ok_for_sibcall (tree  decl ATTRIBUTE_UNUSED,
			      tree  exp ATTRIBUTE_UNUSED)
{
  return 0;
}	/* or1k_function_ok_for_sibcall () */


/* -------------------------------------------------------------------------- */
/*!Should an argument be passed by reference.

   This target hook should return true if an argument at the position
   indicated by "cum" should be passed by reference. This predicate is queried
   after target independent reasons for being passed by reference, such as
   TREE_ADDRESSABLE ("type").

   If the hook returns TRUE, a copy of that argument is made in memory and a
   pointer to the argument is passed instead of the argument itself. The
   pointer is passed in whatever way is appropriate for passing a pointer to
   that type.

   For the OR1K, all aggregates and arguments greater than 8 bytes are passed
   this way.

   @param[in] cum    Position of argument under consideration.
   @param[in[ mode   Not sure what this relates to.
   @param[in] type   Type of the argument.
   @param[in] named  Not sure what this relates to.

   @return  Non-zero (TRUE) if the argument should be passed by reference,
            zero (FALSE) otherwise.                                           */
/* -------------------------------------------------------------------------- */
static bool
or1k_pass_by_reference (cumulative_args_t  cum ATTRIBUTE_UNUSED,
                        enum machine_mode  mode ATTRIBUTE_UNUSED,
                        const_tree         type,
                        bool               named ATTRIBUTE_UNUSED)
{
  return (type && (AGGREGATE_TYPE_P (type) || int_size_in_bytes (type) > 8));

}	/* or1k_pass_by_reference () */


/* -------------------------------------------------------------------------- */
/*!How many bytes at the beginning of an argument must be put into registers.

   This target hook returns the number of bytes at the beginning of an
   argument that must be put in registers. The value must be zero for
   arguments that are passed entirely in registers or that are entirely pushed
   on the stack.

   On some machines, certain arguments must be passed partially in registers
   and partially in memory. On these machines, typically the first few words
   of arguments a re passed in registers, and the rest on the stack. If a
   multi-word argument (a double or a structure) crosses that boundary, its
   first few words must be passed in registers and the rest must be
   pushed. This macro tells the compiler when this occurs, and how many bytes
   should go in registers.

   FUNCTION_ARG for these arguments should return the first register to be
   used by the caller for this argument; likewise FUNCTION_INCOMING_ARG, for
   the called function.

   On the OR1K we never split argumetns between registers and memory.

   JPB 30-Aug-10: Is this correct? Surely we should allow this. The ABI spec
                  is incomplete on this point.

   @param[in] cum    Position of argument under consideration.
   @param[in[ mode   Not sure what this relates to.
   @param[in] type   Type of the argument.
   @param[in] named  Not sure what this relates to.

   @return  The number of bytes of the argument to go into registers          */
/* -------------------------------------------------------------------------- */
static int
or1k_arg_partial_bytes (cumulative_args_t cum ATTRIBUTE_UNUSED,
                        enum machine_mode  mode ATTRIBUTE_UNUSED,
                        tree               type ATTRIBUTE_UNUSED,
                        bool               named ATTRIBUTE_UNUSED)
{
  return 0;

}	/* or1k_arg_partial_bytes () */


/* -------------------------------------------------------------------------- */
/*!Promote the mode of a function's arguments/return value.

   Like PROMOTE_MODE, but it is applied to outgoing function arguments or
   function return values. The target hook should return the new mode and
   possibly change "*punsignedp" if the promotion should change
   signedness. This function is called only for scalar or pointer types.

   "for_return" allows to distinguish the promotion of arguments and return
   values. If it is 1, a return value is being promoted and
   TARGET_FUNCTION_VALUE must perform the same promotions done here. If it is
   2, the returned mode should be that of the register in which an incoming
   parameter is copied, or the outgoing result is computed; then the hook
   should return the same mode as PROMOTE_MODE, though the signedness may be
   different.

   The default is to not promote arguments and return values. You can also
   define the hook to "default_promote_function_mode_always_promote" if you
   would like to apply the same rules given by PROMOTE_MODE.

   For the OR1K, if the size of the mode is integral and less than 4, we
   promote to SImode, otherwise we return the mode we are supplied.

   @param[in]  type        Not sure. Type of the argument?
   @param[in]  mode        The mode of argument/return value to consider.
   @param[out] punsignedp  Signedness of the value.
   @param[in]  fntype      Not sure. Type of the function?
   @param[in]  for_return  1 if a return value, 2 if an incoming value.

   @return  The new mode.                                                     */
/* -------------------------------------------------------------------------- */
static enum machine_mode
or1k_promote_function_mode (const_tree         type ATTRIBUTE_UNUSED,
			    enum machine_mode  mode,
			    int               *punsignedp ATTRIBUTE_UNUSED,
			    const_tree         fntype ATTRIBUTE_UNUSED,
			    int                for_return ATTRIBUTE_UNUSED)
{
  return (   (GET_MODE_CLASS (mode) == MODE_INT)
	  && (GET_MODE_SIZE (mode) < 4)) ? SImode : mode;

}	/* or1k_promote_function_mode () */


/* -------------------------------------------------------------------------- */
/*!Is this a legitimate address?

  A function that returns whether x (an RTX) is a legitimate memory address on
  the target machine for a memory operand of mode mode.

  Legitimate addresses are defined in two variants: a strict variant and a
  non-strict one.  The strict parameter chooses which variant is desired by
  the caller.

  The strict variant is used in the reload pass. It must be defined so that
  any pseudo- register that has not been allocated a hard register is
  considered a memory reference.  This is because in contexts where some kind
  of register is required, a pseudo-register with no hard register must be
  rejected. For non-hard registers, the strict variant should look up the
  reg_renumber array; it should then proceed using the hard register number in
  the array, or treat the pseudo as a memory reference if the array holds -1.

  The non-strict variant is used in other passes. It must be defined to accept
  all pseudo-registers in every context where some kind of register is
  required.

  Normally, constant addresses which are the sum of a symbol_ref and an
  integer are stored inside a const RTX to mark them as constant. Therefore,
  there is no need to recognize such sums specifically as legitimate
  addresses. Normally you would simply recognize any const as legitimate.

  Usually PRINT_OPERAND_ADDRESS is not prepared to handle constant sums that
  are not marked with const. It assumes that a naked plus indicates
  indexing. If so, then you must reject such naked constant sums as
  illegitimate addresses, so that none of them will be given to
  PRINT_OPERAND_ADDRESS.

  On some machines, whether a symbolic address is legitimate depends on the
  section that the address refers to. On these machines, define the target
  hook TARGET_ENCODE_ SECTION_INFO to store the information into the
  symbol_ref, and then check for it here. When you see a const, you will have
  to look inside it to find the symbol_ref in order to determine the
  section. See the internals manual section on "Assembler Format" for more
  info.

  Some ports are still using a deprecated legacy substitute for this hook, the
  GO_IF_LEGITIMATE_ADDRESS macro. This macro has this syntax:

    #define GO_IF_LEGITIMATE_ADDRESS (mode, x, label )

  and should goto label if the address x is a valid address on the target
  machine for a memory operand of mode mode. Whether the strict or non-strict
  variants are desired is defined by the REG_OK_STRICT macro introduced
  earlier in this section. Using the hook is usually simpler because it limits
  the number of files that are recompiled when changes are made.

   The OR1K only has a single addressing mode, which is a base register with
   16-bit displacement. We can accept just 16-bit constants as addresses (they
   can use r0 as base address, and we can accept plain registers as addresses
   (they can use a displacement of zero).

   @param[in] mode    The mode of the address
   @param[in] x       The address (RTX)
   @param[in] strict  Non-zero (TRUE) if we are in "strict" mode, zero (FALSE)
                      otherwise.

   @return  Non-zero (TRUE) if this is a legitimate address, zero (FALSE)
            otherwise.                                                        */
/* -------------------------------------------------------------------------- */
static bool
or1k_legitimate_address_p (enum machine_mode  mode ATTRIBUTE_UNUSED,
			   rtx                x,
			   bool               strict)
{
  if (GET_CODE (x) == PLUS)
    {
      rtx offset = XEXP (x, 1);
      x = XEXP (x, 0);

      /* Register elimination is going to adjust all of these offsets.
	 We might as well keep them as a unit until then.  */
      if (!strict && (x == arg_pointer_rtx || x == frame_pointer_rtx))
	return CONST_INT_P (offset);

      /* Otherwise, keep the offset within 16-bit signed.  */
      if (! satisfies_constraint_I (offset))
        return 0;
    }

  /* Addresses consisting of just a register are OK.  */
  return REG_P (x) && or1k_regnum_ok_for_base_p (REGNO (x), strict);
}

/* -------------------------------------------------------------------------- */
/*!Initialize a trampoline for nested functions.

   A nested function is defined by *two* pieces of information, the address of
   the function (like any other function) and a pointer to the frame of the
   enclosing function. The latter is required to allow the nested function to
   access local variables in the enclosing function's frame.

   This represents a problem, since a function in C is represented as an
   address that can be held in a single variable as a pointer. Requiring two
   pointers will not fit.

   The solution is documented in "Lexical Closures for C++" by Thomas
   M. Breuel (USENIX C++ Conference Proceedings, October 17-21, 1988). The
   nested function is represented by a small block of code and data on the
   enclosing function's stack frame, which sets up a pointer to the enclosing
   function's stack frame (the static chain pointer) in a register defined by
   the ABI, and then jumps to the code of the function proper.

   The function can be represented as a single pointer to this block of code,
   known as a trampoline, which when called generates both pointers
   needed. The nested function (which knows it is a nested function at compile
   time) can then generate code to access the enclosing frame via the static
   chain register.

   There is a catch that the trampoline is set up as data, but executed as
   instructions. The former will be via the data cache, the latter via the
   instruction cache. There is a risk that a later trampoline will not be seen
   by the instruction cache, so the wrong code will be executed. So the
   instruction cache should be flushed for the trampoline address range.

   This hook is called to initialize a trampoline. "m_tramp" is an RTX for the
   memory block for the trampoline; "fndecl" is the FUNCTION_DECL for the
   nested function; "static_chain" is an RTX for the static chain value that
   should be passed to the function when it is called.

   If the target defines TARGET_ASM_TRAMPOLINE_TEMPLATE, then the first thing
   this hook should do is emit a block move into "m_tramp" from the memory
   block returned by assemble_trampoline_template. Note that the block move
   need only cover the constant parts of the trampoline. If the target
   isolates the variable parts of the trampoline to the end, not all
   TRAMPOLINE_SIZE bytes need be copied.

   If the target requires any other actions, such as flushing caches or
   enabling stack execution, these actions should be performed after
   initializing the trampoline proper.

   For the OR1K, no static chain register is used. We choose to use the return
   value (rv) register. The code is based on that for MIPS.
   The trampoline code is:

              l.movhi r11,hi(end_addr)
              l.ori   r11,lo(end_addr)
              l.lwz   r13,4(r11)
              l.jr    r13
              l.lwz   r11,0(r11)
      end_addr:
              .word   <static chain>
              .word   <nested_function>

   @note For the OR1K we need to flush the instruction cache, which is a
         privileged operation. Needs fixing.

   @param[in] m_tramp      The lowest address of the trampoline on the stack.
   @param[in] fndecl       Declaration of the enclosing function.
   @param[in] chain_value  Static chain pointer to pass to the nested
                           function.                                          */
/* -------------------------------------------------------------------------- */
static void
or1k_trampoline_init (rtx   m_tramp,
		      tree  fndecl,
		      rtx   chain_value)
{
  rtx  addr;				/* Start address of the trampoline */
  rtx  end_addr;			/* End address of the code block */

  rtx  high;				/* RTX for the high part of end_addr */
  rtx  low;				/* RTX for the low part of end_addr */
  rtx  opcode;				/* RTX for generated opcodes */
  rtx  mem;				/* RTX for trampoline memory */

  rtx *trampoline;	/* The trampoline code */

  unsigned int  i;			/* Index into trampoline */
  unsigned int  j;			/* General counter */

  HOST_WIDE_INT  end_addr_offset;	  /* Offset to end of code */
  HOST_WIDE_INT  static_chain_offset;	  /* Offset to stack chain word */
  HOST_WIDE_INT  target_function_offset;  /* Offset to func address word */

  /* Work out the offsets of the pointers from the start of the trampoline
     code.  */
  trampoline             = (rtx*) alloca (or1k_trampoline_code_words() * sizeof(rtx));
  end_addr_offset        = or1k_trampoline_code_size ();
  static_chain_offset    = end_addr_offset;
  target_function_offset = static_chain_offset + GET_MODE_SIZE (ptr_mode);

  /* Get pointers in registers to the beginning and end of the code block.  */
  addr     = force_reg (Pmode, XEXP (m_tramp, 0));
  end_addr = or1k_force_binary (Pmode, PLUS, addr, GEN_INT (end_addr_offset));

  /* Build up the code in TRAMPOLINE.

              l.movhi r11,hi(end_addr)
              l.ori   r11,lo(end_addr)
              l.lwz   r13,4(r11)
              l.jr    r13
              l.lwz   r11,0(r11)
       end_addr:
  */

  i = 0;

  /* Break out the high and low parts of the end_addr */
  high = expand_simple_binop (SImode, LSHIFTRT, end_addr, GEN_INT (16),
			      NULL, false, OPTAB_WIDEN);
  low  = convert_to_mode (SImode, gen_lowpart (HImode, end_addr), true);

  /* Emit the l.movhi, adding an operation to OR in the high bits from the
     RTX. */
  opcode = gen_int_mode (OR1K_MOVHI (11, 0), SImode);
  trampoline[i++] = expand_simple_binop (SImode, IOR, opcode, high, NULL,
					 false, OPTAB_WIDEN); 
  
  /* Emit the l.ori, adding an operations to OR in the low bits from the
     RTX. */
  opcode = gen_int_mode (OR1K_ORI (11, 11, 0), SImode);
  trampoline[i++] = expand_simple_binop (SImode, IOR, opcode, low, NULL,
					 false, OPTAB_WIDEN); 

  /* Emit the l.lwz of the function address. No bits to OR in here, so we can
     do the opcode directly. */
  trampoline[i++] =
    gen_int_mode (OR1K_LWZ (13, 11, target_function_offset - end_addr_offset),
		  SImode);

  if (TARGET_DELAY_ON) {
    /* Emit the l.jr of the function. No bits to OR in here, so we can do the
       opcode directly. */
    trampoline[i++] = gen_int_mode (OR1K_JR (13), SImode);
    
    /* Emit the l.lwz of the static chain. No bits to OR in here, so we can
       do the opcode directly. */
    trampoline[i++] =
      gen_int_mode (OR1K_LWZ (STATIC_CHAIN_REGNUM, 11,
                              static_chain_offset - end_addr_offset), SImode);
  } else {
    trampoline[i++] =
      gen_int_mode (OR1K_LWZ (STATIC_CHAIN_REGNUM, 11,
                              static_chain_offset - end_addr_offset), SImode);
    trampoline[i++] = gen_int_mode (OR1K_JR (13), SImode);
    if (TARGET_DELAY_COMPAT)
      trampoline[i++] = gen_int_mode (OR1K_NOP, SImode);
  }

  /* Copy the trampoline code.  Leave any padding uninitialized.  */
  for (j = 0; j < i; j++)
    {
      mem = adjust_address (m_tramp, SImode, j * GET_MODE_SIZE (SImode));
      or1k_emit_move (mem, trampoline[j]);
    }

  /* Set up the static chain pointer field.  */
  mem = adjust_address (m_tramp, ptr_mode, static_chain_offset);
  or1k_emit_move (mem, chain_value);

  /* Set up the target function field.  */
  mem = adjust_address (m_tramp, ptr_mode, target_function_offset);
  or1k_emit_move (mem, XEXP (DECL_RTL (fndecl), 0));

  /* Flushing the trampoline from the instruction cache needs to be done
     here. */

}	/* or1k_trampoline_init () */


/* -------------------------------------------------------------------------- */
/*!Provide support for DW_AT_calling_convention

   Define this to enable the dwarf attribute DW_AT_calling_convention to be
   emitted for each function. Instead of an integer return the enum value for
   the DW_CC_ tag.

   To support optional call frame debugging information, you must also define
   INCOMING_RETURN_ADDR_RTX and either set RTX_FRAME_RELATED_P on the prologue
   insns if you use RTL for the prologue, or call "dwarf2out_def_cfa" and
   "dwarf2out_reg_save" as appropriate from TARGET_ASM_FUNCTION_PROLOGUE if
   you don’t.

   For the OR1K, it should be sufficient to return DW_CC_normal in all cases.

   @param[in] function  The function requiring debug information

   @return  The enum of the DW_CC tag.                                        */
/* -------------------------------------------------------------------------- */
static int
or1k_dwarf_calling_convention (const_tree  function ATTRIBUTE_UNUSED)
{
  return  DW_CC_normal;

}	/* or1k_dwarf_calling_convention () */

/* ========================================================================== */
/* Target hook initialization.

   In most cases these use the static functions declared above. They have
   defaults, so must be undefined first, before being redefined.

   The description of what they do is found with the function above, unless it
   is a standard function or a constant, in which case it is defined here (as
   with TARGET_ASM_NAMED_SECTION).

   The final declaration is of the global "targetm" structure. */

/* Output assembly directives to switch to section name. The section should
   have attributes as specified by flags, which is a bit mask of the SECTION_*
   flags defined in ‘output.h’. If decl is non-NULL, it is the VAR_DECL or
   FUNCTION_DECL with which this section is associated.

   For OR1K, we use the default ELF sectioning. */
#undef  TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION  default_elf_asm_named_section

#undef  TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE or1k_function_value

#undef  TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL or1k_function_ok_for_sibcall

#undef  TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE or1k_pass_by_reference

#undef  TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES or1k_arg_partial_bytes

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE or1k_option_override

#undef TARGET_ASM_FILE_START
#define TARGET_ASM_FILE_START or1k_asm_file_start

/* This target hook returns TRUE if an argument declared in a prototype as an
   integral type smaller than int should actually be passed as an int. In
   addition to avoiding errors in certain cases of mismatch, it also makes for
   better code on certain machines.

   The default is to not promote prototypes.

   For the OR1K we do require this, so use a utility hook, which always
   returns TRUE. */
#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_true

#undef  TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE or1k_promote_function_mode

#undef  TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P  or1k_legitimate_address_p

#undef  TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS or1k_legitimize_address

#undef  TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT  or1k_trampoline_init

#undef  TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM or1k_cannot_force_const_mem

#undef TARGET_DWARF_CALLING_CONVENTION
#define TARGET_DWARF_CALLING_CONVENTION  or1k_dwarf_calling_convention

/* uClibc has some instances where (non-coforming to ISO C) a non-varargs
   prototype is in scope when calling that function which is implemented
   as varargs.  We want this to work at least where none of the anonymous
   arguments are used.  I.e. we want the last named argument to be known
   as named so it can be passed in a register, varars funtion or not.  */
#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING hook_bool_CUMULATIVE_ARGS_true

/* Is this suitable for an immediate operand.

   JPB 1-Sep-10: Is this correct. We can only do 16-bit immediates directly. */
static bool
or1k_legitimate_constant_p (enum machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  if (or1k_tls_symbolic_operand (x) != TLS_MODEL_NONE)
    return 0;

  return GET_CODE(x) != CONST_DOUBLE || (GET_MODE (x) == VOIDmode && !flag_pic);
}
#undef TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P or1k_legitimate_constant_p

/* On the OR1K, no functions pop their arguments.
   JPB 29-Aug-10: Is this really correct? */
static int
or1k_return_pops_args (tree ARG_UNUSED(fundecl), tree ARG_UNUSED(funtype), int ARG_UNUSED(size))
{
  return 0;
}
#undef TARGET_RETURN_POPS_ARGS
#define TARGET_RETURN_POPS_ARGS or1k_return_pops_args

/* Determine where to put an argument to a function.  Value is zero to push
   the argument on the stack, or a hard register in which to store the
   argument.

   "mode" is the argument's machine mode.

   "type" is the data type of the argument (as a tree).  This is null for
    libcalls where that information may not be available.

   "cum" is a variable of type CUMULATIVE_ARGS which gives info about the
    preceding args and about the function being called.

   "named" is nonzero if this argument is a named parameter (otherwise it is
    an extra parameter matching an ellipsis).

    On the ARC the first MAX_ARC_PARM_REGS args are normally in registers and
    the rest are pushed.  */
static rtx
or1k_function_arg (cumulative_args_t cum, enum machine_mode mode,
                     const_tree type, bool named)
{
  CUMULATIVE_ARGS *cum_pnt = get_cumulative_args (cum);

  if (OR1K_PASS_IN_REG_P (*cum_pnt, mode, type, named))
    return gen_rtx_REG (mode, OR1K_ROUND_ADVANCE_CUM (*cum_pnt, mode, type)
                          + GP_ARG_MIN_REG);
  else
    return 0;
}
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG or1k_function_arg
/* Update the data in "cum" to advance over an argument of mode "mode" and
   data type "type".  ("type" is null for libcalls where that information may
   not be available.)  */
static void
or1k_function_arg_advance (cumulative_args_t cum, enum machine_mode mode,
                           const_tree type, bool ARG_UNUSED(named))
{
  CUMULATIVE_ARGS *cum_pnt = get_cumulative_args (cum);

  *cum_pnt = OR1K_ROUND_ADVANCE_CUM (*cum_pnt, mode, type)
    + OR1K_ROUND_ADVANCE_ARG (mode, type);
}

#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE or1k_function_arg_advance

#undef TARGET_PRINT_OPERAND_PUNCT_VALID_P
#define TARGET_PRINT_OPERAND_PUNCT_VALID_P or1k_print_operand_punct_valid_p

#undef TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND or1k_print_operand

#undef TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS or1k_print_operand_address

/* Trampoline stubs are yet to be written. */
/* #define TARGET_ASM_TRAMPOLINE_TEMPLATE */
/* #define TARGET_TRAMPOLINE_INIT */

/* Lay out structs with increased alignment so that they can be accessed
   more efficiently.  But don't increase the size of one or two byte
   structs.  */
int
or1k_struct_alignment (tree t)
{
  unsigned HOST_WIDE_INT total = 0;
  int default_align_fields = 0;
  int special_align_fields = 0;
  tree field;
  unsigned max_align
    = maximum_field_alignment ? maximum_field_alignment : BIGGEST_ALIGNMENT;
  bool struct_p;

  switch (TREE_CODE (t))
    {
    case RECORD_TYPE:
      struct_p = true; break;
    case UNION_TYPE: case QUAL_UNION_TYPE:
      struct_p = false; break;
    default: gcc_unreachable ();
    }
  /* Skip all non field decls */
  for (field = TYPE_FIELDS (t); field; field = TREE_CHAIN (field))
    {
      unsigned HOST_WIDE_INT field_size;

      if (TREE_CODE (field) != FIELD_DECL ||
	  TREE_TYPE (field) == error_mark_node)
	continue;
      /* If this is a field in a non-qualified union, or the sole field in
	 a struct, and the alignment was set by the user, don't change the
	 alignment.
	 If the field is a struct/union in a non-qualified union, we already
	 had sufficient opportunity to pad it - if we didn't, that'd be
	 because the alignment was set as above.
	 Likewise if the field is a struct/union and the sole field in a
	 struct.  */
      if (DECL_USER_ALIGN (field)
	  || TYPE_USER_ALIGN (TREE_TYPE (field))
	  || TREE_CODE (TREE_TYPE (field)) == UNION_TYPE
	  || TREE_CODE (TREE_TYPE (field)) == QUAL_UNION_TYPE
	  || TREE_CODE (TREE_TYPE (field)) == RECORD_TYPE)
	{
	  if (TREE_CODE (t) == UNION_TYPE)
	    return 0;
	  special_align_fields++;
	}
      else if (DECL_PACKED (field))
	special_align_fields++;
      else
	default_align_fields++;
      if (!tree_fits_uhwi_p (DECL_SIZE (field)))
	field_size = max_align;
      else
	field_size = tree_to_uhwi (DECL_SIZE (field));
      if (field_size >= BIGGEST_ALIGNMENT)
	total = max_align;
      if (struct_p)
	total += field_size;
      else
	total = MAX (total, field_size);
    }

  if (!default_align_fields
      && (TREE_CODE (t) != RECORD_TYPE || special_align_fields <= 1))
    return 0;
  return total < max_align ? (1U << ceil_log2 (total)) : max_align;
}

/* Increase the alignment of objects so that they are easier to copy.
   Note that this can cause more struct copies to be inlined, so code
   size might increase, but so should perfromance.  */
int
or1k_data_alignment (tree t, int align)
{
  if (align < FASTEST_ALIGNMENT && TREE_CODE (t) == ARRAY_TYPE)
    {
      int size = int_size_in_bytes (t);

      return (size > 0 && size < FASTEST_ALIGNMENT / BITS_PER_UNIT
	      ? (1 << floor_log2 (size)) * BITS_PER_UNIT
	      : FASTEST_ALIGNMENT);
    }
  return align;
}

static void
or1k_option_override (void)
{
  if (!TARGET_DELAY_ON)
    flag_delayed_branch = FALSE;
}

static void
or1k_asm_file_start(void)
{
  default_file_start();

  if (TARGET_DELAY_OFF) {
    fprintf(asm_out_file, "\t.nodelay\n");
  }
}

/* Implement EH_RETURN_HANDLER_RTX. 
 * Make eh_return use the link register. Epilogue LR restore
 * is suppressed for eh_return. */
rtx
or1k_eh_return_handler_rtx (void)
{
  return INCOMING_RETURN_ADDR_RTX;
}

/* Implement RETURN_ADDR_RTX.
 * We do not support moving back to a previous frame. */
rtx
or1k_return_addr_rtx (int count, rtx frame ATTRIBUTE_UNUSED)
{
  if (count != 0)
    return const0_rtx;

  /* We don't know if LR is going to be saved or if we're going to
   * be clobbering it with the GOT instruction.
   * Therefore the safest bet is to force a save of LR and use that.
   * Assume it's going to be first in the stack. */

  cfun->machine->force_lr_save = true;
  return gen_rtx_MEM (Pmode, plus_constant (Pmode, arg_pointer_rtx,
                                            -UNITS_PER_WORD));
}

/* Implement TARGET_FRAME_POINTER_REQUIRED.
 * We want frame pointer in eh_return and when alloca is used */
static bool
or1k_frame_pointer_required (void)
{
  return crtl->calls_eh_return || cfun->calls_alloca;
}

/* Functions to save and restore machine-specific function data.  */
static struct machine_function *
or1k_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}

void
or1k_init_expanders (void)
{
  /* Arrange to initialize and mark the machine per-function
   * status.  */
  init_machine_status = or1k_init_machine_status;

  if (cfun && cfun->machine)
    {
      cfun->machine->force_lr_save = false;
    }
}

#undef  TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED or1k_frame_pointer_required

/* Initialize the GCC target structure.  */
struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-or1k.h"
