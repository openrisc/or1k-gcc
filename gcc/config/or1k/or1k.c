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
#include "alias.h"
#include "cfgrtl.h"

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
  HOST_WIDE_INT static_size;
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
      return (!crtl->is_leaf
	      || cfun->machine->force_lr_save
	      || crtl->uses_pic_offset_table
	      || df_regs_ever_live_p (regno));

    case HW_TO_GCC_REGNO (25):
    case HW_TO_GCC_REGNO (27):
    case HW_TO_GCC_REGNO (29):
    case HW_TO_GCC_REGNO (31):
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

  for (unsigned int regno = 1; regno <= OR1K_LAST_HGR_REGNUM; regno++)
    if (or1k_save_reg_p (regno))
      {
	save_size += UNITS_PER_WORD;
	save_mask |= 1U << regno;
      }

  vars_size = get_frame_size ();
  vars_size = OR1K_ALIGN (vars_size, UNITS_PER_WORD);
  args_size = crtl->outgoing_args_size;
  total_size = vars_size + save_size + args_size;

  frame_info.static_size = total_size;
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

/* A subroutine of the atomic operation splitters.  Jump to LABEL if
   COND is true.  Mark the jump as unlikely to be taken.  */

static void
emit_unlikely_jump (rtx_code code, rtx label)
{
  int very_unlikely = REG_BR_PROB_BASE / 100 - 1;
  rtx_insn *i;
  rtx x;

  x = gen_rtx_REG (BImode, SR_F_REG);
  x = gen_rtx_fmt_ee (code, VOIDmode, x, const0_rtx);
  x = gen_rtx_IF_THEN_ELSE (VOIDmode, x, label, pc_rtx);
  i = emit_jump_insn (gen_rtx_SET (pc_rtx, x));
  add_int_reg_note (i, REG_BR_PROB, very_unlikely);
}

static void
emit_compare (rtx_code code, rtx a, rtx b)
{
  emit_insn (gen_rtx_SET (gen_rtx_REG (BImode, SR_F_REG),
			  gen_rtx_fmt_ee (code, BImode, a, b)));
}

/* A subroutine of the atomic operation splitters.  Emit a load-locked
   instruction in MODE.  */

static void
emit_load_locked (machine_mode mode, rtx reg, rtx mem)
{
  gcc_assert (mode == SImode);
  emit_insn (gen_load_locked_si (reg, mem));
}

/* A subroutine of the atomic operation splitters.  Emit a store-conditional
   instruction in MODE.  */

static void
emit_store_conditional (machine_mode mode, rtx mem, rtx val)
{
  gcc_assert (mode == SImode);
  emit_insn (gen_store_conditional_si (mem, val));
}

/* A subroutine of the various atomic expanders.  For sub-word operations,
   we must adjust things to operate on SImode.  Given the original MEM,
   return a new aligned memory.  Also build and return the quantities by
   which to shift and mask.  */

static rtx
or1k_adjust_atomic_subword (rtx orig_mem, rtx *pshift, rtx *pmask)
{
  rtx addr, align, shift, mask, mem;
  machine_mode mode = GET_MODE (orig_mem);

  addr = XEXP (orig_mem, 0);
  addr = force_reg (Pmode, addr);

  /* Aligned memory containing subword.  Generate a new memory.  We
     do not want any of the existing MEM_ATTR data, as we're now
     accessing memory outside the original object.  */
  align = expand_binop (Pmode, and_optab, addr, GEN_INT (-4),
			NULL_RTX, 1, OPTAB_LIB_WIDEN);
  mem = gen_rtx_MEM (SImode, align);
  MEM_VOLATILE_P (mem) = MEM_VOLATILE_P (orig_mem);
  if (MEM_ALIAS_SET (orig_mem) == ALIAS_SET_MEMORY_BARRIER)
    set_mem_alias_set (mem, ALIAS_SET_MEMORY_BARRIER);

  /* Shift amount for subword relative to aligned word.  */
  rtx mode_mask = GEN_INT (mode == QImode ? 3 : 2);
  shift = expand_binop (SImode, and_optab, gen_lowpart (SImode, addr),
			mode_mask, NULL_RTX, 1, OPTAB_LIB_WIDEN);
  if (BYTES_BIG_ENDIAN)
    shift = expand_binop (SImode, xor_optab, shift, mode_mask,
			  shift, 1, OPTAB_LIB_WIDEN);
  shift = expand_binop (SImode, ashl_optab, shift, GEN_INT (3),
			shift, 1, OPTAB_LIB_WIDEN);
  *pshift = shift;

  /* Mask for insertion.  */
  mask = expand_binop (SImode, ashl_optab, GEN_INT (GET_MODE_MASK (mode)),
		       shift, NULL_RTX, 1, OPTAB_LIB_WIDEN);
  *pmask = mask;

  return mem;
}

static void
or1k_finish_atomic_subword (machine_mode mode, rtx o, rtx n, rtx shift)
{
  n = expand_binop (SImode, lshr_optab, n, shift,
		    NULL_RTX, 1, OPTAB_LIB_WIDEN);
  emit_move_insn (o, gen_lowpart (mode, n));
}

/* Expand an atomic compare and swap operation.  */

void
or1k_expand_atomic_compare_and_swap (rtx operands[])
{
  rtx boolval, retval, mem, oldval, newval;
  rtx label1, label2;
  machine_mode mode;
  bool is_weak;

  boolval = operands[0];
  retval = operands[1];
  mem = operands[2];
  oldval = operands[3];
  newval = operands[4];
  is_weak = (INTVAL (operands[5]) != 0);
  mode = GET_MODE (mem);

  if (reg_overlap_mentioned_p (retval, oldval))
    oldval = copy_to_reg (oldval);

  label1 = NULL_RTX;
  if (!is_weak)
    {
      label1 = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());
      emit_label (XEXP (label1, 0));
    }
  label2 = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());

  emit_load_locked (mode, retval, mem);
  emit_compare (EQ, retval, oldval);
  emit_unlikely_jump (EQ, label2);
  emit_store_conditional (mode, mem, newval);

  if (!is_weak)
    emit_unlikely_jump (EQ, label1);
  emit_label (XEXP (label2, 0));

  /* In all cases, SR_F contains 1 on success, and 0 on failure.  */
  emit_insn (gen_sne_sr_f (boolval));
}

void
or1k_expand_atomic_compare_and_swap_qihi (rtx operands[])
{
  rtx boolval, orig_retval, retval, scratch, mem, oldval, newval;
  rtx label1, label2, mask, shift;
  machine_mode mode;
  bool is_weak;

  boolval = operands[0];
  orig_retval = operands[1];
  mem = operands[2];
  oldval = operands[3];
  newval = operands[4];
  is_weak = (INTVAL (operands[5]) != 0);
  mode = GET_MODE (mem);

  mem = or1k_adjust_atomic_subword (mem, &shift, &mask);

  /* Shift and mask OLDVAL and NEWVAL into position with the word.  */
  if (oldval != const0_rtx)
    {
      oldval = convert_modes (SImode, mode, oldval, 1);
      oldval = expand_binop (SImode, ashl_optab, oldval, shift,
			     NULL_RTX, 1, OPTAB_LIB_WIDEN);
    }
  if (newval != const0_rtx)
    {
      newval = convert_modes (SImode, mode, newval, 1);
      newval = expand_binop (SImode, ashl_optab, newval, shift,
			     NULL_RTX, 1, OPTAB_LIB_WIDEN);
    }

  label1 = NULL_RTX;
  if (!is_weak)
    {
      label1 = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());
      emit_label (XEXP (label1, 0));
    }
  label2 = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());

  scratch = gen_reg_rtx (SImode);
  emit_load_locked (SImode, scratch, mem);

  retval = expand_binop (SImode, and_optab, scratch, mask,
			 NULL_RTX, 1, OPTAB_LIB_WIDEN);
  scratch = expand_binop (SImode, xor_optab, scratch, retval,
			  scratch, 1, OPTAB_LIB_WIDEN);

  emit_compare (EQ, retval, oldval);
  emit_unlikely_jump (EQ, label2);

  if (newval != const0_rtx)
    scratch = expand_binop (SImode, ior_optab, scratch, newval,
			    scratch, 1, OPTAB_LIB_WIDEN);

  emit_store_conditional (SImode, mem, scratch);

  if (!is_weak)
    emit_unlikely_jump (EQ, label1);
  emit_label (XEXP (label2, 0));

  or1k_finish_atomic_subword (mode, orig_retval, retval, shift);

  /* In all cases, CR0 contains EQ on success, and NE on failure.  */
  emit_insn (gen_sne_sr_f (boolval));
}

/* Expand an atomic exchange operation.  */

void
or1k_expand_atomic_exchange (rtx operands[])
{
  rtx retval, mem, val, label;
  machine_mode mode;

  retval = operands[0];
  mem = operands[1];
  val = operands[2];
  mode = GET_MODE (mem);

  if (reg_overlap_mentioned_p (retval, val))
    val = copy_to_reg (val);

  label = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());
  emit_label (XEXP (label, 0));

  emit_load_locked (mode, retval, mem);
  emit_store_conditional (mode, mem, val);
  emit_unlikely_jump (EQ, label);
}

void
or1k_expand_atomic_exchange_qihi (rtx operands[])
{
  rtx orig_retval, retval, mem, val, scratch;
  rtx label, mask, shift;
  machine_mode mode;

  orig_retval = operands[0];
  mem = operands[1];
  val = operands[2];
  mode = GET_MODE (mem);

  mem = or1k_adjust_atomic_subword (mem, &shift, &mask);

  /* Shift and mask VAL into position with the word.  */
  if (val != const0_rtx)
    {
      val = convert_modes (SImode, mode, val, 1);
      val = expand_binop (SImode, ashl_optab, val, shift,
		          NULL_RTX, 1, OPTAB_LIB_WIDEN);
    }

  label = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());
  emit_label (XEXP (label, 0));

  scratch = gen_reg_rtx (SImode);
  emit_load_locked (SImode, scratch, mem);

  retval = expand_binop (SImode, and_optab, scratch, mask,
			 NULL_RTX, 1, OPTAB_LIB_WIDEN);
  scratch = expand_binop (SImode, xor_optab, scratch, retval,
			  scratch, 1, OPTAB_LIB_WIDEN);
  if (val != const0_rtx)
    scratch = expand_binop (SImode, ior_optab, scratch, val,
			    scratch, 1, OPTAB_LIB_WIDEN);

  emit_store_conditional (SImode, mem, scratch);
  emit_unlikely_jump (EQ, label);

  or1k_finish_atomic_subword (mode, orig_retval, retval, shift);
}

/* Expand an atomic fetch-and-operate pattern.  CODE is the binary operation
   to perform (with MULT as a stand-in for NAND).  MEM is the memory on which
   to operate.  VAL is the second operand of the binary operator.  BEFORE and
   AFTER are optional locations to return the value of MEM either before of
   after the operation.  */

void
or1k_expand_atomic_op (rtx_code code, rtx mem, rtx val,
		       rtx orig_before, rtx orig_after)
{
  machine_mode mode = GET_MODE (mem);
  rtx before = orig_before, after = orig_after;
  rtx label;

  label = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());
  emit_label (XEXP (label, 0));

  if (before == NULL_RTX)
    before = gen_reg_rtx (mode);

  emit_load_locked (mode, before, mem);

  if (code == MULT)
    {
      after = expand_binop (mode, and_optab, before, val,
			    after, 1, OPTAB_LIB_WIDEN);
      after = expand_unop (mode, one_cmpl_optab, after, after, 1);
    }
  else
    after = expand_simple_binop (mode, code, before, val,
				 after, 1, OPTAB_LIB_WIDEN);

  emit_store_conditional (mode, mem, after);
  emit_unlikely_jump (EQ, label);

  if (orig_before)
    emit_move_insn (orig_before, before);
  if (orig_after)
    emit_move_insn (orig_after, after);
}

void
or1k_expand_atomic_op_qihi (rtx_code code, rtx mem, rtx val,
			    rtx orig_before, rtx orig_after)
{
  machine_mode mode = GET_MODE (mem);
  rtx label, mask, shift, x;
  rtx before, after, scratch;

  mem = or1k_adjust_atomic_subword (mem, &shift, &mask);

  /* Shift and mask VAL into position with the word.  */
  val = convert_modes (SImode, mode, val, 1);
  val = expand_binop (SImode, ashl_optab, val, shift,
		      NULL_RTX, 1, OPTAB_LIB_WIDEN);

  switch (code)
    {
    case IOR:
    case XOR:
      /* We've already zero-extended VAL.  That is sufficient to
	 make certain that it does not affect other bits.  */
      break;

    case AND:
    case MULT: /* NAND */
      /* If we make certain that all of the other bits in VAL are
	 set, that will be sufficient to not affect other bits.  */
      x = expand_unop (SImode, one_cmpl_optab, mask, NULL_RTX, 1);
      val = expand_binop (SImode, ior_optab, val, x,
			  val, 1, OPTAB_LIB_WIDEN);
      break;

    case PLUS:
    case MINUS:
      /* These will all affect bits outside the field and need
	 adjustment via MASK within the loop.  */
      break;

    default:
      gcc_unreachable ();
    }

  label = gen_rtx_LABEL_REF (VOIDmode, gen_label_rtx ());
  emit_label (XEXP (label, 0));

  before = scratch = gen_reg_rtx (SImode);
  emit_load_locked (SImode, before, mem);

  switch (code)
    {
    case IOR:
    case XOR:
    case AND:
      after = expand_simple_binop (SImode, code, before, val,
				   NULL_RTX, 1, OPTAB_LIB_WIDEN);
      scratch = after;
      break;

    case PLUS:
    case MINUS:
      before = expand_binop (SImode, and_optab, scratch, mask,
			     NULL_RTX, 1, OPTAB_LIB_WIDEN);
      scratch = expand_binop (SImode, xor_optab, scratch, before,
			      scratch, 1, OPTAB_LIB_WIDEN);
      after = expand_simple_binop (SImode, code, before, val,
				   NULL_RTX, 1, OPTAB_LIB_WIDEN);
      after = expand_binop (SImode, and_optab, after, mask,
			    after, 1, OPTAB_LIB_WIDEN);
      scratch = expand_binop (SImode, ior_optab, scratch, after,
			      scratch, 1, OPTAB_LIB_WIDEN);
      break;

    case MULT: /* NAND */
      after = expand_binop (SImode, and_optab, before, val,
			    NULL_RTX, 1, OPTAB_LIB_WIDEN);
      after = expand_binop (SImode, xor_optab, after, mask,
			    after, 1, OPTAB_LIB_WIDEN);
      scratch = after;
      break;

    default:
      gcc_unreachable ();
    }

  emit_store_conditional (SImode, mem, scratch);
  emit_unlikely_jump (EQ, label);

  if (orig_before)
    or1k_finish_atomic_subword (mode, orig_before, before, shift);
  if (orig_after)
    or1k_finish_atomic_subword (mode, orig_after, after, shift);
}

/* Implement the TARGET_PRINT_OPERAND_PUNCT_VALID_P hook.  */

static bool
or1k_print_operand_punct_valid_p (unsigned char code)
{
  return code == '(';
}

static void
output_addr_reloc (FILE *stream, rtx x, const char *reloc)
{
  fputs (reloc, stream);
  fputc ('(', stream);
  output_addr_const (stream, x);
  fputc (')', stream);
}

static void
print_lo_sum_reloc (FILE *stream, rtx x)
{
  const char *reloc = "lo";

  if (GET_CODE (x) == CONST)
    x = XEXP (x, 0);
  if (GET_CODE (x) == UNSPEC)
    {
      switch (XINT (x, 1))
	{
	case UNSPEC_GOT:
	  reloc = "got";
	  break;
	case UNSPEC_GOTOFF:
	  reloc = "gotofflo";
	  break;
	case UNSPEC_TPOFF:
	  reloc = "tpofflo";
	  break;
	case UNSPEC_GOTTPOFF:
	  reloc = "gottpofflo";
	  break;
	case UNSPEC_TLSGD:
	  reloc = "tlsgdlo";
	  break;
	default:
	  output_operand_lossage("invalid lo_sum relocation");
	}
      x = XVECEXP (x, 0, 0);
    }
  else
    gcc_assert(!flag_pic);
  output_addr_reloc (stream, x, reloc);
}

static void
print_high_reloc (FILE *stream, rtx x)
{
  const char *reloc = "hi";

  if (GET_CODE (x) == CONST)
    x = XEXP (x, 0);
  if (GET_CODE (x) == UNSPEC)
    {
      switch (XINT (x, 1))
	{
	case UNSPEC_GOTOFF:
	  reloc = "gotoffhi";
	  break;
	case UNSPEC_TPOFF:
	  reloc = "tpoffhi";
	  break;
	case UNSPEC_GOTTPOFF:
	  reloc = "gottpoffhi";
	  break;
	case UNSPEC_TLSGD:
	  reloc = "tlsgdhi";
	  break;
	default:
	  output_operand_lossage("invalid high relocation");
	}
      x = XVECEXP (x, 0, 0);
    }
  else
    gcc_assert(!flag_pic);
  output_addr_reloc (stream, x, reloc);
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

    case 'h':
      print_high_reloc (stream, x);
      break;
    case 'L':
      print_lo_sum_reloc (stream, x);
      break;

    case 'P':
      if (!flag_pic || SYMBOL_REF_LOCAL_P (x))
	output_addr_const (stream, x);
      else
	output_addr_reloc (stream, x, "plt");
      break;

    case 0:
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
or1k_print_operand_address (FILE *stream, machine_mode, rtx addr)
{
  rtx offset;

  switch (GET_CODE (addr))
    {
    case REG:
      putc ('0', stream);
      break;

    case PLUS:
      gcc_assert (REG_P (XEXP (addr, 0)));
      offset = XEXP (addr, 1);
      addr = XEXP (addr, 0);
      output_addr_const (stream, offset);
      break;

    case LO_SUM:
      offset = XEXP (addr, 1);
      addr = XEXP (addr, 0);
      print_lo_sum_reloc (stream, offset);
      break;

    default:
      output_addr_const (stream, addr);
      return;
    }

  fprintf (stream, "(%s)", reg_names[REGNO (addr)]);
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

bool
or1k_regnum_ok_for_base_p (unsigned num, bool strict)
{
  if (strict)
    return (num < FIRST_PSEUDO_REGISTER
	    ? IN_RANGE (num, 0, OR1K_LAST_HGR_REGNUM)
	    : IN_RANGE (reg_renumber[num], 0, OR1K_LAST_HGR_REGNUM));
  else
    return num <= OR1K_LAST_SGR_REGNUM || num >= FIRST_PSEUDO_REGISTER;
}

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
gen_sym_unspec (rtx x, int kind)
{
  return gen_rtx_CONST (Pmode, gen_rtx_UNSPEC (Pmode, gen_rtvec (1, x), kind));
}

static rtx
or1k_legitimize_address_1 (rtx x, rtx scratch)
{
  rtx base, addend, t1, t2;
  enum tls_model tls_kind = TLS_MODEL_NONE;
  bool is_local = true;

  split_const (x, &base, &addend);

  switch (GET_CODE (base))
    {
    case REG:
    case SUBREG:
      break;

    case SYMBOL_REF:
      tls_kind = SYMBOL_REF_TLS_MODEL (base);
      is_local = SYMBOL_REF_LOCAL_P (base);
      /* FALLTHRU */

    case LABEL_REF:
      switch (tls_kind)
	{
	case TLS_MODEL_NONE:
	  t1 = can_create_pseudo_p () ? gen_reg_rtx (Pmode) : scratch;
	  if (!flag_pic)
	    {
	      emit_insn (gen_rtx_SET (t1, gen_rtx_HIGH (Pmode, x)));
	      emit_insn (gen_rtx_SET (t1, gen_rtx_LO_SUM (Pmode, t1, x)));
	      return t1;
	    }
	  else if (is_local)
	    {
	      crtl->uses_pic_offset_table = 1;
	      t2 = gen_sym_unspec (x, UNSPEC_GOTOFF);
	      emit_insn (gen_rtx_SET (t1, gen_rtx_HIGH (Pmode, t2)));
	      t2 = gen_sym_unspec (x, UNSPEC_GOTOFF);
	      emit_insn (gen_rtx_SET (t1, gen_rtx_LO_SUM (Pmode, t1, t2)));
	      emit_insn (gen_add3_insn (t1, pic_offset_table_rtx, t1));
	      return t1;
	    }
	  else
	    {
	      crtl->uses_pic_offset_table = 1;
	      base = gen_sym_unspec (base, UNSPEC_GOT);
	      t2 = gen_rtx_LO_SUM (Pmode, pic_offset_table_rtx, base);
	      t2 = gen_const_mem (Pmode, t2);
	      emit_insn (gen_rtx_SET (t1, t2));
	      base = t1;
	    }
	  break;

	case TLS_MODEL_GLOBAL_DYNAMIC:
	case TLS_MODEL_LOCAL_DYNAMIC:
	  /* TODO: For now, treat LD as GD */
	  crtl->uses_pic_offset_table = 1;
	  t1 = gen_reg_rtx (Pmode);
	  t2 = gen_sym_unspec (base, UNSPEC_TLSGD);
	  emit_insn (gen_rtx_SET (t1, gen_rtx_HIGH (Pmode, t2)));
	  t2 = gen_sym_unspec (base, UNSPEC_TLSGD);
	  emit_insn (gen_rtx_SET (t1, gen_rtx_LO_SUM (Pmode, t1, t2)));
	  emit_insn (gen_add3_insn (t1, pic_offset_table_rtx, t1));
	  base = gen_reg_rtx (Pmode);
	  or1k_tls_call (base, t1);
	  break;

	case TLS_MODEL_INITIAL_EXEC:
	  crtl->uses_pic_offset_table = 1;
	  t1 = gen_reg_rtx (Pmode);
	  t2 = gen_sym_unspec (base, UNSPEC_GOTTPOFF);
	  emit_insn (gen_rtx_SET (t1, gen_rtx_HIGH (Pmode, t2)));
	  t2 = gen_sym_unspec (base, UNSPEC_GOTTPOFF);
	  emit_insn (gen_rtx_SET (t1, gen_rtx_LO_SUM (Pmode, t1, t2)));
	  emit_insn (gen_add3_insn (t1, pic_offset_table_rtx, t1));
	  t2 = force_reg (Pmode, gen_const_mem (Pmode, t1));
	  t1 = gen_rtx_REG (Pmode, THREAD_PTR_REGNUM);
	  base = gen_reg_rtx (Pmode);
	  emit_insn (gen_add3_insn (base, t1, t2));
	  break;

	case TLS_MODEL_LOCAL_EXEC:
	  t1 = gen_reg_rtx (Pmode);
	  t2 = gen_sym_unspec (x, UNSPEC_TPOFF);
	  emit_insn (gen_rtx_SET (t1, gen_rtx_HIGH (Pmode, t2)));
	  t2 = gen_sym_unspec (x, UNSPEC_TPOFF);
	  emit_insn (gen_rtx_SET (t1, gen_rtx_LO_SUM (Pmode, t1, t2)));
	  t2 = gen_rtx_REG (Pmode, THREAD_PTR_REGNUM);
	  emit_insn (gen_add3_insn (t1, t1, t2));
	  return t1;

	default:
	  gcc_unreachable ();
	}
      break;

    case UNSPEC:
      /* Re-recognize what we've already emitted.  The CONST will
	 have been stripped by split_const.  */
      gcc_assert (GET_CODE (x) == CONST);
      gcc_assert (base == XEXP (x, 0));
      switch (XINT (base, 1))
	{
	case UNSPEC_GOTOFF:
	case UNSPEC_TPOFF:
	case UNSPEC_GOTTPOFF:
	case UNSPEC_TLSGD:
	  t1 = can_create_pseudo_p () ? gen_reg_rtx (Pmode) : scratch;
	  emit_insn (gen_rtx_SET (t1, gen_rtx_HIGH (Pmode, copy_rtx (x))));
	  emit_insn (gen_rtx_SET (t1, gen_rtx_LO_SUM (Pmode, t1, x)));
	  return t1;

	case UNSPEC_GOT:
	default:
	  gcc_unreachable ();
	};
      break;

    default:
      gcc_assert (can_create_pseudo_p ());
      base = force_reg (Pmode, base);
      break;
    }

  /* If we get here, we still have addend outstanding.  */
  gcc_checking_assert (register_operand (base, Pmode));
  if (addend == const0_rtx)
    return base;
  if (!satisfies_constraint_I (addend))
    {
      HOST_WIDE_INT i = INTVAL (addend);
      HOST_WIDE_INT l = sext_hwi (i, 16);

      t1 = can_create_pseudo_p () ? gen_reg_rtx (Pmode) : scratch;
      t2 = can_create_pseudo_p () ? gen_reg_rtx (Pmode) : scratch;
      emit_move_insn (t1, gen_int_mode (i - l, Pmode));
      emit_insn (gen_add3_insn (t2, base, t1));
      return plus_constant (Pmode, t2, l);
    }
  return gen_rtx_PLUS (Pmode, base, addend);
}

static rtx
or1k_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED,
                         enum machine_mode mode ATTRIBUTE_UNUSED)
{
  return or1k_legitimize_address_1 (x, NULL_RTX);
}

/* In the name of slightly smaller debug output, and to cater to
   general assembler lossage, recognize PIC+GOTOFF and turn it back
   into a direct symbol reference.  */

static rtx
or1k_delegitimize_address (rtx x)
{
  if (GET_CODE (x) == CONST)
    {
      /* The LO_SUM to which X was attached has been stripped.
	 Since the only legitimate address we could have been computing
	 is that of the symbol, assume that's what we've done.  */
      rtx inner = XEXP (x, 0);
      if (GET_CODE (inner) == UNSPEC
	  && XINT (inner, 1) == UNSPEC_GOTOFF)
	return XVECEXP (inner, 0, 0);
    }
  else if (MEM_P (x))
    {
      rtx addr = XEXP (x, 0);
      if (GET_CODE (addr) == LO_SUM
	  && XEXP (addr, 0) == pic_offset_table_rtx)
	{
	  rtx inner = XEXP (addr, 1);
	  if (GET_CODE (inner) == UNSPEC
	      && XINT (inner, 1) == UNSPEC_GOT)
	    return XVECEXP (inner, 0, 0);
	}
    }
  return delegitimize_mem_from_attrs (x);
}

/* Primarily this is required for TLS symbols, but given that our move
   patterns *ought* to be able to handle any symbol at any time, we
   should never be spilling symbolic operands to the constant pool, ever.  */

static bool
or1k_cannot_force_const_mem (enum machine_mode mode ATTRIBUTE_UNUSED, rtx x)
{
  enum rtx_code code = GET_CODE (x);
  return (code == SYMBOL_REF
	  || code == LABEL_REF
	  || code == CONST
	  || code == HIGH);
}

void
or1k_expand_move (machine_mode mode, rtx op0, rtx op1)
{
  if (MEM_P (op0))
    {
      if (!reg_or_0_operand (op1, mode))
	op1 = force_reg (mode, op1);
    }
  else if (mode == QImode || mode == HImode)
    {
      if (can_create_pseudo_p() && optimize > 0)
	{
	  if (CONST_INT_P (op1))
	    {
	      rtx reg = gen_reg_rtx (SImode);
	      emit_insn (gen_rtx_SET (reg, op1));
	      op1 = gen_lowpart (mode, reg);
	    }
	  else if (MEM_P (op1))
	    {
	      rtx reg = gen_reg_rtx (SImode);
	      if (mode == QImode)
		emit_insn (gen_zero_extendqisi2 (reg, op1));
	      else
		emit_insn (gen_zero_extendhisi2 (reg, op1));
	      op1 = gen_lowpart (mode, reg);
	    }
	}
    }
  else if (mode == SImode)
    {
      switch (GET_CODE (op1))
	{
	case CONST:
	case SYMBOL_REF:
	case LABEL_REF:
	  op1 = or1k_legitimize_address_1 (op1, op0);
	  if (GET_CODE (op1) == PLUS)
	    {
	      emit_insn (gen_add3_insn (op0, XEXP (op1, 0), XEXP (op1, 1)));
	      return;
	    }
	  break;

	case CONST_INT:
	  if (!input_operand (op1, mode))
	    {
	      rtx sub = can_create_pseudo_p () ? gen_reg_rtx (mode) : op0;
	      HOST_WIDE_INT i = INTVAL (op1);
	      HOST_WIDE_INT l = i & 0xffff;

	      emit_insn (gen_rtx_SET (sub, gen_int_mode (i - l, mode)));
	      emit_insn (gen_rtx_SET (op0, gen_rtx_IOR (mode, sub,
							GEN_INT (l))));
	      return;
	    }
	  break;

	default:
	  break;
	}
    }
  emit_insn (gen_rtx_SET (op0, op1));
}

/* ========================================================================== */
/* Functions to support the Machine Description                               */

void
or1k_expand_call (rtx retval, rtx fnaddr, rtx callarg1)
{
  rtx call, use = NULL;

  /* Calls via the PLT require the PIC register.  */
  if (flag_pic
      && GET_CODE (XEXP (fnaddr, 0)) == SYMBOL_REF
      && !SYMBOL_REF_LOCAL_P (XEXP (fnaddr, 0)))
    {
      crtl->uses_pic_offset_table = 1;
      rtx hard_pic = gen_rtx_REG (Pmode, REAL_PIC_OFFSET_TABLE_REGNUM);
      emit_move_insn (hard_pic, pic_offset_table_rtx);
      use_reg (&use, hard_pic);
    }

  if (!call_insn_operand (XEXP (fnaddr, 0), Pmode))
    {
      fnaddr = copy_to_mode_reg (Pmode, XEXP (fnaddr, 0));
      fnaddr = gen_rtx_MEM (SImode, fnaddr);
    }

  call = gen_rtx_CALL (VOIDmode, fnaddr, callarg1);
  if (retval)
    call = gen_rtx_SET (retval, call);
  call = emit_call_insn (call);

  CALL_INSN_FUNCTION_USAGE (call) = use;
}

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

  for (unsigned regno = 1; regno <= OR1K_LAST_HGR_REGNUM; regno++)
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

  if (flag_stack_usage_info)
    current_function_static_stack_size = frame_info.static_size;

  if (total_size == 0 && save_mask == 0)
    goto fini;

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

 fini:
  /* Fix up, or remove, the insn that initialized the pic register.  */
  rtx_insn *set_got_insn = cfun->machine->set_got_insn;
  if (crtl->uses_pic_offset_table)
    {
      rtx reg = SET_DEST (PATTERN (set_got_insn));
      rtx_insn *insn = emit_insn_before (gen_set_got (reg), set_got_insn);
      RTX_FRAME_RELATED_P (insn) = 1;
      add_reg_note (insn, REG_CFA_FLUSH_QUEUE, NULL_RTX);
    }
  delete_insn (set_got_insn);
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

  for (unsigned int regno = 1; regno <= OR1K_LAST_HGR_REGNUM; regno++)
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
  else if (dwarf)
    {
      insn = get_last_insn ();
      REG_NOTES (insn) = dwarf;
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* Move up to the stack frame of an exception handler.  */
  if (crtl->calls_eh_return)
    emit_insn (gen_add2_insn (stack_pointer_rtx, EH_RETURN_STACKADJ_RTX));

  /* Return instruction emitted in gen_epilogue.  */
}

/* ========================================================================== */
/* Target hook functions.

   These are initialized at the end of this file, to avoid having to
   predeclare all the functions. They are only needed here, so are static.    */

machine_mode
or1k_promote_mode(machine_mode mode, bool, const_tree type)
{
  if ((type ? INTEGRAL_TYPE_P (type) : SCALAR_INT_MODE_P (mode))
      && GET_MODE_SIZE (mode) < UNITS_PER_WORD)
    mode = SImode;
  return mode;
}

static rtx
or1k_libcall_value (machine_mode mode, const_rtx /* func */)
{
  mode = or1k_promote_mode (mode, false, NULL);
  return gen_rtx_REG (mode, GP_ARG_RETURN);
}

static rtx
or1k_function_value (const_tree ret_type, const_tree /* fn_decl_or_type */,
                     bool /* outgoing */)
{
  machine_mode mode = VOIDmode;
  if (ret_type)
    mode = TYPE_MODE (ret_type);
  mode = or1k_promote_mode (mode, false, ret_type);
  return gen_rtx_REG (mode, GP_ARG_RETURN);
}


/* Check if a function is suitable for tail call optimization,
   implementing the TARGET_FUNCTION_OK_FOR_SIBCALL target hook.  */

static bool
or1k_function_ok_for_sibcall (tree decl, tree exp ATTRIBUTE_UNUSED)
{
  /* We can sibcall optimize any indirect function.  */
  if (decl == NULL)
    return true;

  /* We can sibcall to any direct function if not PIC.  */
  if (!flag_pic)
    return true;

  /* If the call may go through the PLT, we need r16 live.  */
  if (!targetm.binds_local_p (decl))
    return false;

  return true;
}

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
or1k_legitimate_address_p (machine_mode, rtx x, bool strict)
{
  rtx base;

  switch (GET_CODE (x))
    {
    case REG:
      base = x;
      break;

    case LO_SUM:
      base = XEXP (x, 0);
      if (!REG_P (base))
	return false;
      x = XEXP (x, 1);
      switch (GET_CODE (x))
	{
	case CONST:
	  x = XEXP (x, 0);
	  if (GET_CODE (x) != UNSPEC)
	    return false;
	  switch (XINT (x, 1))
	    {
	    case UNSPEC_GOT:
	      /* Assume legitimize_address properly categorized the symbol.
		 Continue to check the base.  */
	      break;
	    default:
	      return false;
	    }
	  break;
	default:
	  return false;
	}
      break;

    case PLUS:
      base = XEXP (x, 0);
      if (!REG_P (base))
	return false;
      x = XEXP (x, 1);

      /* Register elimination is going to adjust all of these offsets.
	 We might as well keep them as a unit until then.  */
      if (!strict && (base == arg_pointer_rtx || base == frame_pointer_rtx))
	return CONST_INT_P (x);

      if (!satisfies_constraint_I (x))
	return false;
      break;

    default:
      return false;
    }

  return or1k_regnum_ok_for_base_p (REGNO (base), strict);
}

/* -------------------------------------------------------------------------- */
/*!Initialize a trampoline for nested functions.

   For the OR1K, we choose to re-use the return value (rv) register as
   the static chain.

	l.movhi r13,hi(nested_func)
	l.movhi r11,hi(static_chain)
	l.ori	r13,r13,lo(nested_func)
	l.jr	r13
	 l.ori	r11,r11,lo(static_chain)

   @note For the OR1K we need to flush the instruction cache, which is a
         privileged operation. Needs fixing.

   @param[in] m_tramp      The lowest address of the trampoline on the stack.
   @param[in] fndecl       Declaration of the enclosing function.
   @param[in] chain_value  Static chain pointer to pass to the nested
                           function.                                          */
/* -------------------------------------------------------------------------- */

int
or1k_trampoline_code_size (void)
{
  // Need one more word in TARGET_DELAY_COMPAT mode for l.nop in delay slot.
  int words = 5 + !!TARGET_DELAY_COMPAT;
  return words * 4;
}

static void
or1k_trampoline_init (rtx m_tramp, tree fndecl, rtx chain)
{
  rtx tramp[6], fnaddr, f_hi, f_lo, c_hi, c_lo;
  int n;

  fnaddr = force_reg (SImode, XEXP (DECL_RTL (fndecl), 0));
  f_hi = expand_binop (SImode, lshr_optab, fnaddr, GEN_INT (16),
		       NULL, true, OPTAB_DIRECT);
  f_lo = expand_binop (SImode, and_optab, fnaddr, GEN_INT (0xffff),
		       NULL, true, OPTAB_DIRECT);

  chain = force_operand (chain, NULL);
  c_hi = expand_binop (SImode, lshr_optab, chain, GEN_INT (16),
		       NULL, true, OPTAB_DIRECT);
  c_lo = expand_binop (SImode, and_optab, chain, GEN_INT (0xffff),
		       NULL, true, OPTAB_DIRECT);

  tramp[0] = expand_binop (SImode, ior_optab, f_hi,
			   gen_int_mode (OR1K_MOVHI (13, 0), SImode),
			   f_hi, true, OPTAB_DIRECT);
  tramp[1] = expand_binop (SImode, ior_optab, c_hi,
			   gen_int_mode (OR1K_MOVHI (11, 0), SImode),
			   c_hi, true, OPTAB_DIRECT);
  tramp[2] = expand_binop (SImode, ior_optab, f_lo,
			   gen_int_mode (OR1K_ORI (13, 13, 0), SImode),
			   f_lo, true, OPTAB_DIRECT);
  tramp[3] = expand_binop (SImode, ior_optab, c_lo,
			   gen_int_mode (OR1K_ORI (11, 11, 0), SImode),
			   c_lo, true, OPTAB_DIRECT);
  tramp[4] = gen_int_mode (OR1K_JR (13), SImode);

  n = 5;
  if (TARGET_DELAY_COMPAT)
    tramp[n++] = gen_int_mode (OR1K_NOP, SImode);
  else if (TARGET_DELAY_ON)
    std::swap (tramp[3], tramp[4]);

  /* Copy the trampoline code.  Leave any padding uninitialized.  */
  for (int i = 0; i < n; ++i)
    {
      rtx mem = adjust_address (m_tramp, SImode, i * 4);
      emit_move_insn (mem, tramp[i]);
    }

  /* Flushing the trampoline from the instruction cache needs
     to be done here. */
}

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

#undef  TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE or1k_libcall_value
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

#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_const_tree_true

#undef  TARGET_PROMOTE_FUNCTION_MODE
#define TARGET_PROMOTE_FUNCTION_MODE default_promote_function_mode_always_promote

#undef  TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P  or1k_legitimate_address_p

#undef  TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS or1k_legitimize_address

#undef  TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS or1k_delegitimize_address

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

/* Is this suitable for an immediate operand.  */

static bool
or1k_legitimate_constant_p (machine_mode mode, rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
    case CONST_WIDE_INT:
    case HIGH:
      /* We construct these, rather than spilling to memory.  */
      return true;

    case CONST_DOUBLE:
      /* We construct these, rather than spilling to memory.  */
      return mode == SFmode;

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      /* These may need to be split and not reconstructed.  */
      if (or1k_tls_symbolic_operand (x) != TLS_MODEL_NONE)
	return false;
      return true;

    default:
      return false;
    }
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

static bool
or1k_rtx_costs (rtx x, machine_mode mode, int outer_code,
		int opno ATTRIBUTE_UNUSED, int *total,
		bool speed ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
    case CONST_DOUBLE:
      if (x == CONST0_RTX (mode))
	*total = 0;
      else if ((outer_code == PLUS || outer_code == XOR || outer_code == MULT)
	       && satisfies_constraint_I (x))
	*total = 0;
      else if ((outer_code == AND || outer_code == IOR)
	       && satisfies_constraint_K (x))
	*total = 0;
      else if (satisfies_constraint_I (x)
	       || satisfies_constraint_K (x)
	       || satisfies_constraint_M (x))
	*total = 2;
      else
	*total = COSTS_N_INSNS (2);
      return true;

    case HIGH:
      /* This is effectively an 'M' constraint.  */
      *total = 2;
      return true;

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      if (outer_code == LO_SUM || outer_code == HIGH)
	*total = 0;
      else
	{
	  /* ??? Extra cost for GOT or TLS symbols.  */
	  *total = COSTS_N_INSNS (1 + (outer_code != MEM));
	}
      return true;

    case LO_SUM:
    case PLUS:
      if (outer_code == MEM)
	{
	  *total = 0;
	  return true;
	}
      break;

    default:
      break;
    }
  return false;
}

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS or1k_rtx_costs

/* Create and initialize PIC register if required.  */
static void
or1k_init_pic_reg (void)
{
  start_sequence ();

  cfun->machine->set_got_insn
    = emit_insn (gen_set_got_tmp (pic_offset_table_rtx));

  rtx_insn *seq = get_insns ();
  end_sequence ();

  edge entry_edge = single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun));
  insert_insn_on_edge (seq, entry_edge);
  commit_one_edge_insertion (entry_edge);
}
#undef TARGET_INIT_PIC_REG
#define TARGET_INIT_PIC_REG or1k_init_pic_reg
#undef TARGET_USE_PSEUDO_PIC_REG
#define TARGET_USE_PSEUDO_PIC_REG hook_bool_void_true

#undef TARGET_MIN_ANCHOR_OFFSET
#define TARGET_MIN_ANCHOR_OFFSET  -32768
#undef TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET  32767

/* Initialize the GCC target structure.  */
struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-or1k.h"
