/* Target Code for OpenRISC
   Copyright (C) 2018 Free Software Foundation, Inc.
   Contributed by Stafford Horne based on other ports.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "df.h"
#include "regs.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "diagnostic-core.h"
#include "output.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"
#include "expr.h"
#include "builtins.h"
#include "optabs.h"
#include "explow.h"
#include "cfgrtl.h"

/* These 4 are needed to allow using satisfies_constraint_J.  */
#include "insn-config.h"
#include "recog.h"
#include "tm_p.h"
#include "tm-constrs.h"

/* This file should be included last.  */
#include "target-def.h"

/* Per-function machine data.  */
struct GTY(()) machine_function
{
  /* Number of bytes saved on the stack for callee saved registers.  */
  HOST_WIDE_INT callee_saved_reg_size;

  /* Number of bytes saved on the stack for local variables.  */
  HOST_WIDE_INT local_vars_size;

  /* Number of bytes saved on the stack for outgoing/sub-fucntion args.  */
  HOST_WIDE_INT args_size;

  /* The sum of sizes: locals vars, called saved regs, stack pointer
   * and an optional frame pointer.
   * Used in expand_prologue () and expand_epilogue().  */
  HOST_WIDE_INT total_size;

  /* Remember where the set_got_placeholder is located.  */
  rtx_insn *set_got_insn;
};

/* Zero initialization is OK for all current fields.  */

static struct machine_function *
or1k_init_machine_status (void)
{
  return ggc_cleared_alloc<machine_function> ();
}


/* The TARGET_OPTION_OVERRIDE worker.  */
static void
or1k_option_override (void)
{
  /* Set the per-function-data initializer.  */
  init_machine_status = or1k_init_machine_status;
}

/* Return true if REGNO must be saved for the current function.  */

static bool
callee_saved_regno_p (int regno)
{
  /* Check call-saved registers.  */
  if (!call_used_regs[regno] && df_regs_ever_live_p (regno))
    return true;

  switch (regno)
    {
    case HARD_FRAME_POINTER_REGNUM:
      return frame_pointer_needed;

    case LR_REGNUM:
      /* Always save LR if we are saving HFP, producing a walkable
	 stack chain with -fno-omit-frame-pointer.  */
      return (frame_pointer_needed
	      || !crtl->is_leaf
	      || crtl->uses_pic_offset_table
	      || df_regs_ever_live_p (regno));

    default:
      return false;
    }
}

/* Compute the size of the local area and the size to be adjusted by the
 * prologue and epilogue.  This is now the TARGET_COMPUTE_FRAME_LAYOUT worker.
 *
 * OpenRISC stack grows downwards and contains:
 *
 *  ---- previous frame --------
 *  current func arg[n]
 *  current func arg[0]   <-- r2 [HFP,AP]
 *  ---- current stack frame ---  ^  ---\
 *  return address      r9        |     |
 *  old frame pointer   r2       (+)    |-- machine->total_size
 *  callee saved regs             |     | > machine->callee_saved_reg_size
 *  local variables               |  ---/ > machine->local_vars_size       <-FP
 *  sub function args     <-- r1 [SP]
 *  ----------------------------  |
 *                               (-)
 *         (future)               |
 *                                V
 *
 * All of these contents are optional.
 */

static void
or1k_compute_frame_layout (void)
{
  HOST_WIDE_INT local_vars_size, args_size, save_reg_size;

  local_vars_size = get_frame_size ();
  local_vars_size = ROUND_UP (local_vars_size, UNITS_PER_WORD);

  args_size = crtl->outgoing_args_size;
  args_size = ROUND_UP (args_size, UNITS_PER_WORD);

  save_reg_size = 0;
  for (int regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (callee_saved_regno_p (regno))
      save_reg_size += UNITS_PER_WORD;

  cfun->machine->local_vars_size = local_vars_size;
  cfun->machine->args_size = args_size;
  cfun->machine->callee_saved_reg_size = save_reg_size;
  cfun->machine->total_size = save_reg_size + local_vars_size + args_size;
}

static void
or1k_save_reg (int regno, HOST_WIDE_INT offset)
{
  rtx reg = gen_rtx_REG (Pmode, regno);
  rtx mem = gen_frame_mem (SImode, plus_constant (Pmode, stack_pointer_rtx,
						  offset));
  rtx insn = emit_move_insn (mem, reg);
  RTX_FRAME_RELATED_P (insn) = 1;
}

static rtx
or1k_restore_reg (int regno, HOST_WIDE_INT offset, rtx cfa_restores)
{
  rtx reg = gen_rtx_REG (Pmode, regno);
  rtx mem = gen_frame_mem (SImode, plus_constant (Pmode, stack_pointer_rtx,
						  offset));
  emit_move_insn (reg, mem);
  return alloc_reg_note (REG_CFA_RESTORE, reg, cfa_restores);
}

void
or1k_expand_prologue (void)
{
  HOST_WIDE_INT sp_offset = -cfun->machine->total_size;
  HOST_WIDE_INT reg_offset, this_offset;
  rtx insn;

  if (flag_stack_usage_info)
    current_function_static_stack_size = -sp_offset;

  /* Early exit for frameless functions.  */
  if (sp_offset == 0)
    goto fini;

  /* Adjust the stack pointer.  For large stack offsets we will
     do this in multiple parts, before and after saving registers.  */
  reg_offset = (sp_offset + cfun->machine->local_vars_size
		+ cfun->machine->args_size);
  this_offset = MAX (sp_offset, -32764);
  reg_offset -= this_offset;
  sp_offset -= this_offset;

  insn = emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
				GEN_INT (this_offset)));
  RTX_FRAME_RELATED_P (insn) = 1;

  /* Save callee-saved registers.  */
  for (int regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (regno != HARD_FRAME_POINTER_REGNUM
        && regno != LR_REGNUM
        && callee_saved_regno_p (regno))
      {
	or1k_save_reg (regno, reg_offset);
	reg_offset += UNITS_PER_WORD;
      }

  /* Save and update frame pointer.  */
  if (callee_saved_regno_p (HARD_FRAME_POINTER_REGNUM))
    {
      or1k_save_reg (HARD_FRAME_POINTER_REGNUM, reg_offset);
      if (frame_pointer_needed)
	{
	  insn = emit_insn (gen_addsi3 (hard_frame_pointer_rtx,
					stack_pointer_rtx,
					GEN_INT (-this_offset)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      reg_offset += UNITS_PER_WORD;
    }

  /* Save the link register.  */
  if (callee_saved_regno_p (LR_REGNUM))
    {
      or1k_save_reg (LR_REGNUM, reg_offset);
      reg_offset += UNITS_PER_WORD;
    }
  gcc_assert (reg_offset + this_offset == 0);

  /* Allocate the rest of the stack frame, if any.  */
  if (sp_offset != 0)
    {
      if (sp_offset < 2 * -32768)
	{
          /* For very large offsets, we need a temporary register.  */
	  rtx tmp = gen_rtx_REG (Pmode, PE_TMP_REGNUM);
	  emit_move_insn (tmp, GEN_INT (sp_offset));
	  insn = emit_insn (gen_addsi3 (stack_pointer_rtx,
					stack_pointer_rtx, tmp));
	  if (!frame_pointer_needed)
	    {
	      RTX_FRAME_RELATED_P (insn) = 1;
	      add_reg_note (insn, REG_CFA_ADJUST_CFA,
			    gen_rtx_SET (stack_pointer_rtx,
					 plus_constant (Pmode,
							stack_pointer_rtx,
							sp_offset)));
	    }
	}
      else
	{
	  /* Otherwise, emit one or two sequential subtracts.  */
	  do
	    {
	      this_offset = MAX (sp_offset, -32768);
	      sp_offset -= this_offset;

	      insn = emit_insn (gen_addsi3 (stack_pointer_rtx,
					    stack_pointer_rtx,
					    GEN_INT (this_offset)));
	      if (!frame_pointer_needed)
		RTX_FRAME_RELATED_P (insn) = 1;
	    }
	  while (sp_offset != 0);
	}
    }

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

void
or1k_expand_epilogue (void)
{
  HOST_WIDE_INT reg_offset, sp_offset;
  rtx insn, cfa_restores = NULL;

  sp_offset = cfun->machine->total_size;
  if (sp_offset == 0)
    return;

  reg_offset = cfun->machine->local_vars_size + cfun->machine->args_size;

  if (sp_offset >= 32768 || cfun->calls_alloca)
    {
      /* The saved registers are out of range of the stack pointer.
	 We need to partially deallocate the stack frame now.  */
      if (frame_pointer_needed)
	{
	  /* Reset the stack pointer to the bottom of the saved regs.  */
	  sp_offset -= reg_offset;
	  reg_offset = 0;
	  insn = emit_insn (gen_addsi3 (stack_pointer_rtx,
					hard_frame_pointer_rtx,
					GEN_INT (-sp_offset)));
	  RTX_FRAME_RELATED_P (insn) = 1;
	  add_reg_note (insn, REG_CFA_DEF_CFA,
			plus_constant (Pmode, stack_pointer_rtx, sp_offset));
	}
      else if (sp_offset >= 3 * 32768)
	{
	  /* For very large offsets, we need a temporary register.  */
	  rtx tmp = gen_rtx_REG (Pmode, PE_TMP_REGNUM);
	  emit_move_insn (tmp, GEN_INT (reg_offset));
	  insn = emit_insn (gen_addsi3 (stack_pointer_rtx,
					stack_pointer_rtx, tmp));
	  sp_offset -= reg_offset;
	  reg_offset = 0;
	  RTX_FRAME_RELATED_P (insn) = 1;
	  add_reg_note (insn, REG_CFA_DEF_CFA,
			plus_constant (Pmode, stack_pointer_rtx, sp_offset));
	}
      else
	{
	  /* Otherwise, emit one or two sequential additions.  */
	  do
	    {
	      HOST_WIDE_INT this_offset = MIN (reg_offset, 32764);
	      reg_offset -= this_offset;
	      sp_offset -= this_offset;

	      insn = emit_insn (gen_addsi3 (stack_pointer_rtx,
					    stack_pointer_rtx,
					    GEN_INT (this_offset)));
	      RTX_FRAME_RELATED_P (insn) = 1;
	      add_reg_note (insn, REG_CFA_DEF_CFA,
			    plus_constant (Pmode, stack_pointer_rtx,
					   sp_offset));
	    }
	  while (sp_offset >= 32768);
	}
    }

  /* Restore callee-saved registers.  */
  for (int regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (regno != HARD_FRAME_POINTER_REGNUM
        && regno != LR_REGNUM
        && callee_saved_regno_p (regno))
      {
	cfa_restores = or1k_restore_reg (regno, reg_offset, cfa_restores);
	reg_offset += UNITS_PER_WORD;
      }

  /* Restore frame pointer.  */
  if (callee_saved_regno_p (HARD_FRAME_POINTER_REGNUM))
    {
      cfa_restores = or1k_restore_reg (HARD_FRAME_POINTER_REGNUM,
				       reg_offset, cfa_restores);
      reg_offset += UNITS_PER_WORD;
    }

  /* Restore link register.  */
  if (callee_saved_regno_p (LR_REGNUM))
    {
      cfa_restores = or1k_restore_reg (LR_REGNUM, reg_offset, cfa_restores);
      reg_offset += UNITS_PER_WORD;
    }
  gcc_assert (reg_offset == sp_offset);

  /* Restore stack pointer.  */
  insn = emit_insn (gen_addsi3 (stack_pointer_rtx, stack_pointer_rtx,
				GEN_INT (sp_offset)));
  RTX_FRAME_RELATED_P (insn) = 1;
  REG_NOTES (insn) = cfa_restores;
  add_reg_note (insn, REG_CFA_DEF_CFA, stack_pointer_rtx);
}

/* Worker for TARGET_INIT_PIC_REG.  */

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
#define TARGET_INIT_PIC_REG  or1k_init_pic_reg
#undef TARGET_USE_PSEUDO_PIC_REG
#define TARGET_USE_PSEUDO_PIC_REG  hook_bool_void_true

/* Worker for INITIAL_FRAME_ADDRESS_RTX.  */

rtx
or1k_initial_frame_addr ()
{
  /* Use this to force a stack frame for the current function.  */
  crtl->accesses_prior_frames = 1;
  return arg_pointer_rtx;
}

/* Worker for DYNAMIC_CHAIN_ADDRESS.  */

rtx
or1k_dynamic_chain_addr (rtx frame)
{
  return plus_constant (Pmode, frame, -2 * UNITS_PER_WORD);
}

/* Worker for RETURN_ADDR_RTX.  */

rtx
or1k_return_addr (int, rtx frame)
{
  return gen_frame_mem (Pmode, plus_constant (Pmode, frame, -UNITS_PER_WORD));
}

/* Worker for TARGET_FRAME_POINTER_REQUIRED.  */

static bool
or1k_frame_pointer_required ()
{
  /* ??? While IRA checks accesses_prior_frames, reload does not.
     We do want the frame pointer for this case.  */
  return (crtl->accesses_prior_frames || crtl->profile);
}

/* TODO, do we need to just set to r9? or should we put it to where r9
   is stored on the stack?  */
void
or1k_expand_eh_return (rtx eh_addr)
{
  emit_move_insn (gen_rtx_REG (Pmode, LR_REGNUM), eh_addr);
}

/* We allow the following eliminiations:
    FP -> HARD_FP or SP
    AP -> HARD_FP or SP

  HFP and AP are the same which is handled below.
 */
int
or1k_initial_elimination_offset (int from, int to)
{
  int offset;

  /* Set OFFSET to the offset from the stack pointer.  */
  switch (from)
    {
    /* Incoming args are all the way up at the previous frame.  */
    case ARG_POINTER_REGNUM:
      offset = cfun->machine->total_size;
      break;

    /* Local args, are just past the ougoing args if any.  */
    case FRAME_POINTER_REGNUM:
      offset = cfun->machine->args_size;
      break;

    default:
      gcc_unreachable ();
    }

  if (to == HARD_FRAME_POINTER_REGNUM)
    offset -= cfun->machine->total_size;

  return offset;

}

/* Worker function for TARGET_LEGITIMATE_ADDRESS_P.  */

static bool
or1k_legitimate_address_p (machine_mode, rtx x, bool strict_p)
{
  rtx base, addend;

  switch (GET_CODE (x))
    {
    case REG:
      base = x;
      break;

    case PLUS:
      base = XEXP (x, 0);
      addend = XEXP (x, 1);
      if (!REG_P (base))
	return false;
      /* Register elimination is going to adjust all of these offsets.
	 We might as well keep them as a unit until then.  */
      if (!strict_p && virtual_frame_reg_operand (base, VOIDmode))
	return CONST_INT_P (addend);
      if (!satisfies_constraint_I (addend))
	return false;
      break;

    case LO_SUM:
      base = XEXP (x, 0);
      if (!REG_P (base))
	return false;
      x = XEXP (x, 1);
      switch (GET_CODE (x))
	{
	case CONST:
	case SYMBOL_REF:
	case LABEL_REF:
	  /* Assume legitimize_address properly categorized
	     the symbol.  Continue to check the base.  */
	  break;

	case UNSPEC:
	  switch (XINT (x, 1))
	    {
	    case UNSPEC_GOT:
	    case UNSPEC_GOTOFF:
	    case UNSPEC_TPOFF:
	    case UNSPEC_GOTTPOFF:
	      /* Assume legitimize_address properly categorized
	         the symbol.  Continue to check the base.  */
	      break;
	    default:
	      return false;
	    }
	  break;

	default:
	  return false;
	}
      break;

    default:
      return false;
    }

  unsigned regno = REGNO (base);
  if (regno >= FIRST_PSEUDO_REGISTER)
    {
      if (strict_p)
	regno = reg_renumber[regno];
      else
	return true;
    }
  if (strict_p)
    return regno <= 31;
  else
    return REGNO_OK_FOR_BASE_P (regno);
}

/* Return the TLS type for TLS symbols, 0 otherwise.  */

tls_model
or1k_tls_symbolic_operand (rtx op)
{
  rtx sym, addend;
  split_const (op, &sym, &addend);
  if (SYMBOL_REF_P (sym))
    return SYMBOL_REF_TLS_MODEL (sym);
  return TLS_MODEL_NONE;
}

/* Get a reference to the '__tls_get_addr' symbol.  */

static GTY(()) rtx gen_tls_tga;

static rtx
gen_tls_get_addr (void)
{
  if (!gen_tls_tga)
    gen_tls_tga = init_one_libfunc ("__tls_get_addr");
  return gen_tls_tga;
}

/* Emit a call to '__tls_get_addr'.  */

static void
or1k_tls_call (rtx dest, rtx arg)
{
  emit_library_call_value (gen_tls_get_addr (), dest, LCT_CONST,
			   Pmode, arg, Pmode);
}

/* Helper for or1k_legitimize_address_1.  Wrap X in an unspec.  */
static rtx
gen_sym_unspec (rtx x, int kind)
{
  return gen_rtx_UNSPEC (Pmode, gen_rtvec (1, x), kind);
}

/* Worker for TARGET_LEGITIMIZE_ADDRESS_DISPLACEMENT.
   Split an out-of-range address displacement into hi and lo parts.
   The hi part will have to be loaded into a register separately,
   but the low part will be folded into the memory operand.  */

static bool
or1k_legitimize_address_displacement (rtx *off1, rtx *off2,
				      poly_int64 poly_offset, machine_mode)
{
  HOST_WIDE_INT orig_offset = poly_offset;
  HOST_WIDE_INT lo, hi;

  /* If the displacement is within range of 2 addi insns, prefer that.
     Otherwise split as per normal, at which point the register allocator
     will see that OFF1 is not a valid add3 operand and load it into
     a register, as desired.  */
  if (orig_offset >= 0 && orig_offset < 2 * 32767)
    {
      hi = 32767;
      lo = orig_offset - hi;
    }
  else if (orig_offset < 0 && orig_offset >= 2 * -32768)
    {
      hi = -32768;
      lo = orig_offset - hi;
    }
  else
    {
      lo = sext_hwi (orig_offset, 16);
      hi = orig_offset - lo;
    }

  *off1 = GEN_INT (hi);
  *off2 = GEN_INT (lo);
  return true;
}

#undef  TARGET_LEGITIMIZE_ADDRESS_DISPLACEMENT
#define TARGET_LEGITIMIZE_ADDRESS_DISPLACEMENT \
  or1k_legitimize_address_displacement

/* Worker function for TARGET_LEGITIMIZE_ADDRESS.  */

static rtx
or1k_legitimize_address_1 (rtx x, rtx scratch)
{
  rtx base, addend, t1, t2;
  tls_model tls_kind = TLS_MODEL_NONE;
  bool is_local = true;

  split_const(x, &base, &addend);
  switch (GET_CODE (base))
    {
    default:
      gcc_assert (can_create_pseudo_p ());
      base = force_reg (Pmode, base);
      break;

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
	      return gen_rtx_LO_SUM (Pmode, t1, x);
	    }
	  else if (is_local)
	    {
	      crtl->uses_pic_offset_table = 1;
	      t2 = gen_sym_unspec (x, UNSPEC_GOTOFF);
	      emit_insn (gen_rtx_SET (t1, gen_rtx_HIGH (Pmode, t2)));
	      emit_insn (gen_add3_insn (t1, t1, pic_offset_table_rtx));
	      return gen_rtx_LO_SUM (Pmode, t1, copy_rtx (t2));
	    }
	  else
	    {
	      base = gen_sym_unspec (base, UNSPEC_GOT);
	      crtl->uses_pic_offset_table = 1;
	      t2 = gen_rtx_LO_SUM (Pmode, pic_offset_table_rtx, base);
	      t2 = gen_const_mem (Pmode, t2);
	      emit_insn (gen_rtx_SET (t1, t2));
	      base = t1;
	    }
	  break;

	case TLS_MODEL_GLOBAL_DYNAMIC:
	case TLS_MODEL_LOCAL_DYNAMIC:
	  /* TODO: For now, treat LD as GD.  */
	  t1 = gen_reg_rtx (Pmode);
	  base = gen_sym_unspec (base, UNSPEC_TLSGD);
	  emit_insn (gen_rtx_SET (t1, gen_rtx_HIGH (Pmode, base)));
	  emit_insn (gen_rtx_SET (t1, gen_rtx_LO_SUM (Pmode, t1, base)));
	  crtl->uses_pic_offset_table = 1;
	  emit_insn (gen_add3_insn (t1, t1, pic_offset_table_rtx));
	  base = gen_reg_rtx (Pmode);
	  or1k_tls_call (base, t1);
	  break;

	case TLS_MODEL_INITIAL_EXEC:
	  t1 = gen_reg_rtx (Pmode);
	  t2 = gen_reg_rtx (Pmode);
	  base = gen_sym_unspec (base, UNSPEC_GOTTPOFF);
	  emit_insn (gen_rtx_SET (t1, gen_rtx_HIGH (Pmode, base)));
	  crtl->uses_pic_offset_table = 1;
	  emit_insn (gen_add3_insn (t1, t1, pic_offset_table_rtx));
	  t1 = gen_rtx_LO_SUM (Pmode, t1, base);
	  emit_move_insn (t2, gen_const_mem (Pmode, t1));
	  t1 = gen_rtx_REG (Pmode, TLS_REGNUM);
	  emit_insn (gen_add3_insn (t2, t2, t1));
	  base = t2;
	  break;

	case TLS_MODEL_LOCAL_EXEC:
	  x = gen_sym_unspec (x, UNSPEC_TPOFF);
	  t1 = gen_reg_rtx (Pmode);
	  emit_insn (gen_rtx_SET (t1, gen_rtx_HIGH (Pmode, x)));
	  t2 = gen_rtx_REG (Pmode, TLS_REGNUM);
	  emit_insn (gen_add3_insn (t1, t1, t2));
	  return gen_rtx_LO_SUM (Pmode, t1, x);

	default:
	  gcc_unreachable ();
	}
      break;

    /*
     * Accept what we may have already emitted.
     */

    case LO_SUM:
    case UNSPEC:
      return x;
    }

  /* If we get here, we still have addend outstanding.  */
  gcc_checking_assert (register_operand (base, Pmode));
  if (addend == const0_rtx)
    return base;
  if (satisfies_constraint_I (addend)
      || virtual_frame_reg_operand (base, VOIDmode))
    return gen_rtx_PLUS (Pmode, base, addend);
  else
    {
      rtx hi, lo;
      bool ok = (or1k_legitimize_address_displacement
		 (&hi, &lo, INTVAL (addend), SImode));
      gcc_assert (ok);

      t2 = can_create_pseudo_p () ? gen_reg_rtx (Pmode) : scratch;
      if (satisfies_constraint_I (hi))
	emit_insn (gen_addsi3 (t2, base, hi));
      else
	{
	  t1 = can_create_pseudo_p () ? gen_reg_rtx (Pmode) : scratch;
	  emit_move_insn (t1, hi);
	  emit_insn (gen_add3_insn (t2, base, t1));
	}
      if (lo == const0_rtx)
	return t2;
      else
	return gen_rtx_PLUS (Pmode, t2, lo);
    }
}

static rtx
or1k_legitimize_address (rtx x, rtx oldx ATTRIBUTE_UNUSED, machine_mode)
{
  return or1k_legitimize_address_1 (x, NULL_RTX);
}

#undef  TARGET_LEGITIMIZE_ADDRESS
#define TARGET_LEGITIMIZE_ADDRESS or1k_legitimize_address

/* Worker for TARGET_DELEGITIMIZE_ADDRESS.
   In the name of slightly smaller debug output, and to cater to
   general assembler lossage, recognize PIC+GOTOFF and turn it back
   into a direct symbol reference.  */

static rtx
or1k_delegitimize_address (rtx x)
{
  if (GET_CODE (x) == UNSPEC)
    {
      /* The LO_SUM to which X was attached has been stripped.
	 Since the only legitimate address we could have been computing
	 is that of the symbol, assume that's what we've done.  */
      if (XINT (x, 1) == UNSPEC_GOTOFF)
	return XVECEXP (x, 0, 0);
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

#undef  TARGET_DELEGITIMIZE_ADDRESS
#define TARGET_DELEGITIMIZE_ADDRESS or1k_delegitimize_address

/* Worker for TARGET_CANNOT_FORCE_CONST_MEM.
   Primarily this is required for TLS symbols, but given that our move
   patterns *ought* to be able to handle any symbol at any time, we
   should never be spilling symbolic operands to the constant pool, ever.  */

static bool
or1k_cannot_force_const_mem (machine_mode, rtx x)
{
  rtx_code code = GET_CODE (x);
  return (code == SYMBOL_REF
	  || code == LABEL_REF
	  || code == CONST
	  || code == HIGH);
}

#undef  TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM or1k_cannot_force_const_mem

static bool
or1k_legitimate_constant_p (machine_mode, rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
    case CONST_WIDE_INT:
    case HIGH:
      /* We construct these, rather than spilling to memory.  */
      return true;

    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      /* These may need to be split and not reconstructed.  */
      return or1k_tls_symbolic_operand (x) == TLS_MODEL_NONE;

    default:
      return false;
    }
}

#undef  TARGET_LEGITIMATE_CONSTANT_P
#define TARGET_LEGITIMATE_CONSTANT_P or1k_legitimate_constant_p

/* Worker function for TARGET_PASS_BY_REFERENCE.  */

static bool
or1k_pass_by_reference (cumulative_args_t, machine_mode mode,
			const_tree type, bool)
{
  HOST_WIDE_INT size;
  if (type)
    {
      if (AGGREGATE_TYPE_P (type))
	return true;
      size = int_size_in_bytes (type);
    }
  else
    size = GET_MODE_SIZE (mode);
  return size < 0 || size > 8;
}

/* Worker function for TARGET_FUNCTION_VALUE.  */

static rtx
or1k_function_value (const_tree valtype,
		     const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
		     bool outgoing ATTRIBUTE_UNUSED)
{
  /* TODO support 2 reg return values and return on stack?  */
  return gen_rtx_REG (TYPE_MODE (valtype), RV_REGNUM);
}

/* Worker function for TARGET_LIBCALL_VALUE.  */

static rtx
or1k_libcall_value (machine_mode mode,
		    const_rtx fun ATTRIBUTE_UNUSED)
{
  /* TODO support 2 reg return values and return on stack?  */
  return gen_rtx_REG (mode, RV_REGNUM);
}


/* Worker function for TARGET_FUNCTION_VALUE_REGNO_P.  */

static bool
or1k_function_value_regno_p (const unsigned int regno)
{
  return (regno == RV_REGNUM);
}

/* Worker function for TARGET_FUNCTION_ARG.  Return the next register to be
   used to hold a function argument or NULL_RTX if there's no more space.  */

static rtx
or1k_function_arg (cumulative_args_t cum_v, machine_mode mode,
		   const_tree type ATTRIBUTE_UNUSED,
		   bool named)
{
  /* VOIDmode is passed as a special flag for "last argument".  */
  if (mode == VOIDmode)
    return NULL_RTX;

  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int nreg = CEIL (GET_MODE_SIZE (mode), UNITS_PER_WORD);

  /* Note that all large arguments are passed by reference.  */
  gcc_assert (nreg <= 2);
  if (named && *cum + nreg <= 6)
    return gen_rtx_REG (mode, *cum + 3);
  else
    return NULL_RTX;
}

/* Worker function for TARGET_FUNCTION_ARG_ADVANCE.  Update the cumulative
   args to advnaced past the next function argument.  This is not needed
   for arguments passed on the stack.  */

static void
or1k_function_arg_advance (cumulative_args_t cum_v, machine_mode mode,
			   const_tree type ATTRIBUTE_UNUSED, bool named)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);
  int nreg = CEIL (GET_MODE_SIZE (mode), UNITS_PER_WORD);

  /* Note that all large arguments are passed by reference.  */
  gcc_assert (nreg <= 2);
  if (named)
    *cum += nreg;
}

/* worker function for TARGET_RETURN_IN_MEMORY.  What type of args get returned
   in memory?  Any value bigger than 64-bits is returned in memory.  */

static bool
or1k_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  const HOST_WIDE_INT size = int_size_in_bytes (type);
  return (size == -1 || size > (2 * UNITS_PER_WORD));
}

/* Print reloc(x + add).  */

static void
output_addr_reloc (FILE *stream, rtx x, HOST_WIDE_INT add, const char *reloc)
{
  if (*reloc)
    {
      fputs (reloc, stream);
      fputc ('(', stream);
    }
  output_addr_const (stream, x);
  if (add)
    {
      if (add > 0)
	fputc ('+', stream);
      fprintf (stream, HOST_WIDE_INT_PRINT_DEC, add);
    }
  if (*reloc)
    fputc (')', stream);
}

enum reloc_kind
{
  RKIND_LO,
  RKIND_HI,
  RKIND_MAX
};

enum reloc_type
{
  RTYPE_DIRECT,
  RTYPE_GOT,
  RTYPE_GOTOFF,
  RTYPE_TPOFF,
  RTYPE_GOTTPOFF,
  RTYPE_TLSGD,
  RTYPE_MAX
};

static void
print_reloc (FILE *stream, rtx x, HOST_WIDE_INT add, reloc_kind kind)
{
  /* All data relocations.  A NULL in this table indicates a form that
     we expect to never generate, while "" indicates a form that requires
     no special markup.  */
  static const char * const relocs[RKIND_MAX][RTYPE_MAX] = {
    { "lo", "got", "gotofflo", "tpofflo", "gottpofflo", "tlsgdlo" },
    { "ha", NULL,  "gotoffha", "tpoffha", "gottpoffha", "tlsgdhi" },
  };
  reloc_type type = RTYPE_DIRECT;

  if (GET_CODE (x) == UNSPEC)
    {
      switch (XINT (x, 1))
	{
	case UNSPEC_GOT:
	  type = RTYPE_GOT;
	  break;
	case UNSPEC_GOTOFF:
	  type = RTYPE_GOTOFF;
	  break;
	case UNSPEC_TPOFF:
	  type = RTYPE_TPOFF;
	  break;
	case UNSPEC_GOTTPOFF:
	  type = RTYPE_GOTTPOFF;
	  break;
	case UNSPEC_TLSGD:
	  type = RTYPE_TLSGD;
	  break;
	default:
	  output_operand_lossage("invalid relocation");
	  return;
	}
      x = XVECEXP (x, 0, 0);
    }

  const char *reloc = relocs[kind][type];
  if (reloc == NULL)
    output_operand_lossage("invalid relocation");
  else
    output_addr_reloc (stream, x, add, reloc);
}

/* Worker function for TARGET_PRINT_OPERAND_ADDRESS.  */

static void
or1k_print_operand_address (FILE *file, machine_mode, rtx addr)
{
  rtx offset;

  switch (GET_CODE (addr))
    {
    case REG:
      fputc ('0', file);
      break;

    case PLUS:
      offset = XEXP (addr, 1);
      addr = XEXP (addr, 0);
      gcc_assert (CONST_INT_P (offset));
      if (GET_CODE (addr) == LO_SUM)
	{
	  print_reloc (file, XEXP (addr, 1), INTVAL (offset), RKIND_LO);
	  addr = XEXP (addr, 0);
	}
      else
	output_addr_const (file, offset);
      break;

    case LO_SUM:
      offset = XEXP (addr, 1);
      addr = XEXP (addr, 0);
      print_reloc (file, offset, 0, RKIND_LO);
      break;

    default:
      output_addr_const (file, addr);
      return;
    }

  fprintf (file, "(%s)", reg_names[REGNO (addr)]);
}

/* Worker function for TARGET_PRINT_OPERAND.  */

static void
or1k_print_operand (FILE *file, rtx x, int code)
{
  rtx operand = x;

  switch (code)
    {
    case '#':
      /* Conditionally add a nop in unfilled delay slot.  */
      if (final_sequence == NULL)
	fputs ("\n\t l.nop\n", file);
      break;

    case 'r':
      if (REG_P (x))
        fprintf (file, "%s", reg_names[REGNO (operand)]);
      else if (x == CONST0_RTX (GET_MODE (x)))
        fprintf (file, "r0");
      else
	output_operand_lossage ("invalid %%r value");
      break;

    case 'H':
      print_reloc (file, x, 0, RKIND_HI);
      break;
    case 'L':
      print_reloc (file, x, 0, RKIND_LO);
      break;
    case 'P':
      if (!flag_pic || SYMBOL_REF_LOCAL_P (x))
	output_addr_const (file, x);
      else
	output_addr_reloc (file, x, 0, "plt");
      break;

    case 0:
      /* Print an operand as without a modifier letter.  */
      switch (GET_CODE (operand))
	{
	case REG:
	  if (REGNO (operand) > 31)
	    internal_error ("internal error: bad register: %d",
			    REGNO (operand));
	  fprintf (file, "%s", reg_names[REGNO (operand)]);
	  break;

	case MEM:
	  output_address (GET_MODE (XEXP (operand, 0)), XEXP (operand, 0));
	  break;

	case CODE_LABEL:
	case LABEL_REF:
	  output_asm_label (operand);
	  break;

	default:
	  /* No need to handle all strange variants, let output_addr_const
	     do it for us.  */
	  if (CONSTANT_P (operand))
	    output_addr_const (file, operand);
	  else
	    internal_error ("unexpected operand: %d", GET_CODE (operand));
	  break;
	}
      break;

    default:
      output_operand_lossage ("unknown operand letter: '%c'", code);
      break;
    }
}

/* Worker function for TARGET_TRAMPOLINE_INIT.  */

static void
or1k_trampoline_init (rtx m_tramp, tree fndecl, rtx chain)
{
  const unsigned movhi_r13 = (0x06u << 26) | (13 << 21);
  const unsigned movhi_r11 = (0x06u << 26) | (11 << 21);
  const unsigned ori_r13_r13 = (0x2a << 26) | (13 << 21) | (13 << 16);
  const unsigned ori_r11_r11 = (0x2a << 26) | (11 << 21) | (11 << 16);
  const unsigned jr_r13 = (0x11 << 26) | (13 << 11);
  rtx tramp[5], fnaddr, f_hi, f_lo, c_hi, c_lo;

  fnaddr = force_operand (XEXP (DECL_RTL (fndecl), 0), NULL);
  f_hi = expand_binop (SImode, lshr_optab, fnaddr, GEN_INT (16),
		       NULL, true, OPTAB_DIRECT);
  f_lo = expand_binop (SImode, and_optab, fnaddr, GEN_INT (0xffff),
		       NULL, true, OPTAB_DIRECT);

  chain = force_operand (chain, NULL);
  c_hi = expand_binop (SImode, lshr_optab, chain, GEN_INT (16),
		       NULL, true, OPTAB_DIRECT);
  c_lo = expand_binop (SImode, and_optab, chain, GEN_INT (0xffff),
		       NULL, true, OPTAB_DIRECT);

  /* We want to generate
   *
   *	l.movhi r13,hi(nested_func)
   *	l.movhi r11,hi(static_chain)
   *	l.ori	r13,r13,lo(nested_func)
   *	l.jr	r13
   *	 l.ori	r11,r11,lo(static_chain)
   */
  tramp[0] = expand_binop (SImode, ior_optab, f_hi,
			   gen_int_mode (movhi_r13, SImode),
			   f_hi, true, OPTAB_DIRECT);
  tramp[1] = expand_binop (SImode, ior_optab, c_hi,
			   gen_int_mode (movhi_r11, SImode),
			   c_hi, true, OPTAB_DIRECT);
  tramp[2] = expand_binop (SImode, ior_optab, f_lo,
			   gen_int_mode (ori_r13_r13, SImode),
			   f_lo, true, OPTAB_DIRECT);
  tramp[4] = expand_binop (SImode, ior_optab, c_lo,
			   gen_int_mode (ori_r11_r11, SImode),
			   c_lo, true, OPTAB_DIRECT);
  tramp[3] = gen_int_mode (jr_r13, SImode);

  for (int i = 0; i < 5; ++i)
    {
      rtx mem = adjust_address (m_tramp, SImode, i * 4);
      emit_move_insn (mem, tramp[i]);
    }

  /* Flushing the trampoline from the instruction cache needs
     to be done here. */
}

/* Worker for TARGET_HARD_REGNO_MODE_OK.  */

static bool
or1k_hard_regno_mode_ok (unsigned int regno, machine_mode mode)
{
  /* For OpenRISC, GENERAL_REGS can hold anything, while
     FLAG_REGS are really single bits within SP[SR].  */
  if (REGNO_REG_CLASS (regno) == FLAG_REGS)
    return mode == BImode;
  return true;
}

#undef TARGET_HARD_REGNO_MODE_OK
#define TARGET_HARD_REGNO_MODE_OK or1k_hard_regno_mode_ok

/* Worker for TARGET_CAN_CHANGE_MODE_CLASS.  */

static bool
or1k_can_change_mode_class (machine_mode from, machine_mode to,
			    reg_class_t rclass)
{
  if (rclass == FLAG_REGS)
    return from == to;
  return true;
}

#undef TARGET_CAN_CHANGE_MODE_CLASS
#define TARGET_CAN_CHANGE_MODE_CLASS or1k_can_change_mode_class

void
or1k_expand_move (machine_mode mode, rtx op0, rtx op1)
{
  if (MEM_P (op0))
    {
      if (!const0_operand(op1, mode))
	op1 = force_reg (mode, op1);
    }
  else if (mode == QImode || mode == HImode)
    {
      /* ??? Maybe promote MEMs and CONST_INT to SImode,
	 and then squish back with gen_lowpart.  */
    }
  else
    {
      switch (GET_CODE (op1))
        {
        case CONST_INT:
	  if (!input_operand (op1, mode))
	    {
	      HOST_WIDE_INT i = INTVAL (op1);
	      HOST_WIDE_INT lo = i & 0xffff;
	      HOST_WIDE_INT hi = i ^ lo;
	      rtx subtarget = op0;

	      if (!cse_not_expected && can_create_pseudo_p ())
		subtarget = gen_reg_rtx (SImode);
	      emit_insn (gen_rtx_SET (subtarget, GEN_INT (hi)));
	      emit_insn (gen_iorsi3 (op0, subtarget, GEN_INT (lo)));
	      return;
	    }
	  break;

	case CONST:
	case SYMBOL_REF:
	case LABEL_REF:
	  op1 = or1k_legitimize_address_1 (op1, op0);
	  break;

	default:
	  break;
	}
    }
  emit_insn (gen_rtx_SET (op0, op1));
}

/* Expand a comparison in operands[0] .. operands[2], where
   [0] is the operator and [1],[2] are the operands.  Split out
   the compare into SR[F] and return a new operation in operands[0].  */

void
or1k_expand_compare (rtx *operands)
{
  rtx sr_f = gen_rtx_REG (BImode, SR_F_REGNUM);

  /* Emit the given comparison into the Flag bit.  */
  PUT_MODE (operands[0], BImode);
  emit_insn (gen_rtx_SET (sr_f, operands[0]));

  /* Adjust the operands for use in the caller.  */
  operands[0] = gen_rtx_NE (VOIDmode, sr_f, const0_rtx);
  operands[1] = sr_f;
  operands[2] = const0_rtx;
}

void
or1k_expand_call (rtx retval, rtx fnaddr, rtx callarg1, bool sibcall)
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

  /* Normal calls clobber LR.  This is required in order to
     prevent e.g. a prologue store of LR being placed into
     the delay slot of the call, after it has been updated.  */
  if (!sibcall)
    {
      rtx clob = gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (Pmode, LR_REGNUM));
      call = gen_rtx_PARALLEL (VOIDmode, gen_rtvec (2, call, clob));
    }
  call = emit_call_insn (call);

  CALL_INSN_FUNCTION_USAGE (call) = use;
}

/* Worker for TARGET_FUNCTION_OK_FOR_SIBCALL.  */

static bool
or1k_function_ok_for_sibcall (tree decl ATTRIBUTE_UNUSED,
			      tree exp ATTRIBUTE_UNUSED)
{
  /* We can sibcall to any function if not PIC.  */
  if (!flag_pic)
    return true;

  /* We can sibcall any indirect function.  */
  if (decl == NULL)
    return true;

  /* If the call may go through the PLT, we need r16 live.  */
  return targetm.binds_local_p (decl);
}

#undef TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL or1k_function_ok_for_sibcall

/* Worker for TARGET_RTX_COSTS.  */

static bool
or1k_rtx_costs (rtx x, machine_mode mode, int outer_code,
		int opno ATTRIBUTE_UNUSED, int *total,
		bool speed ATTRIBUTE_UNUSED)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
      if (x == const0_rtx)
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

    case CONST_DOUBLE:
      *total = (x == CONST0_RTX (mode) ? 0 : COSTS_N_INSNS (2));
      return true;

    case HIGH:
      /* This is effectively an 'M' constraint.  */
      *total = 2;
      return true;

    case LO_SUM:
      /* This is effectively an 'I' constraint.  */
      *total = (outer_code == MEM ? 0 : 2);
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

    case PLUS:
      if (outer_code == MEM)
	*total = 0;
      break;

    default:
      break;
    }
  return false;
}

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS or1k_rtx_costs

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE or1k_option_override

#undef TARGET_COMPUTE_FRAME_LAYOUT
#define TARGET_COMPUTE_FRAME_LAYOUT or1k_compute_frame_layout

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P or1k_legitimate_address_p

#undef  TARGET_HAVE_TLS
#define TARGET_HAVE_TLS true

/* Calling Conventions.  */
#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE or1k_function_value
#undef TARGET_LIBCALL_VALUE
#define TARGET_LIBCALL_VALUE or1k_libcall_value
#undef TARGET_FUNCTION_VALUE_REGNO_P
#define TARGET_FUNCTION_VALUE_REGNO_P or1k_function_value_regno_p
#undef TARGET_FUNCTION_ARG
#define TARGET_FUNCTION_ARG or1k_function_arg
#undef TARGET_FUNCTION_ARG_ADVANCE
#define TARGET_FUNCTION_ARG_ADVANCE or1k_function_arg_advance
#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY	or1k_return_in_memory
#undef TARGET_PASS_BY_REFERENCE
#define	TARGET_PASS_BY_REFERENCE or1k_pass_by_reference
#undef TARGET_TRAMPOLINE_INIT
#define TARGET_TRAMPOLINE_INIT or1k_trampoline_init
#undef TARGET_FRAME_POINTER_REQUIRED
#define TARGET_FRAME_POINTER_REQUIRED or1k_frame_pointer_required

/* Assembly generation.  */
#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND or1k_print_operand
#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS or1k_print_operand_address

/* Section anchor support.  */
#undef  TARGET_MIN_ANCHOR_OFFSET
#define TARGET_MIN_ANCHOR_OFFSET  -32768
#undef  TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET  32767

struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-or1k.h"
