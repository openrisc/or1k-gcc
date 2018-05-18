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
  int callee_saved_reg_size;

  /* Number of bytes saved on the stack for local variables.  */
  int local_vars_size;

  /* Number of bytes saved on the stack for outgoing/sub-fucntion args.  */
  int args_size;

  /* The sum of sizes: locals vars, called saved regs, stack pointer
   * and an optional frame pointer.
   * Used in expand_prologue () and expand_epilogue().  */
  int total_size;
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

static bool
callee_saved_regno_p (int regno)
{
  /* If we are already saving the frame pointer don't save it 2 times.  */
  if (frame_pointer_needed && regno == HARD_FRAME_POINTER_REGNUM)
    return false;

  return !call_used_regs[regno];
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
 *
 * */

#define OR1K_STACK_ALIGN(LOC)						\
  (((LOC) + ((STACK_BOUNDARY / BITS_PER_UNIT) - 1))			\
   & ~((STACK_BOUNDARY / BITS_PER_UNIT) - 1))

static void
or1k_compute_frame_layout (void)
{
  /* For aligning the local variables.  */
  int stack_alignment = STACK_BOUNDARY / BITS_PER_UNIT;
  int padding;
  int regno;

  cfun->machine->local_vars_size = OR1K_STACK_ALIGN (get_frame_size ());
  cfun->machine->args_size = OR1K_STACK_ALIGN (crtl->outgoing_args_size);

  /* Save callee-saved registers.  */
  cfun->machine->callee_saved_reg_size = 0;

  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if (df_regs_ever_live_p (regno) && callee_saved_regno_p (regno))
      cfun->machine->callee_saved_reg_size += 4;

  cfun->machine->total_size =
    + cfun->machine->local_vars_size
    + cfun->machine->callee_saved_reg_size
    + cfun->machine->args_size;

  if (frame_pointer_needed)
    cfun->machine->total_size += 4;

  /* Add space for the stack pointer.  */
  cfun->machine->total_size += 4;
}

static void
or1k_save_restore_reg (int regno, int offset, bool save_p)
{
  rtx reg;
  rtx mem;

  reg = gen_rtx_REG (Pmode, regno);
  mem = gen_rtx_MEM (SImode, gen_rtx_PLUS (Pmode, stack_pointer_rtx,
                                           GEN_INT (offset)));
  if (save_p)
    {
      rtx insn = emit_move_insn (mem, reg);
      RTX_FRAME_RELATED_P (insn) = 1;
    }
  else
    emit_move_insn (reg, mem);
}

void
or1k_expand_prologue (void)
{
  int offset;
  int regno;

  offset = -1 * cfun->machine->total_size;

  if (flag_stack_usage_info)
    current_function_static_stack_size = cfun->machine->total_size;

  /* Reserve space for local vars and outgoing args.  */
  offset += cfun->machine->local_vars_size
	 + cfun->machine->args_size;

  /* Save callee-saved registers.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    {
      if (df_regs_ever_live_p (regno) && callee_saved_regno_p (regno))
	{
	  or1k_save_restore_reg (regno, offset, true);

	  offset += 4;
	}
    }

  /* Save and update frame pointer.  */
  if (frame_pointer_needed)
    {
      gcc_assert (offset == -8);
      or1k_save_restore_reg (HARD_FRAME_POINTER_REGNUM, offset, true);
      emit_move_insn (hard_frame_pointer_rtx, stack_pointer_rtx);
      offset += 4;
    }

  /* Save the link register.  */
  gcc_assert (offset == -4);
  or1k_save_restore_reg (LR_REGNUM, offset, true);

  /* Finally, adjust the stack pointer.  */
  offset = -1 * cfun->machine->total_size;
  if (offset != 0)
    {
      rtx insn = emit_insn (gen_addsi3 (stack_pointer_rtx,
					stack_pointer_rtx,
				    	GEN_INT (offset)));
      RTX_FRAME_RELATED_P (insn) = 1;
    }
}

void
or1k_expand_epilogue (void)
{
  int regno;
  int offset;

  /* First, restore the stack pointer.  */
  offset = -1 * cfun->machine->total_size;

  if (offset != 0)
    emit_insn (gen_addsi3 (stack_pointer_rtx,
			   stack_pointer_rtx,
			   GEN_INT (-1 * offset)));

  /* Reverse space for local vars and args.  */
  offset += cfun->machine->local_vars_size
	 + cfun->machine->args_size;

  /* Restore callee-saved registers.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    {
      if (df_regs_ever_live_p (regno) && callee_saved_regno_p (regno))
	{
	  or1k_save_restore_reg (regno, offset, false);
	  offset += 4;
	}
    }

  /* Restore frame pointer.  */
  if (frame_pointer_needed)
    {
      gcc_assert (offset == -8);
      or1k_save_restore_reg (HARD_FRAME_POINTER_REGNUM, offset, false);
      offset += 4;
    }

  /* Restore link register.  */
  gcc_assert (offset == -4);
  or1k_save_restore_reg (LR_REGNUM, offset, false);

  emit_jump_insn (gen_return_internal (gen_rtx_REG (Pmode, LR_REGNUM)));
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

bool
or1k_legitimate_address_p (machine_mode mode ATTRIBUTE_UNUSED,
			   rtx x, bool strict_p ATTRIBUTE_UNUSED)
{
  if (GET_CODE(x) == PLUS
      && REG_P (XEXP (x, 0))
      && satisfies_constraint_M (XEXP (x, 1)))
    return true;

  if (REG_P (x))
    return true;

  return false;
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
		   bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  if (*cum < 6)
    return gen_rtx_REG (mode, *cum + 3);
  else
    return NULL_RTX;
}

#define OR1K_FUNCTION_ARG_SIZE(MODE, TYPE)	\
  ((MODE) != BLKmode ? GET_MODE_SIZE (MODE)	\
   : (unsigned) int_size_in_bytes (TYPE))

/* Worker function for TARGET_FUNCTION_ARG_ADVANCE.  Update the cumulative
   args to advnaced past the next function argument.  This is not needed
   for arguments passed on the stack.  */

static void
or1k_function_arg_advance (cumulative_args_t cum_v, machine_mode mode,
			   const_tree type, bool named ATTRIBUTE_UNUSED)
{
  CUMULATIVE_ARGS *cum = get_cumulative_args (cum_v);

  *cum += ((3 + OR1K_FUNCTION_ARG_SIZE (mode, type)) / 4);
}

/* worker function for TARGET_RETURN_IN_MEMORY.  What type of args get returned
   in memory?  Any value bigger than 64-bits is returned in memory.  */

static bool
or1k_return_in_memory (const_tree type, const_tree fntype ATTRIBUTE_UNUSED)
{
  const HOST_WIDE_INT size = int_size_in_bytes (type);
  return (size == -1 || size > (2 * UNITS_PER_WORD));
}

/* Worker function for TARGET_PRINT_OPERAND_ADDRESS.  */

static void
or1k_print_operand_address (FILE *file, machine_mode, rtx x)
{
  switch (GET_CODE (x))
    {
    case REG:
      fprintf (file, "0(%s)", reg_names[REGNO (x)]);
      break;

    case PLUS:
      switch (GET_CODE (XEXP (x, 1)))
	{
	case CONST_INT:
	  fprintf (file, "%ld(%s)",
		   INTVAL(XEXP (x, 1)), reg_names[REGNO (XEXP (x, 0))]);
	  break;
	case SYMBOL_REF:
	  output_addr_const (file, XEXP (x, 1));
	  fprintf (file, "(%s)", reg_names[REGNO (XEXP (x, 0))]);
	  break;
	case CONST:
	  {
	    rtx plus = XEXP (XEXP (x, 1), 0);
	    if (GET_CODE (XEXP (plus, 0)) == SYMBOL_REF
		&& CONST_INT_P (XEXP (plus, 1)))
	      {
		output_addr_const(file, XEXP (plus, 0));
		fprintf (file,"+%ld(%s)", INTVAL (XEXP (plus, 1)),
			 reg_names[REGNO (XEXP (x, 0))]);
	      }
	    else
	      abort();
	  }
	  break;
	default:
	  abort();
	}
      break;

    default:
      output_addr_const (file, x);
      break;
    }
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

#undef TARGET_OPTION_OVERRIDE
#define TARGET_OPTION_OVERRIDE or1k_option_override

#undef TARGET_COMPUTE_FRAME_LAYOUT
#define TARGET_COMPUTE_FRAME_LAYOUT or1k_compute_frame_layout

#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P or1k_legitimate_address_p

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
#undef TARGET_MUST_PASS_IN_STACK
#define	TARGET_MUST_PASS_IN_STACK must_pass_in_stack_var_size
#undef TARGET_PASS_BY_REFERENCE
#define	TARGET_PASS_BY_REFERENCE hook_pass_by_reference_must_pass_in_stack

/* Assembly generation.  */
#undef  TARGET_PRINT_OPERAND
#define TARGET_PRINT_OPERAND or1k_print_operand
#undef  TARGET_PRINT_OPERAND_ADDRESS
#define TARGET_PRINT_OPERAND_ADDRESS or1k_print_operand_address


struct gcc_target targetm = TARGET_INITIALIZER;

#include "gt-or1k.h"
