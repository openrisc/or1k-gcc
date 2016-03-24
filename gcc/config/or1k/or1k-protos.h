/* Definitions of target machine for GNU compiler, OR1K cpu.

   Copyright (C) 2010 Embecosm Limited

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_OR1K_PROTOS_H
#define GCC_OR1K_PROTOS_H

/* The following are for general support. */
extern bool        or1k_regnum_ok_for_base_p (unsigned, bool);
extern int         or1k_trampoline_code_size (void);
extern void        or1k_output_opcode (FILE *);

/* The following are only needed when handling the machine definition. */
#ifdef RTX_CODE
extern void        or1k_init_expanders (void);
extern void        or1k_expand_call (rtx, rtx, rtx);
extern void        or1k_expand_prologue (void);
extern void        or1k_expand_epilogue (void);
extern bool        or1k_direct_return (void);
extern bool        or1k_expand_move (enum machine_mode mode, rtx operands[]);
extern void        or1k_expand_compare(rtx *operands);
extern void        or1k_emit_set_const32 (rtx  op0,
                                          rtx  op1);
extern bool        or1k_expand_symbol_ref (enum machine_mode mode,
                                           rtx operands[]);

void or1k_expand_atomic_compare_and_swap (rtx operands[]);
void or1k_expand_atomic_compare_and_swap_qihi (rtx operands[]);
void or1k_expand_atomic_exchange (rtx operands[]);
void or1k_expand_atomic_exchange_qihi (rtx operands[]);
void or1k_expand_atomic_op (rtx_code, rtx, rtx, rtx, rtx);
void or1k_expand_atomic_op_qihi (rtx_code, rtx, rtx, rtx, rtx);

#endif

extern int or1k_struct_alignment (tree);
extern int or1k_data_alignment (tree, int);

extern HOST_WIDE_INT or1k_initial_elimination_offset (unsigned, unsigned);
extern void or1k_print_jump_restore (rtx jump_address);
extern rtx or1k_eh_return_handler_rtx (void);
extern rtx or1k_return_addr_rtx (int, rtx);

extern int or1k_legitimate_pic_operand_p (rtx x);

/* For RETURN_ADDR_RTX */
extern rtx get_hard_reg_initial_val (enum machine_mode, unsigned int);

#endif
