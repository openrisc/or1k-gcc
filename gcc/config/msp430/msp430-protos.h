/* Exported function prototypes from the TI MSP430 backend.
   Copyright (C) 2012-2014 Free Software Foundation, Inc.
   Contributed by Red Hat.

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

#ifndef GCC_MSP430_PROTOS_H
#define GCC_MSP430_PROTOS_H

rtx	msp430_eh_return_stackadj_rtx (void);
void	msp430_expand_eh_return (rtx);
void	msp430_expand_epilogue (int);
void	msp430_expand_helper (rtx *operands, const char *, bool);
void	msp430_expand_prologue (void);
const char * msp430x_extendhisi (rtx *);
void	msp430_fixup_compare_operands (enum machine_mode, rtx *);
int	msp430_hard_regno_mode_ok (int, enum machine_mode);
int	msp430_hard_regno_nregs (int, enum machine_mode);
bool    msp430_hwmult_enabled (void);
rtx	msp430_incoming_return_addr_rtx (void);
void	msp430_init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx, tree, int);
int	msp430_initial_elimination_offset (int, int);
bool    msp430_is_f5_mcu (void);
bool    msp430_is_interrupt_func (void);
const char * msp430x_logical_shift_right (rtx);
const char * msp430_mcu_name (void);
bool	msp430_modes_tieable_p (enum machine_mode, enum machine_mode);
void	msp430_output_labelref (FILE *, const char *);
void	msp430_register_pragmas (void);
rtx	msp430_return_addr_rtx (int);
void	msp430_split_movsi (rtx *);
void    msp430_start_function (FILE *, const char *, tree);
rtx	msp430_subreg (enum machine_mode, rtx, enum machine_mode, int);

#endif /* GCC_MSP430_PROTOS_H */
