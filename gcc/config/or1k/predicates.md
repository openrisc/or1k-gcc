;; Predicate definitions for OR32
;;
;; Copyright (C) 2010 Embecosm Limited
;;
;; This file is part of GCC.
;;
;; GCC is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; GCC is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

(define_predicate "cc_reg_operand"
  (match_code "subreg,reg")
{
  register_operand (op, mode);

  if (GET_CODE (op) == REG && REGNO (op) == CC_REG)
    return 1;

  return 0;
})

(define_predicate "input_operand"
  (match_code "subreg,reg,const_int,mem,const")
{
  /* If both modes are non-void they must be the same.  */
  if (mode != VOIDmode && GET_MODE (op) != VOIDmode && mode != GET_MODE (op))
    return 0;

  /* Allow any one instruction integer constant, and all CONST_INT
     variants when we are working in DImode and !arch64.  */
  if (GET_MODE_CLASS (mode) == MODE_INT
      && ((GET_CODE (op) == CONST_INT)
	  && (satisfies_constraint_K (op)
	      || satisfies_constraint_M (op)
	      || satisfies_constraint_I (op))))
    return 1;

  if (register_operand (op, mode))
    return 1;

  /* If this is a SUBREG, look inside so that we handle
     paradoxical ones.  */
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);


  /* Check for valid MEM forms.  */
  if (GET_CODE (op) == MEM)
    return memory_address_p (mode, XEXP (op, 0));

  return 0;
})

(define_predicate "sym_ref_mem_operand"
  (match_code "mem")
{
  if (GET_CODE (op) == MEM)
    {
      rtx t1 = XEXP (op, 0);
      if (GET_CODE (t1) == SYMBOL_REF)
	return 1;
    }
  return 0;
})
