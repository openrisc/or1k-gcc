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

;; Return 1 if OP is the zero constant for MODE.
(define_predicate "const0_operand"
  (and (match_code "const_int,const_double,const_vector")
       (match_test "op == CONST0_RTX (mode)")))

;; Returns true if OP is either the constant zero or a register.
(define_predicate "reg_or_0_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const0_operand")))

(define_predicate "reg_or_s16_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int")
	    (match_test "satisfies_constraint_I (op)"))))

(define_predicate "reg_or_u16_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int")
	    (match_test "satisfies_constraint_K (op)"))))

(define_predicate "shift_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_code "const_int")
	    (match_test "satisfies_constraint_L (op)"))))

(define_predicate "sgeui_operand"
  (match_code "const_int")
{
  HOST_WIDE_INT ival = INTVAL (op);
  return ival != 0 && IN_RANGE (-ival, -32768, 32767);
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

(define_predicate "call_insn_operand"
  (match_code "reg,subreg,symbol_ref"))

;; True iff OP is a symbolic operand.

(define_predicate "symbolic_operand"
  (match_code "symbol_ref,label_ref,const")
{
  switch (GET_CODE (op))
    {
    case SYMBOL_REF:
      return !SYMBOL_REF_TLS_MODEL (op);
    case LABEL_REF:
      return true;
    case CONST:
      op = XEXP (op, 0);
      return (GET_CODE (op) == PLUS
	      && ((GET_CODE (XEXP (op, 0)) == SYMBOL_REF
		   && !SYMBOL_REF_TLS_MODEL (XEXP (op, 0)))
		  || GET_CODE (XEXP (op, 0)) == LABEL_REF)
	      && GET_CODE (XEXP (op, 1)) == CONST_INT);
    default:
      break;
    }
  return false;
})

;; Return true if OP is a symbolic operand for the TLS Global Dynamic model.
(define_predicate "tgd_symbolic_operand"
  (and (match_code "symbol_ref")
       (match_test "SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_GLOBAL_DYNAMIC")))

;; Return true if OP is a symbolic operand for the TLS Local Dynamic model.

(define_predicate "tld_symbolic_operand"
  (and (match_code "symbol_ref")
       (match_test "SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_LOCAL_DYNAMIC")))

;; Return true if OP is a symbolic operand for the TLS Initial Exec model.

(define_predicate "tie_symbolic_operand"
  (and (match_code "symbol_ref")
       (match_test "SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_INITIAL_EXEC")))

;; Return true if OP is a symbolic operand for the TLS Local Exec model.

(define_predicate "tle_symbolic_operand"
  (and (match_code "symbol_ref")
       (match_test "SYMBOL_REF_TLS_MODEL (op) == TLS_MODEL_LOCAL_EXEC")))

(define_predicate "equality_comparison_operator"
  (match_code "eq,ne"))
