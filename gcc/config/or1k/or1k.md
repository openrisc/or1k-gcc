;; Machine description for OpenRISC
;; Copyright (C) 2018 Free Software Foundation, Inc.
;; Contributed by Stafford Horne

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;; -------------------------------------------------------------------------
;; OpenRISC specific constraints, predicates and attributes
;; -------------------------------------------------------------------------

(include "constraints.md")
(include "predicates.md")

;; Register numbers
(define_constants
  [(SP_REGNUM       1)
   (HFP_REGNUM      2)
   (LR_REGNUM       9)
   (TLS_REGNUM     10)
   (RV_REGNUM      11)
   (PE_TMP_REGNUM  13)
   (AP_REGNUM      32)
   (SFP_REGNUM     33)
   (SR_F_REGNUM    34)]
)

;; Instruction scheduler

; Most instructions are 4 bytes long.
(define_attr "length" "" (const_int 4))

(define_attr "type"
  "alu,st,ld,control"
  (const_string "alu"))

(define_automaton "or1k")
(define_cpu_unit "cpu" "or1k")
(define_insn_reservation "alu" 1
  (eq_attr "type" "alu")
  "cpu")
(define_insn_reservation "st" 1
  (eq_attr "type" "st")
  "cpu")
(define_insn_reservation "ld" 3
  (eq_attr "type" "st")
  "cpu")
(define_insn_reservation "control" 1
  (eq_attr "type" "control")
  "cpu")

; Define delay slots for any branch
(define_delay (eq_attr "type" "control")
  [(eq_attr "type" "!control") (nil) (nil)])

;; -------------------------------------------------------------------------
;; nop instruction
;; -------------------------------------------------------------------------

(define_insn "nop"
  [(const_int 0)]
  ""
  "l.nop")

;; -------------------------------------------------------------------------
;; Arithmetic instructions
;; -------------------------------------------------------------------------

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	  (plus:SI
	   (match_operand:SI 1 "register_operand"   "%r,r")
	   (match_operand:SI 2 "reg_or_s16_operand" " r,I")))]
  ""
  "@
  l.add\t%0, %1, %2
  l.addi\t%0, %1, %2")

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	  (mult:SI
	   (match_operand:SI 1 "register_operand"   "%r,r")
	   (match_operand:SI 2 "reg_or_s16_operand" " r,I")))]
  ""
  "@
  l.mul\t%0, %1, %2
  l.muli\t%0, %1, %2")

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (div:SI
	   (match_operand:SI 1 "register_operand" "r")
	   (match_operand:SI 2 "register_operand" "r")))]
  ""
  "l.div\t%0, %1, %2")

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (udiv:SI
	   (match_operand:SI 1 "register_operand" "r")
	   (match_operand:SI 2 "register_operand" "r")))]
  ""
  "l.divu\t%0, %1, %2")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (minus:SI
	   (match_operand:SI 1 "reg_or_0_operand" "rO")
	   (match_operand:SI 2 "register_operand" "r")))]
  ""
  "l.sub\t%0, %r1, %2")

;; -------------------------------------------------------------------------
;; Logical operators
;; -------------------------------------------------------------------------

(define_code_iterator SHIFT  [ashift ashiftrt lshiftrt rotatert])
(define_code_attr shift_op   [(ashift "ashl") (ashiftrt "ashr")
			      (lshiftrt "lshr") (rotatert "rotr")])
(define_code_attr shift_asm  [(ashift "sll") (ashiftrt "sra")
			      (lshiftrt "srl") (rotatert "ror")])

(define_insn "<shift_op>si3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(SHIFT:SI (match_operand:SI 1 "register_operand"  "r,r")
		  (match_operand:SI 2 "reg_or_u6_operand" "r,n")))]
  ""
  "@
   l.<shift_asm>\t%0, %1, %2
   l.<shift_asm>i\t%0, %1, %2")

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	  (and:SI
	   (match_operand:SI 1 "register_operand"   "%r,r")
	   (match_operand:SI 2 "reg_or_u16_operand" " r,J")))]
  ""
  "@
  l.and\t%0, %1, %2
  l.andi\t%0, %1, %2")

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	  (xor:SI
	   (match_operand:SI 1 "register_operand"   "%r,r")
	   (match_operand:SI 2 "reg_or_s16_operand" " r,I")))]
  ""
  "@
  l.xor\t%0, %1, %2
  l.xori\t%0, %1, %2")

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	  (ior:SI
	   (match_operand:SI 1 "register_operand"   "%r,r")
	   (match_operand:SI 2 "reg_or_u16_operand" " r,J")))]
  ""
  "@
  l.or\t%0, %1, %2
  l.ori\t%0, %1, %2")

(define_expand "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "")
	(xor:SI (match_operand:SI 1 "register_operand" "") (const_int -1)))]
  ""
  "")

;; -------------------------------------------------------------------------
;; Move instructions
;; -------------------------------------------------------------------------

(define_mode_iterator I [QI HI SI])

(define_mode_attr ldst [(QI "b") (HI "h") (SI "w")])

(define_expand "mov<I:mode>"
  [(set (match_operand:I 0 "nonimmediate_operand" "")
	(match_operand:I 1 "general_operand" ""))]
  ""
{
  rtx op0 = operands[0];
  rtx op1 = operands[1];
  rtx subtarget = op0;
  rtx offset = NULL;

  if (MEM_P (op0) && !const0_operand(op1, <MODE>mode))
    operands[1] = op1 = force_reg (<MODE>mode, op1);

  switch (GET_CODE (op1))
    {
    case CONST_INT:
      /* Constants smaller than SImode can be loaded directly.
         Otherwise, check to see if it requires splitting.  */
      if (<MODE>mode == SImode
	  && !satisfies_constraint_I (op1)
	  && !satisfies_constraint_J (op1)
	  && !satisfies_constraint_M (op1))
	{
          HOST_WIDE_INT i = INTVAL (op1);
          HOST_WIDE_INT lo = i & 0xffff;
          HOST_WIDE_INT hi = i ^ lo;

          if (!cse_not_expected && can_create_pseudo_p ())
            subtarget = gen_reg_rtx (SImode);
          emit_move_insn (subtarget, GEN_INT (hi));
	  emit_insn (gen_iorsi3 (op0, subtarget, GEN_INT (lo)));
	  DONE;
	}
      break;

    case CONST:
      if (GET_CODE (XEXP (op1, 0)) == PLUS
          && CONST_INT_P (XEXP (XEXP (op1, 0), 1)))
	{
	  offset = XEXP (XEXP (op1, 0), 1);
          op1 = XEXP (XEXP (op1, 0), 0);

          if (!cse_not_expected && can_create_pseudo_p ())
	    subtarget = gen_reg_rtx (Pmode);
        }
      /* fallthru */

    case SYMBOL_REF:
    case LABEL_REF:
      emit_insn (gen_movsi_high (subtarget, op1));
      emit_insn (gen_movsi_lo_sum (subtarget, subtarget, op1));

      if (offset != NULL)
	{
	  subtarget = expand_simple_binop (Pmode, PLUS, subtarget, offset,
                                           op0, 1, OPTAB_DIRECT);
          if (subtarget != op0)
	    emit_move_insn(op0, subtarget);
	}
      DONE;

    default:
      break;
    }
})

;; 8-bit, 16-bit and 32-bit moves

(define_insn "*mov<I:mode>_internal"
  [(set (match_operand:I 0 "nonimmediate_operand" "=r,r,r,r,m,r")
	(match_operand:I 1 "or1k_mov_operand"      "r,M,J,I,rO,m"))]
  "register_operand (operands[0], <I:MODE>mode)
   || reg_or_0_operand (operands[1], <I:MODE>mode)"
  "@
   l.or\t%0, %1, %1
   l.movhi\t%0, hi(%1)
   l.ori\t%0, r0, %1
   l.xori\t%0, r0, %1
   l.s<I:ldst>\t%0, %r1
   l.l<I:ldst>z\t%0, %1"
  [(set_attr "type" "alu,alu,alu,alu,st,ld")])

;; Hi/Low moves for constant and symbol loading

(define_insn "movsi_high"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand:SI 1 "or1k_hilo_operand" "i")))]
  ""
  "l.movhi\t%0, hi(%1)"
  [(set_attr "type" "alu")])

(define_insn "movsi_lo_sum"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand"  "r")
		   (match_operand:SI 2 "or1k_hilo_operand" "i")))]
  ""
  "l.ori\t%0, %1, lo(%2)"
  [(set_attr "type" "alu")])

;; 64-bit moves
;; ??? The clobber that emit_move_multi_word emits is arguably incorrect.
;; Consider gcc.c-torture/execute/20030222-1.c, where a reg-reg DImode
;; move gets register allocated to a no-op move.  At which point the
;; we actively clobber the input.

(define_expand "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
{
  if (MEM_P (operands[0]) && !const0_operand(operands[1], DImode))
    operands[1] = force_reg (DImode, operands[1]);
})

(define_insn_and_split "*movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,o,r")
	(match_operand:DI 1 "general_operand"      " r,o,rO,n"))]
  "register_operand (operands[0], DImode)
   || reg_or_0_operand (operands[1], DImode)"
  "#"
  ""
  [(const_int 0)]
{
  rtx l0 = operand_subword (operands[0], 0, 0, DImode);
  rtx l1 = operand_subword (operands[1], 0, 0, DImode);
  rtx h0 = operand_subword (operands[0], 1, 0, DImode);
  rtx h1 = operand_subword (operands[1], 1, 0, DImode);

  if (reload_completed && reg_overlap_mentioned_p (l0, h1))
    {
      gcc_assert (!reg_overlap_mentioned_p (h0, l1));
      emit_move_insn (h0, h1);
      emit_move_insn (l0, l1);
    }
  else
    {
      emit_move_insn (l0, l1);
      emit_move_insn (h0, h1);
    }
  DONE;
})

;; -------------------------------------------------------------------------
;; Sign Extending
;; -------------------------------------------------------------------------

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand"                    "=r,r")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   l.exthz\t%0, %1
   l.lhz\t%0, %1")

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand"                    "=r,r")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   l.extbz\t%0, %1
   l.lbz\t%0, %1")

;; Sign extension patterns

;; We can do memory extensions with a single load
(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand"                     "=r,r")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand"  "r,m")))]
  ""
  "@
   l.exths\t%0, %1
   l.lhs\t%0, %1")

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand"                     "=r,r")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand"  "r,m")))]
  ""
  "@
   l.extbs\t%0, %1
   l.lbs\t%0, %1")

;; -------------------------------------------------------------------------
;; Compare instructions
;; -------------------------------------------------------------------------

;; OpenRISC supports these integer comparisons:
;;
;;     l.sfeq[i] - equality, r r or r i
;;     l.sfne[i] - not equal, r r or r i
;;     l.sflt{s,u}[i] - less than, signed or unsigned, r r or r i
;;     l.sfle{s,u}[i] - less than or equal, signed or unsigned, r r or r i
;;     l.sfgt{s,u}[i] - greater than, signed or unsigned, r r or r i
;;     l.sfge{s,u}[i] - greater than or equal, signed or unsigned, r r or r i
;;
;;  EQ,NE,LT,LTU,LE,LEU,GT,GTU,GE,GEU
;;  We try to iterate all of thse
;;

(define_code_iterator intcmpcc [ne eq lt ltu gt gtu ge le geu leu])
(define_code_attr insn [(ne "ne") (eq "eq") (lt "lts") (ltu "ltu")
			(gt "gts") (gtu "gtu") (ge "ges") (le "les")
			(geu "geu") (leu "leu") ])

(define_insn "*sf_insn"
  [(set (reg:BI SR_F_REGNUM)
	(intcmpcc:BI (match_operand:SI 0 "reg_or_0_operand"   "rO,rO")
		     (match_operand:SI 1 "reg_or_s16_operand" "r,I")))]
  ""
  "@
   l.sf<insn>\t%r0, %1
   l.sf<insn>i\t%r0, %1")

;; -------------------------------------------------------------------------
;; Conditional Store instructions
;; -------------------------------------------------------------------------

(define_expand "cstoresi4"
  [(set (match_operand:SI 0 "register_operand" "")
	(if_then_else:SI
	  (match_operator 1 "comparison_operator"
	    [(match_operand:SI 2 "reg_or_0_operand" "")
	     (match_operand:SI 3 "reg_or_s16_operand" "")])
	  (match_dup 0)
	  (const_int 0)))]
  ""
{
  or1k_expand_compare (operands + 1);
  emit_move_insn (operands[0], const1_rtx);
})

(define_expand "mov<I:mode>cc"
  [(set (match_operand:I 0 "register_operand" "")
	(if_then_else:I (match_operand 1 "comparison_operator" "")
	  (match_operand:I 2 "reg_or_0_operand" "")
	  (match_operand:I 3 "reg_or_0_operand" "")))]
  ""
{
  rtx xops[3] = { operands[1], XEXP (operands[1], 0), XEXP (operands[1], 1) };
  or1k_expand_compare (xops);
  operands[1] = xops[0];
})

(define_insn "*cmov<I:mode>_positive"
  [(set (match_operand:I 0 "register_operand" "=r")
	(if_then_else:I (ne (reg:BI SR_F_REGNUM) (const_int 0))
		      (match_operand:I 1 "reg_or_0_operand" "rO")
		      (match_operand:I 2 "reg_or_0_operand" "rO")))]
  ""
  "l.cmov\t%0, %r1, %r2")

(define_insn "*cmov<I:mode>_negative"
  [(set (match_operand:I 0 "register_operand" "=r")
	(if_then_else:I (eq (reg:BI SR_F_REGNUM) (const_int 0))
		      (match_operand:I 1 "reg_or_0_operand" "rO")
		      (match_operand:I 2 "reg_or_0_operand" "rO")))]
  ""
  "l.cmov\t%0, %r2, %r1")

;; -------------------------------------------------------------------------
;; Branch instructions
;; -------------------------------------------------------------------------

(define_insn "*cbranch_positive"
  [(set (pc)
	(if_then_else (ne (reg:BI SR_F_REGNUM) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "l.bf\t%0%#"
  [(set_attr "type" "control")])

(define_insn "*cbranch_negative"
  [(set (pc)
	(if_then_else (eq (reg:BI SR_F_REGNUM) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "l.bnf\t%0%#"
  [(set_attr "type" "control")])

(define_expand "cbranchsi4"
  [(set (pc)
	(if_then_else
	  (match_operator 0 "comparison_operator"
	    [(match_operand:SI 1 "reg_or_0_operand" "")
	     (match_operand:SI 2 "reg_or_s16_operand" "")])
	  (label_ref (match_operand 3 "" ""))
	  (pc)))]
  ""
{
  or1k_expand_compare (operands);
})

;; -------------------------------------------------------------------------
;; Jump instructions
;; -------------------------------------------------------------------------

(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "l.j\t%0%#"
  [(set_attr "type" "control")])

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "l.jr\t%0%#"
  [(set_attr "type" "control")])

;; -------------------------------------------------------------------------
;; Prologue & Epilogue
;; -------------------------------------------------------------------------

(define_expand "prologue"
  [(const_int 1)]
  ""
{
  or1k_expand_prologue ();
  DONE;
})

;; Expand epilogue as RTL
(define_expand "epilogue"
  [(return)]
  ""
{
  or1k_expand_epilogue ();
  emit_jump_insn (gen_simple_return ());
  DONE;
})

(define_expand "sibcall_epilogue"
  [(return)]
  ""
{
  or1k_expand_epilogue ();
  /* Placing a USE of LR here, rather than as a REG_USE on the
     sibcall itself, means that LR is not unnecessarily live
     within the function itself, which would force creation of
     a stack frame.  */
  emit_insn (gen_rtx_USE (VOIDmode, gen_rtx_REG (Pmode, LR_REGNUM)));
  DONE;
})

(define_expand "simple_return"
  [(parallel [(simple_return) (use (match_dup 0))])]
  ""
{
  operands[0] = gen_rtx_REG (Pmode, LR_REGNUM);
})

(define_insn "*simple_return"
  [(simple_return)
   (use (match_operand:SI 0 "register_operand" "r"))]
  ""
  "l.jr\t%0%#"
  [(set_attr "type" "control")])

(define_expand "eh_return"
  [(use (match_operand 0 "general_operand"))]
  ""
{
  or1k_expand_eh_return (operands[0]);
  DONE;
})

;; -------------------------------------------------------------------------
;; Call Instructions
;; -------------------------------------------------------------------------

;; Leave these to last, as the modeless operand for call_value
;; interferes with normal patterns.

(define_expand "call"
  [(call (match_operand 0) (match_operand 1))]
  ""
{
  or1k_expand_call (NULL, operands[0], operands[1], false);
  DONE;
})

(define_expand "sibcall"
  [(call (match_operand 0) (match_operand 1))]
  ""
{
  or1k_expand_call (NULL, operands[0], operands[1], true);
  DONE;
})

(define_expand "call_value"
  [(set (match_operand 0) (call (match_operand 1) (match_operand 2)))]
  ""
{
  or1k_expand_call (operands[0], operands[1], operands[2], false);
  DONE;
})

(define_expand "sibcall_value"
  [(set (match_operand 0) (call (match_operand 1) (match_operand 2)))]
  ""
{
  or1k_expand_call (operands[0], operands[1], operands[2], true);
  DONE;
})

(define_insn "*call"
  [(call (mem:SI (match_operand:SI 0 "call_insn_operand" "r,s"))
	 (match_operand 1))
   (clobber (reg:SI LR_REGNUM))]
  "!SIBLING_CALL_P (insn)"
  "@
   l.jalr\t%0%#
   l.jal\t%0%#"
  [(set_attr "type" "control")])

(define_insn "*sibcall"
  [(call (mem:SI (match_operand:SI 0 "call_insn_operand" "c,s"))
	 (match_operand 1))]
  "SIBLING_CALL_P (insn)"
  "@
   l.jr\t%0%#
   l.j\t%0%#"
  [(set_attr "type" "control")])

(define_insn "*call_value"
  [(set (match_operand 0)
	(call (mem:SI (match_operand:SI 1 "call_insn_operand" "r,s"))
	      (match_operand 2)))
   (clobber (reg:SI LR_REGNUM))]
  "!SIBLING_CALL_P (insn)"
  "@
   l.jalr\t%1%#
   l.jal\t%1%#"
  [(set_attr "type" "control")])

(define_insn "*sibcall_value"
  [(set (match_operand 0)
	(call (mem:SI (match_operand:SI 1 "call_insn_operand" "c,s"))
	      (match_operand 2)))]
  "SIBLING_CALL_P (insn)"
  "@
   l.jr\t%1%#
   l.j\t%1%#"
  [(set_attr "type" "control")])
