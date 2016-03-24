;; Machine description for GNU compiler, OpenRISC 1000 family, OR32 ISA
;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008,
;; 2009, 2010 Free Software Foundation, Inc.
;; Copyright (C) 2010 Embecosm Limited

;; Contributed by Damjan Lampret <damjanl@bsemi.com> in 1999.
;; Major optimizations by Matjaz Breskvar <matjazb@bsemi.com> in 2005.
;; Floating point additions by Jungsook Yang <jungsook.yang@uci.edu>
;;                             Julius Baxter <julius@orsoc.se> in 2010
;; Updated for GCC 4.5 by Jeremy Bennett <jeremy.bennett@embecosm.com>
;; and Joern Rennecke <joern.rennecke@embecosm.com> in 2010

;; This file is part of GNU CC.

;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program.  If not, see <http://www.gnu.org/licenses/>. */

(define_constants [
  (SP_REG 1)
  (FP_REG 2) ; hard frame pointer
  (LINK_REGNUM 9)
  (SR_F_REG 34)
])

(define_c_enum "unspec" [
  UNSPEC_GOT
  UNSPEC_GOTOFFHI
  UNSPEC_GOTOFFLO
  UNSPEC_TPOFFLO
  UNSPEC_TPOFFHI
  UNSPEC_GOTTPOFFLO
  UNSPEC_GOTTPOFFHI
  UNSPEC_GOTTPOFFLD
  UNSPEC_TLSGDLO
  UNSPEC_TLSGDHI
  UNSPEC_SET_GOT
  UNSPEC_MSYNC
])

(define_c_enum "unspecv" [
  UNSPECV_LL
  UNSPECV_SC
])

(include "predicates.md")

(include "constraints.md")

(define_attr "type"
  "unknown,load,store,move,extend,logic,add,mul,shift,compare,branch,jump,fp,jump_restore"
  (const_string "unknown"))

;; Number of machine instructions required to implement an insn.
(define_attr "length" "" (const_int 1))

;; Single delay slot after branch or jump instructions, wich may contain any
;; instruction but another branch or jump.
;; If TARGET_DELAY_OFF is not true, then never use delay slots.
;; If TARGET_DELAY_ON is not true, no instruction will be allowed to
;; fill the slot, and so it will be filled by a nop instead.
(define_delay (and (match_test "!TARGET_DELAY_OFF")
		   (eq_attr "type" "branch,jump"))
               [(and (match_test "TARGET_DELAY_ON")
		     (eq_attr "type" "!branch,jump")
		     (eq_attr "length" "1")) (nil) (nil)])

;; ALU is modelled as a single functional unit, which is reserved for varying
;; numbers of slots.
;;
;; I think this is all incorrect for the OR1K. The latency says when the
;; result will be ready, not how long the pipeline takes to execute.
(define_cpu_unit "or1k_alu")
(define_insn_reservation "bit_unit" 3 (eq_attr "type" "shift") "or1k_alu")
(define_insn_reservation "lsu_load" 3 (eq_attr "type" "load") "or1k_alu*3")
(define_insn_reservation "lsu_store" 2 (eq_attr "type" "store") "or1k_alu")
(define_insn_reservation "alu_unit" 2
                         (eq_attr "type" "add,logic,extend,move,compare")
			 "or1k_alu")
(define_insn_reservation "mul_unit" 16 (eq_attr "type" "mul") "or1k_alu*16")

(define_mode_iterator I12 [QI HI])

;; Save registers and allocate the stack frame.
(define_expand "prologue"
  [(use (const_int 1))]
  ""
{
  or1k_expand_prologue ();
  DONE;
})

;; Restore registers, deallocate the stack frame, and return.
(define_expand "epilogue"
  [(const_int 0)]
  ""
{
  or1k_expand_epilogue ();
  emit_jump_insn (gen_return ());
  DONE;
})

;; Similar, but omit the return so that a subsequent "sibcall"
;; pattern can be emitted.
(define_expand "sibcall_epilogue"
  [(const_int 0)]
  ""
{
  or1k_expand_epilogue ();
  emit_insn (gen_rtx_USE (VOIDmode, gen_rtx_REG (Pmode, LINK_REGNUM)));
  DONE;
})

(define_insn "frame_alloc"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI (match_operand:SI 1 "register_operand" "r,r")
		 (match_operand:SI 2 "reg_or_s16_operand" "r,I")))
   (clobber (mem:BLK (match_scratch:SI 3 "=X,X")))]
  ""
  "@
   l.add\t%0,%1,%2
   l.addi\t%0,%1,%2"
  [(set_attr "type" "add")])

(define_expand "return"
  [(parallel [(return)
	      (use (reg:SI LINK_REGNUM))])]
  "or1k_direct_return ()"
  "")

(define_insn "*return"
  [(return)
   (use (match_operand 0 "pmode_register_operand" ""))]
  ""
  "l.jr\t%0%("
  [(set_attr "type" "jump")])

(define_expand "simple_return"
  [(parallel [(simple_return)
	      (use (reg:SI LINK_REGNUM))])]
  "")

(define_insn "*simple_return"
  [(simple_return)
   (use (match_operand 0 "pmode_register_operand" ""))]
  ""
  "l.jr\t%0%("
  [(set_attr "type" "jump")])


;;
;; movQI
;;

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
      if (can_create_pseudo_p())
        {
          if (GET_CODE (operands[1]) == CONST_INT)
	    {
	      rtx reg = gen_reg_rtx (SImode);

	      emit_insn (gen_movsi (reg, operands[1]));
	      operands[1] = gen_lowpart (QImode, reg);
	    }
	  if (GET_CODE (operands[1]) == MEM && optimize > 0)
	    {
	      rtx reg = gen_reg_rtx (SImode);

	      emit_insn (gen_rtx_SET (reg,
				      gen_rtx_ZERO_EXTEND (SImode,
						           operands[1])));

	      operands[1] = gen_lowpart (QImode, reg);
	    }
          if (GET_CODE (operands[0]) != REG)
	    operands[1] = force_reg (QImode, operands[1]);
        }
")

(define_insn "*movqi_internal"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=m,r,r,r,r")
	(match_operand:QI 1 "general_operand"       "r,r,I,K,m"))]
  ""
  "@
   l.sb\t%0,%1
   l.ori\t%0,%1,0
   l.addi\t%0,r0,%1
   l.ori\t%0,r0,%1
   l.lbz\t%0,%1"
  [(set_attr "type" "store,add,add,logic,load")])


;;
;; movHI
;;

(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
      if (can_create_pseudo_p())
        {
          if (GET_CODE (operands[1]) == CONST_INT)
	    {
	      rtx reg = gen_reg_rtx (SImode);

	      emit_insn (gen_movsi (reg, operands[1]));
	      operands[1] = gen_lowpart (HImode, reg);
	    }
	  else if (GET_CODE (operands[1]) == MEM && optimize > 0)
	    {
	      rtx reg = gen_reg_rtx (SImode);

	      emit_insn (gen_rtx_SET (reg,
				      gen_rtx_ZERO_EXTEND (SImode,
					   	           operands[1])));
	      operands[1] = gen_lowpart (HImode, reg);
	    }
          if (GET_CODE (operands[0]) != REG)
	    operands[1] = force_reg (HImode, operands[1]);
        }
")

(define_insn "*movhi_internal"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=m,r,r,r,r")
	(match_operand:HI 1 "general_operand"       "r,r,I,K,m"))]
  ""
  "@
   l.sh\t%0,%1
   l.ori\t%0,%1,0
   l.addi\t%0,r0,%1
   l.ori\t%0,r0,%1
   l.lhz\t%0,%1"
  [(set_attr "type" "store,add,add,logic,load")])

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
{
  if (or1k_expand_move (SImode, operands)) DONE;
})

;;
;; movSI
;;

(define_insn "*movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,r,r,m")
        (match_operand:SI 1 "input_operand"       "I,K,M,r,m,r"))]
  "(register_operand (operands[0], SImode)
   || (register_operand (operands[1], SImode))
   || (operands[1] == const0_rtx))"
  "@
   l.addi\t%0,r0,%1
   l.ori\t%0,r0,%1
   l.movhi\t%0,hi(%1)
   l.ori\t%0,%1,0
   l.lwz\t%0,%1
   l.sw\t%0,%1"
  [(set_attr "type" "add,load,store,add,logic,move")])

(define_insn "movsi_lo_sum"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
                   (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "l.ori\t%0,%1,lo(%2)"
  [(set_attr "type" "logic")])

(define_insn "movsi_high"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand:SI 1 "immediate_operand" "i")))]
  ""
  "l.movhi\t%0,hi(%1)"
  [(set_attr "type" "move")])

(define_insn "movsi_gotofflo"
  [(set (match_operand:SI 0 "register_operand" "=r")
 	(unspec:SI [(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
                    (match_operand 2 "" ""))] UNSPEC_GOTOFFLO))]
  "flag_pic"
  "l.ori\t%0,%1,gotofflo(%2)"
  [(set_attr "type" "logic")])

(define_insn "movsi_gotoffhi"
  [(set (match_operand:SI 0 "register_operand" "=r")
 	(unspec:SI [(match_operand 1 "" "")] UNSPEC_GOTOFFHI))]
  "flag_pic"
  "l.movhi\t%0,gotoffhi(%1)"
  [(set_attr "type" "move")])

(define_insn "movsi_got"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (unspec:SI [(match_operand 1 "symbolic_operand" "")] UNSPEC_GOT))
   (use (reg:SI 16))]
  "flag_pic"
  "l.lwz\t%0,got(%1)(r16)"
  [(set_attr "type" "load")])

(define_insn "movsi_tlsgdlo"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
                   (match_operand:SI 2 "immediate_operand" "i"))] UNSPEC_TLSGDLO))]
  ""
  "l.ori\t%0,%1,tlsgdlo(%2)"
  [(set_attr "type" "logic")])

(define_insn "movsi_tlsgdhi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "immediate_operand" "i")] UNSPEC_TLSGDHI))]
  ""
  "l.movhi\t%0,tlsgdhi(%1)"
  [(set_attr "type" "move")])

(define_insn "movsi_gottpofflo"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
                   (match_operand:SI 2 "immediate_operand" "i"))] UNSPEC_GOTTPOFFLO))]
  ""
  "l.ori\t%0,%1,gottpofflo(%2)"
  [(set_attr "type" "logic")])

(define_insn "movsi_gottpoffhi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "immediate_operand" "i")] UNSPEC_GOTTPOFFHI))]
  ""
  "l.movhi\t%0,gottpoffhi(%1)"
  [(set_attr "type" "move")])

(define_insn "load_gottpoff"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "register_operand" "r")] UNSPEC_GOTTPOFFLD))]
  ""
  "l.lwz\t%0,0(%1)"
  [(set_attr "type" "load")])

(define_insn "movsi_tpofflo"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
                   (match_operand:SI 2 "immediate_operand" "i"))] UNSPEC_TPOFFLO))]
  ""
  "l.ori\t%0,%1,tpofflo(%2)"
  [(set_attr "type" "logic")])

(define_insn "movsi_tpoffhi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "immediate_operand" "i")] UNSPEC_TPOFFHI))]
  ""
  "l.movhi\t%0,tpoffhi(%1)"
  [(set_attr "type" "move")])

(define_insn_and_split "movsi_insn_big"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "immediate_operand" "i"))]
  "GET_CODE (operands[1]) != CONST_INT"
  ;; the switch of or1k bfd to Rela allows us to schedule insns separately.
  "l.movhi\t%0,hi(%1)\;l.ori\t%0,%0,lo(%1)"
  "(GET_CODE (operands[1]) != CONST_INT
    || ! (CONST_OK_FOR_CONSTRAINT_P (INTVAL (operands[1]), 'I', \"I\")
	  || CONST_OK_FOR_CONSTRAINT_P (INTVAL (operands[1]), 'K', \"K\")
	  || CONST_OK_FOR_CONSTRAINT_P (INTVAL (operands[1]), 'M', \"M\")))
   && reload_completed
   && GET_CODE (operands[1]) != HIGH && GET_CODE (operands[1]) != LO_SUM"
  [(pc)]
{
  if (!or1k_expand_symbol_ref(SImode, operands))
    {
      emit_insn (gen_movsi_high (operands[0], operands[1]));
      emit_insn (gen_movsi_lo_sum (operands[0], operands[0], operands[1]));
    }
  DONE;
}
  [(set_attr "type" "move")
   (set_attr "length" "2")])

;;
;; Conditional Branches & Moves
;; 

; ??? Perhaps delay this expansion until after reload.  We may be
; committing too early as to the sense of the comparison we use to
; make the most of our immediate operands.
(define_expand "cbranchsi4"
  [(set (pc)
	(if_then_else
	  (match_operator 0 "ordered_comparison_operator"
	    [(match_operand:SI 1 "reg_or_0_operand")
	     (match_operand:SI 2 "reg_or_s16_operand")])
	  (label_ref (match_operand 3 ""))
	  (pc)))]
   ""
{
  or1k_expand_compare (operands);
})

; ??? Note that we only support eq ge gt le lt ne.
; There are no opcodes for (un)ordered comparisons.
; Reject these and hope they go to the library.
(define_expand "cbranchsf4"
  [(set (pc)
	(if_then_else
	  (match_operator 0 "ordered_comparison_operator"
	    [(match_operand:SF 1 "register_operand")
	     (match_operand:SF 2 "register_operand")])
	  (label_ref (match_operand 3 ""))
	  (pc)))]
   "TARGET_HARD_FLOAT"
{
  if (!ordered_comparison_operator (operands[0], VOIDmode))
    FAIL;
  or1k_expand_compare (operands);
})

(define_insn "*bf"
  [(set (pc)
	(if_then_else (ne (reg:BI SR_F_REG) (const_int 0))
	  (label_ref (match_operand 0 "" ""))
	  (pc)))]
  ""
  "l.bf\t%l0%("
  [(set_attr "type" "branch")])

(define_insn "*bnf"
  [(set (pc)
	(if_then_else (eq (reg:BI SR_F_REG) (const_int 0))
	  (label_ref (match_operand 0 "" ""))
	  (pc)))]
  ""
  "l.bnf\t%l0%("
  [(set_attr "type" "branch")])

(define_expand "movsicc"
  [(set (reg:BI SR_F_REG) (match_operand 1 "comparison_operator" ""))
   (set (match_operand:SI 0 "register_operand" "")
	(if_then_else:SI
	  (ne (reg:BI SR_F_REG) (const_int 0))
	  (match_operand:SI 2 "reg_or_0_operand" "")
	  (match_operand:SI 3 "reg_or_0_operand" "")))]
  "TARGET_CMOV"
{
  PUT_MODE (operands[1], BImode);
})

(define_insn "*cmovsi_f"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(if_then_else:SI
	  (ne (reg:BI SR_F_REG) (const_int 0))
	  (match_operand:SI 1 "reg_or_0_operand" "rO")
	  (match_operand:SI 2 "reg_or_0_operand" "rO")))]
  "TARGET_CMOV"
  "l.cmov\t%0,%r1,%r2")

(define_insn "*cmovsi_nf"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(if_then_else:SI
	  (eq (reg:BI SR_F_REG) (const_int 0))
	  (match_operand:SI 1 "reg_or_0_operand" "rO")
	  (match_operand:SI 2 "reg_or_0_operand" "rO")))]
  "TARGET_CMOV"
  "l.cmov\t%0,%r2,%r1")

(define_expand "cstoresi4"
  [(set (match_operand:SI 0 "register_operand")
	(match_operator:SI 1 "ordered_comparison_operator"
	  [(match_operand:SI 2 "reg_or_0_operand")
	   (match_operand:SI 3 "reg_or_s16_operand")]))]
  "TARGET_CMOV"
{
  or1k_expand_compare (operands + 1);
  PUT_MODE (operands[1], SImode);
  emit_insn (gen_rtx_SET (operands[0], operands[1]));
  DONE;
})

;; ??? See cbranchsf4.
(define_expand "cstoresf4"
  [(set (match_operand:SI 0 "register_operand")
	(match_operator:SI 1 "ordered_comparison_operator"
	  [(match_operand:SF 2 "register_operand")
	   (match_operand:SF 3 "register_operand")]))]
   "TARGET_CMOV && TARGET_HARD_FLOAT"
{
  if (!ordered_comparison_operator (operands[1], VOIDmode))
    FAIL;
  or1k_expand_compare (operands + 1);
  PUT_MODE (operands[1], SImode);
  emit_insn (gen_rtx_SET (operands[0], operands[1]));
  DONE;
})

;; Being able to copy SR_F to a general register even without CMOV
;; is helpful for the atomic insns, wherein the usual usage is to
;; test the success of the compare-and-swap.  Leaving the success
;; flag in SR_F means that we can in those cases avoid an extra compare
;; and avoid the additional setting of a general register.
;; Therefore, we allow for scc to operate without CMOV, but fail to
;; advertise cstoresi4 without CMOV.

(define_expand "sne"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ne:SI (reg:BI SR_F_REG) (const_int 0)))]
  "")

(define_insn "*scc"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operator:SI 1 "equality_comparison_operator"
	  [(reg:BI SR_F_REG) (const_int 0)]))]
  ""
  "#")

(define_split
  [(set (match_operand:SI 0 "register_operand")
	(match_operator:SI 1 "equality_comparison_operator"
	  [(reg:BI SR_F_REG) (const_int 0)]))]
  "TARGET_CMOV"
  [(set (match_dup 0) (const_int 1))
   (set (match_dup 0)
	(if_then_else:SI (match_dup 1)
	  (match_dup 0)
	  (const_int 0)))]
  "")

(define_split
  [(set (match_operand:SI 0 "register_operand")
	(match_operator:SI 1 "equality_comparison_operator"
	  [(reg:BI SR_F_REG) (const_int 0)]))]
  "!TARGET_CMOV && reload_completed"
  [(const_int 0)]
{
  emit_move_insn (operands[0], const1_rtx);

  rtx label = gen_label_rtx ();
  rtx x = gen_rtx_LABEL_REF (Pmode, label);
  x = gen_rtx_IF_THEN_ELSE (VOIDmode, operands[1], x, pc_rtx);
  emit_jump_insn (gen_rtx_SET (pc_rtx, x));

  emit_move_insn (operands[0], const0_rtx);

  emit_label (label);
  DONE;
})

;;
;; Setting SR[F] from comparision
;;

(define_insn "*comparesi"
  [(set (reg:BI SR_F_REG)
	(match_operator:BI 2 "ordered_comparison_operator"
	  [(match_operand:SI 0 "reg_or_0_operand"  "rO,rO")
	   (match_operand:SI 1 "reg_or_s16_operand" "I,r")]))]
  ""
  "@
   l.sf%C2i\t%r0,%1
   l.sf%C2\t%r0,%1"
  [(set_attr "type" "compare")])

(define_insn "*comparesf"
  [(set (reg:BI SR_F_REG)
	(match_operator:BI 2 "ordered_comparison_operator"
	  [(match_operand:SF 0 "register_operand" "r")
	   (match_operand:SF 1 "register_operand" "r")]))]
  "TARGET_HARD_FLOAT"
  "lf.sf%C2.s\t%0,%1"
  [(set_attr "type" "compare")])

;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
(define_insn_and_split "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,o,r")
	(match_operand:DI 1 "general_operand"      " r,o,r,n"))]
  ""
  { return or1k_output_move_double (operands); }
  "&& reload_completed && CONSTANT_P (operands[1])"
  [(set (match_dup 2) (match_dup 3)) (set (match_dup 4) (match_dup 5))]
  {
    operands[2] = operand_subword (operands[0], 0, 0, DImode);
    operands[3] = operand_subword (operands[1], 0, 0, DImode);
    operands[4] = operand_subword (operands[0], 1, 0, DImode);
    operands[5] = operand_subword (operands[1], 1, 0, DImode);
  }
  [(set_attr "length" "2,2,2,3")])

;; Moving double and single precision floating point values


(define_insn "movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=r,r,o,r")
	(match_operand:DF 1 "general_operand"      " r,o,r,i"))]
  ""
  { return or1k_output_move_double (operands); }
  [(set_attr "length" "2,2,2,3")])


(define_insn "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,r,m")
        (match_operand:SF 1 "general_operand"  "r,m,r"))]
  ""
  "@
   l.ori\t%0,%1,0
   l.lwz\t%0,%1
   l.sw\t%0,%1"
  [(set_attr "type" "move,load,store")])


;;
;; extendqisi2
;;

(define_expand "extendqisi2"
  [(use (match_operand:SI 0 "register_operand" ""))
   (use (match_operand:QI 1 "nonimmediate_operand" ""))]
  ""
{
  if (TARGET_SEXT)
    emit_insn (gen_extendqisi2_sext(operands[0], operands[1]));
  else {
    if ( GET_CODE(operands[1]) == MEM ) {
      emit_insn (gen_extendqisi2_no_sext_mem(operands[0], operands[1]));
    }
    else {
      emit_insn (gen_extendqisi2_no_sext_reg(operands[0], operands[1]));
    }
 }
 DONE;
})

(define_insn "extendqisi2_sext"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_SEXT"
  "@
   l.extbs\t%0,%1
   l.lbs\t%0,%1"
  [(set_attr "type" "extend,load")])

(define_insn "extendqisi2_no_sext_mem"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI (match_operand:QI 1 "memory_operand" "m")))]
  "!TARGET_SEXT"
  "l.lbs\t%0,%1"
  [(set_attr "type" "load")])

(define_expand "extendqisi2_no_sext_reg"
  [(set (match_dup 2)
	(ashift:SI (match_operand:QI 1 "register_operand" "")
		   (const_int 24)))
   (set (match_operand:SI 0 "register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 24)))]
  "!TARGET_SEXT"
{
  operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode);
})

;;
;; extendhisi2
;;

(define_expand "extendhisi2"
  [(use (match_operand:SI 0 "register_operand" ""))
   (use (match_operand:HI 1 "nonimmediate_operand" ""))]
  ""
{
  if (TARGET_SEXT)
    emit_insn (gen_extendhisi2_sext(operands[0], operands[1]));
  else {
    if ( GET_CODE(operands[1]) == MEM ) {
      emit_insn (gen_extendhisi2_no_sext_mem(operands[0], operands[1]));
    }
    else {
      emit_insn (gen_extendhisi2_no_sext_reg(operands[0], operands[1]));
    }
 }
 DONE;
})

(define_insn "extendhisi2_sext"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_SEXT"
  "@
   l.exths\t%0,%1
   l.lhs\t%0,%1"
  [(set_attr "type" "extend,load")])

(define_insn "extendhisi2_no_sext_mem"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI (match_operand:HI 1 "memory_operand" "m")))]
  "!TARGET_SEXT"
  "l.lhs\t%0,%1"
  [(set_attr "type" "load")])

(define_expand "extendhisi2_no_sext_reg"
  [(set (match_dup 2)
	(ashift:SI (match_operand:HI 1 "register_operand" "")
		   (const_int 16)))
   (set (match_operand:SI 0 "register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 16)))]
  "!TARGET_SEXT"
{
  operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode);
})


;;
;; zero_extend<m><n>2
;;

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   l.andi\t%0,%1,0xff
   l.lbz\t%0,%1"
  [(set_attr "type" "logic,load")])

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   l.andi\t%0,%1,0xffff
   l.lhz\t%0,%1"
  [(set_attr "type" "logic,load")])

;;
;; Shift/rotate operations
;;

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (ashift:SI (match_operand:SI 1 "register_operand" "r,r")
                   (match_operand:SI 2 "shift_operand"    "r,L")))]
  ""
  "@
   l.sll\t%0,%1,%2
   l.slli\t%0,%1,%2"
  [(set_attr "type" "shift")])

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (ashiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
                     (match_operand:SI 2 "shift_operand"    "r,L")))]
  ""
  "@
   l.sra\t%0,%1,%2
   l.srai\t%0,%1,%2"
  [(set_attr "type" "shift")])

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (lshiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
                     (match_operand:SI 2 "shift_operand"    "r,L")))]
  ""
  "@
   l.srl\t%0,%1,%2
   l.srli\t%0,%1,%2"
  [(set_attr "type" "shift")])

(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (rotatert:SI (match_operand:SI 1 "register_operand" "r,r")
		     (match_operand:SI 2 "shift_operand"    "r,L")))]
  "TARGET_ROR"
  "@
   l.ror\t%0,%1,%2
   l.rori\t%0,%1,%2"
  [(set_attr "type" "shift")])

;;
;; Logical bitwise operations
;;

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(and:SI (match_operand:SI 1 "register_operand" "%r,r")
		(match_operand:SI 2 "reg_or_u16_operand" "r,K")))]
  ""
  "@
   l.and\t%0,%1,%2
   l.andi\t%0,%1,%2"
  [(set_attr "type" "logic")])

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ior:SI (match_operand:SI 1 "register_operand" "%r,r")
		(match_operand:SI 2 "reg_or_u16_operand" "r,K")))]
  ""
  "@
   l.or\t%0,%1,%2
   l.ori\t%0,%1,%2"
  [(set_attr "type" "logic")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(xor:SI (match_operand:SI 1 "register_operand" "%r,r")
		(match_operand:SI 2 "reg_or_s16_operand" "r,I")))]
  ""
  "@
   l.xor\t%0,%1,%2
   l.xori\t%0,%1,%2"
  [(set_attr "type" "logic")])

(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(not:QI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "l.xori\t%0,%1,0x00ff"
  [(set_attr "type" "logic")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "l.xori\t%0,%1,0xffff"
  [(set_attr "type" "logic")])

;;
;; Arithmetic operations 
;;

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "l.sub\t%0,r0,%1"
  [(set_attr "type" "add")])

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI (match_operand:SI 1 "register_operand" "%r,r")
		 (match_operand:SI 2 "reg_or_s16_operand" "r,I")))]
  ""
  "@
   l.add\t%0,%1,%2
   l.addi\t%0,%1,%2"
  [(set_attr "type" "add")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "register_operand" "r")
		  (match_operand:SI 2 "register_operand" "r")))]
  ""
  "l.sub\t%0,%1,%2"
  [(set_attr "type" "add")])

;;
;; mul and div
;;

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (mult:SI (match_operand:SI 1 "register_operand"   "%r,r")
                 (match_operand:SI 2 "reg_or_s16_operand" " r,I")))]
  "TARGET_HARD_MUL"
  "@
   l.mul\t%0,%1,%2
   l.muli\t%0,%1,%2"
  [(set_attr "type" "mul")])

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (div:SI (match_operand:SI 1 "register_operand" "r")
                 (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_HARD_DIV"
  "l.div\t%0,%1,%2"
  [(set_attr "type" "mul")])

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (udiv:SI (match_operand:SI 1 "register_operand" "r")
                 (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_HARD_DIV"
  "l.divu\t%0,%1,%2"
  [(set_attr "type" "mul")])

;;
;; jumps 
;;

;; jump

(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "l.j\t%l0%("
  [(set_attr "type" "jump")])

;; indirect jump

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "l.jr\t%0%("
  [(set_attr "type" "jump")])

;; table jump

(define_expand "tablejump"
  [(parallel [(set (pc) (match_operand:SI 0 "register_operand" ""))
	      (use (label_ref (match_operand 1 "" "")))])]
   ""
{
  if (CASE_VECTOR_PC_RELATIVE || flag_pic)
    operands[0]
      = force_reg (Pmode,
		   gen_rtx_PLUS (Pmode, operands[0],
				 gen_rtx_LABEL_REF (Pmode, operands[1])));
})

(define_insn "*tablejump_internal"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "l.jr\t%0%("
  [(set_attr "type" "jump")])


;; no-op

(define_insn "nop"
  [(const_int 0)]
  ""
  "l.nop"
  [(set_attr "type" "logic")])

;;
;; floating point
;;

;; floating point arithmetic 

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (plus:SF (match_operand:SF 1 "register_operand" "r")
                 (match_operand:SF 2 "register_operand" "r")))]
  "TARGET_HARD_FLOAT"
  "lf.add.s\t%0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (plus:DF (match_operand:DF 1 "register_operand" "r")
                 (match_operand:DF 2 "register_operand" "r")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "lf.add.d\t%0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (minus:SF (match_operand:SF 1 "register_operand" "r")
                 (match_operand:SF 2 "register_operand" "r")))]
  "TARGET_HARD_FLOAT"
  "lf.sub.s\t%0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (minus:DF (match_operand:DF 1 "register_operand" "r")
		  (match_operand:DF 2 "register_operand" "r")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "lf.sub.d\t%0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (mult:SF (match_operand:SF 1 "register_operand" "r")
                 (match_operand:SF 2 "register_operand" "r")))]
  "TARGET_HARD_FLOAT"
  "lf.mul.s\t%0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (mult:DF (match_operand:DF 1 "register_operand" "r")
                 (match_operand:DF 2 "register_operand" "r")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "lf.mul.d\t%0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (div:SF (match_operand:SF 1 "register_operand" "r")
		(match_operand:SF 2 "register_operand" "r")))]
  "TARGET_HARD_FLOAT"
  "lf.div.s\t%0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (div:DF (match_operand:DF 1 "register_operand" "r")
		(match_operand:DF 2 "register_operand" "r")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "lf.div.d\t%0,%1,%2"
  [(set_attr "type" "fp")])

;; Conversion between fixed point and floating point.


(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(float:SF (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_HARD_FLOAT"
  "lf.itof.s\t%0,%1"
  [(set_attr "type" "fp")])

;; not working 
(define_insn "fixunssfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(fix:SI (match_operand:SF 1 "register_operand" "r")))]
  "TARGET_HARD_FLOAT"
  "lf.ftoi.s\t%0,%1"
  [(set_attr "type" "fp")])

;; The insn to set GOT.
(define_insn "set_got"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(const_int 0)] UNSPEC_SET_GOT))
   (clobber (reg:SI 9))]
  ""
{
  if (TARGET_DELAY_ON)
    return ("l.jal\t8\;"
	    " l.movhi\t%0,gotpchi(_GLOBAL_OFFSET_TABLE_-4)\;"
	    "l.ori\t%0,%0,gotpclo(_GLOBAL_OFFSET_TABLE_+0)\;"
	    "l.add\t%0,%0,r9");
  else if (TARGET_DELAY_OFF)
    return ("l.jal\t4\;"
	    "l.movhi\t%0,gotpchi(_GLOBAL_OFFSET_TABLE_)\;"
	    "l.ori\t%0,%0,gotpclo(_GLOBAL_OFFSET_TABLE_+4)\;"
	    "l.add\t%0,%0,r9");
  else
    return ("l.jal\t8\;"
	    " l.nop\;"
	    "l.movhi\t%0,gotpchi(_GLOBAL_OFFSET_TABLE_)\;"
	    "l.ori\t%0,%0,gotpclo(_GLOBAL_OFFSET_TABLE_+4)\;"
	    "l.add\t%0,%0,r9");
}
  [(set (attr "length")
	(if_then_else (ne (symbol_ref "TARGET_DELAY_COMPAT") (const_int 0))
	  (const_int 5)
	  (const_int 4)))])

;;
;; Atomic operations
;;

(include "sync.md")

;;
;; CALLS
;; Leave these to last, as the modeless operand for call_value
;; interferes with normal patterns.
;;

(define_expand "call"
  [(call (match_operand 0)
	 (match_operand 1))
   (use (match_operand 2))]
  ""
{
  or1k_expand_call (NULL, operands[0], operands[1]);
  DONE;
})

(define_expand "sibcall"
  [(call (match_operand 0)
	 (match_operand 1))
   (use (match_operand 2))]
  ""
{
  or1k_expand_call (NULL, operands[0], operands[1]);
  DONE;
})

(define_expand "call_value"
  [(set (match_operand 0)
	(call (match_operand:SI 1)
	      (match_operand 2)))
   (use (match_operand 3))]
  ""
{
  or1k_expand_call (operands[0], operands[1], operands[2]);
  DONE;
})

(define_expand "sibcall_value"
  [(set (match_operand 0)
	(call (match_operand:SI 1)
	      (match_operand 2)))
   (use (match_operand 3))]
  ""
{
  or1k_expand_call (operands[0], operands[1], operands[2]);
  DONE;
})

(define_insn "*call"
  [(call (mem:SI (match_operand:SI 0 "call_insn_operand" "r,s"))
                 (match_operand 1))]
  "!SIBLING_CALL_P (insn)"
  "@
   l.jalr\t%0%(
   l.jal\t%P0%("
  [(set_attr "type" "jump")])

(define_insn "*sibcall"
  [(call (mem:SI (match_operand:SI 0 "call_insn_operand" "c,s"))
                 (match_operand 1))]
  "SIBLING_CALL_P (insn)"
  "@
   l.jr\t%0%(
   l.j\t%P0%("
  [(set_attr "type" "jump")])

(define_insn "*call_value"
  [(set (match_operand 0)
	(call (mem:SI (match_operand:SI 1 "call_insn_operand" "r,s"))
	      (match_operand 2)))]
  "!SIBLING_CALL_P (insn)"
  "@
   l.jalr\t%1%(
   l.jal\t%P1%("
  [(set_attr "type" "jump")])

(define_insn "*sibcall_value"
  [(set (match_operand 0)
	(call (mem:SI (match_operand:SI 1 "call_insn_operand" "c,s"))
	      (match_operand 2)))]
  "SIBLING_CALL_P (insn)"
  "@
   l.jr\t%1%(
   l.j\t%P1%("
  [(set_attr "type" "jump")])

;; Local variables:
;; mode:emacs-lisp
;; comment-start: ";; "
;; eval: (set-syntax-table (copy-sequence (syntax-table)))
;; eval: (modify-syntax-entry ?[ "(]")
;; eval: (modify-syntax-entry ?] ")[")
;; eval: (modify-syntax-entry ?{ "(}")
;; eval: (modify-syntax-entry ?} "){")
;; eval: (setq indent-tabs-mode t)
;; End:
