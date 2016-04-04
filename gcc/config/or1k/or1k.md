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
  (SR_CY_REG 35)
])

(define_c_enum "unspec" [
  UNSPEC_GOT
  UNSPEC_GOTOFF
  UNSPEC_TPOFF
  UNSPEC_GOTTPOFF
  UNSPEC_TLSGD
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
(define_mode_attr sz [(QI "b") (HI "h") (SI "w")])

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
   (clobber (mem:BLK (match_scratch:SI 3 "=X,X")))
   (clobber (reg:BI SR_CY_REG))]
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
;; movQI/movHI
;;

(define_expand "mov<mode>"
  [(set (match_operand:I12 0 "general_operand" "")
	(match_operand:I12 1 "general_operand" ""))]
  ""
{
  or1k_expand_move (<MODE>mode, operands[0], operands[1]);
  DONE;
})

(define_insn "*mov<mode>"
  [(set (match_operand:I12 0 "nonimmediate_operand" "=m,r,r,r,r")
	(match_operand:I12 1 "general_operand"      "rO,r,K,I,m"))]
  "register_operand (operands[0], <MODE>mode)
   || reg_or_0_operand (operands[1], <MODE>mode)"
  "@
   l.s<sz>\t%0,%r1
   l.ori\t%0,%1,0
   l.ori\t%0,r0,%1
   l.xori\t%0,r0,%1
   l.l<sz>z\t%0,%1"
  [(set_attr "type" "store,logic,logic,logic,load")])

;;
;; movSI
;;

(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
{
  or1k_expand_move (SImode, operands[0], operands[1]);
  DONE;
})

(define_insn "*movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=m,r,r,r,r,r")
        (match_operand:SI 1 "input_operand"        "rO,M,K,I,r,m"))]
  "register_operand (operands[0], SImode)
   || reg_or_0_operand (operands[1], SImode)"
  "@
   l.sw\t%0,%r1
   l.movhi\t%0,hi(%1)
   l.ori\t%0,r0,%1
   l.xori\t%0,r0,%1
   l.ori\t%0,%1,0
   l.lwz\t%0,%1"
  [(set_attr "type" "store,move,logic,logic,logic,load")])

(define_insn "*movsi_lo_sum"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
                   (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "l.ori\t%0,%1,%L2"
  [(set_attr "type" "logic")])

(define_insn "*movsi_high"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand:SI 1 "immediate_operand" "i")))]
  ""
  "l.movhi\t%0,%h1"
  [(set_attr "type" "move")])

(define_insn_and_split "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,o,r")
	(match_operand:DI 1 "general_operand"      " r,o,r,n"))]
  ""
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
	   (match_operand:SI 3 "nonmemory_operand")]))]
  ""
{
  rtx_code code = GET_CODE (operands[1]);
  rtx out = operands[0];
  rtx in0 = operands[2];
  rtx in1 = operands[3];
  bool need_inv = false;
  bool need_swap = false;
  bool need_zero = false;
  bool need_reg = false;

 restart:
  switch (code)
    {
    case EQ:
      need_inv = true;
      /* fallthru */
    case NE:
      need_zero = in1 != const0_rtx;
      need_reg = !reg_or_s16_operand(in1, SImode);
      break;

    case LEU:
      if (CONST_INT_P (in1) && INTVAL (in1) != -1)
	{
	  in1 = gen_int_mode (INTVAL (in1) + 1, SImode);
	  code = LTU;
	  goto do_ltu_i;
	}
      need_swap = true;
      goto do_geu;

    case GTU:
      if (CONST_INT_P (in1) && INTVAL (in1) != -1)
	{
	  in1 = gen_int_mode (INTVAL (in1) + 1, SImode);
	  code = GEU;
	  goto do_geu;
	}
      need_swap = true;
      goto do_ltu_r;

    case GEU:
    do_geu:
      if (sgeui_operand (in1, SImode))
	break;
      need_inv = true;
      goto do_ltu_r;

    case LTU:
    do_ltu_i:
      if (sgeui_operand (in1, SImode))
	{
	  need_inv = true;
	  break;
	}
    do_ltu_r:
      need_reg = !reg_or_0_operand (in1, SImode);
      break;

    case LT:
      if (in1 == const0_rtx)
	break;
      goto do_default;

    case GE:
      if (in1 == const0_rtx)
	{
	  need_inv = true;
	  break;
	}
      goto do_default;

    default:
    do_default:
      if (TARGET_CMOV)
	goto do_cmov;
      if (optimize_size)
	FAIL;
      rtx min = force_reg (SImode, gen_int_mode (0x80000000, SImode));
      in0 = expand_binop (SImode, add_optab, in0, min, NULL, 1, OPTAB_DIRECT);
      if (CONST_INT_P (in1))
	in1 = gen_int_mode (INTVAL (in1) + 0x80000000, SImode);
      else
	in1 = expand_binop (SImode, add_optab, in1, min, NULL, 1, OPTAB_DIRECT);
      code = unsigned_condition (code);
      goto restart;
    }

  /* Use l.cmov unless it's more expensive.  */
  if (TARGET_CMOV
      && (3 + !reg_or_s16_operand (in1, SImode)
	  <= 2 + need_inv + need_zero + need_reg))
    {
    do_cmov:
      if (!reg_or_s16_operand (in1, SImode))
	in1 = force_reg (SImode, in1);
      if (GET_CODE (operands[1]) != code || operands[3] != in1)
	{
	  operands[1] = gen_rtx_fmt_ee (code, SImode, in0, in1);
	  operands[3] = in1;
	}
      or1k_expand_compare (operands + 1);
      PUT_MODE (operands[1], SImode);
      emit_insn (gen_rtx_SET (out, operands[1]));
      DONE;
    }

  if (need_swap)
    {
      std::swap (in0, in1);
      code = swap_condition (code);
    }
  if (need_inv)
    code = reverse_condition (code);
  if (need_reg)
    in1 = force_reg (SImode, in1);
  if (need_zero)
    {
      emit_insn (gen_xorsi3 (out, in0, in1));
      in0 = out;
      in1 = const0_rtx;
    }

  switch (code)
    {
    case NE:
      emit_insn (gen_sne_sr_cy (out, in0));
      break;
    case LT:
      emit_insn (gen_lshrsi3 (out, in0, GEN_INT (31)));
      break;
    case LTU:
      emit_insn (gen_sltu_sr_cy (out, in0, in1));
      break;
    case GEU:
      emit_insn (gen_sgeui_sr_cy (out, in0, in1));
      break;
    default:
      gcc_unreachable ();
    }

  if (need_inv)
    emit_insn (gen_xorsi3 (out, out, const1_rtx));
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

(define_expand "sne_sr_f"
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

(define_insn_and_split "sne_sr_cy"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ne:SI (match_operand:SI 1 "register_operand" "r")
	       (const_int 0)))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_addsi_co (operands[0], operands[1], constm1_rtx));
  emit_insn (gen_addsi_ci (operands[0], const0_rtx, const0_rtx));
  DONE;
})

(define_insn_and_split "*add1_sne_sr_cy"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(plus:SI
	  (ne:SI (match_operand:SI 1 "register_operand" "r")
	         (const_int 0))
	  (match_operand:SI 2 "reg_or_s16_operand" "rI")))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_addsi_co (operands[0], operands[1], constm1_rtx));
  emit_insn (gen_addsi_ci (operands[0], const0_rtx, operands[2]));
  DONE;
})

(define_insn_and_split "*add2_sne_sr_cy"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(plus:SI
	  (plus:SI
	    (ne:SI (match_operand:SI 1 "register_operand" "r")
	           (const_int 0))
	    (match_operand:SI 2 "register_operand" "%r"))
	  (match_operand:SI 3 "reg_or_s16_operand" "rI")))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_addsi_co (operands[0], operands[1], constm1_rtx));
  emit_insn (gen_addsi_ci (operands[0], operands[2], operands[3]));
  DONE;
})

(define_insn_and_split "sltu_sr_cy"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(ltu:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
		(match_operand:SI 2 "reg_or_0_operand" "rO")))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_subsi_co (operands[0], operands[1], operands[2]));
  emit_insn (gen_addsi_ci (operands[0], const0_rtx, const0_rtx));
  DONE;
})

(define_insn_and_split "*add1_slt_sr_cy"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(plus:SI
	  (ltu:SI
	    (match_operand:SI 1 "register_operand" "r")
	    (match_operand:SI 2 "register_operand" "r"))
	  (match_operand:SI 3 "reg_or_s16_operand" "rI")))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_subsi_co (operands[0], operands[1], operands[2]));
  emit_insn (gen_addsi_ci (operands[0], const0_rtx, operands[3]));
  DONE;
})

(define_insn_and_split "*add2_slt_sr_cy"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(plus:SI
	  (plus:SI
	    (ltu:SI
	      (match_operand:SI 1 "register_operand" "r")
	      (match_operand:SI 2 "register_operand" "r"))
	    (match_operand:SI 3 "register_operand" "%r"))
	  (match_operand:SI 4 "reg_or_s16_operand" "rI")))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_subsi_co (operands[0], operands[1], operands[2]));
  emit_insn (gen_addsi_ci (operands[0], operands[3], operands[4]));
  DONE;
})

(define_insn_and_split "sgeui_sr_cy"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(geu:SI (match_operand:SI 1 "register_operand" "r")
		(match_operand:SI 2 "sgeui_operand" "n")))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  HOST_WIDE_INT c = INTVAL (operands[2]);
  emit_insn (gen_addsi_co (operands[0], operands[1], GEN_INT (-c)));
  emit_insn (gen_addsi_ci (operands[0], const0_rtx, const0_rtx));
  DONE;
})

(define_insn_and_split "*add1_sgeui_sr_cy"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(plus:SI
	  (geu:SI (match_operand:SI 1 "register_operand" "r")
		  (match_operand:SI 2 "sgeui_operand" "n"))
	  (match_operand:SI 3 "reg_or_s16_operand" "rI")))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  HOST_WIDE_INT c = INTVAL (operands[2]);
  /* See sgeui_operand comments.  */
  c = (c < 0 ? -c : -1 - (c + 1));
  emit_insn (gen_addsi_co (operands[0], operands[1], GEN_INT (c)));
  emit_insn (gen_addsi_ci (operands[0], const0_rtx, operands[3]));
  DONE;
})

(define_insn_and_split "*add2_sgeui_sr_cy"
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(plus:SI
	  (plus:SI
	    (geu:SI (match_operand:SI 1 "register_operand" "r")
		    (match_operand:SI 2 "sgeui_operand" "n"))
	    (match_operand:SI 3 "register_operand" "%r"))
	  (match_operand:SI 4 "reg_or_s16_operand" "rI")))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "&& reload_completed"
  [(const_int 0)]
{
  HOST_WIDE_INT c = INTVAL (operands[2]);
  /* See sgeui_operand comments.  */
  c = (c < 0 ? -c : -1 - (c + 1));
  emit_insn (gen_addsi_co (operands[0], operands[1], GEN_INT (c)));
  emit_insn (gen_addsi_ci (operands[0], operands[3], operands[4]));
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

;; Moving double and single precision floating point values

(define_expand "movsf"
  [(set (match_operand:SI 0 "general_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
{
  or1k_expand_move (SFmode, operands[0], operands[1]);
  DONE;
})

(define_insn "*movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,r,m,r")
        (match_operand:SF 1 "general_operand"      "rG,m,rG,F"))]
  "register_operand (operands[0], SFmode)
   || reg_or_0_operand (operands[1], SFmode)"
  "@
   l.ori\t%0,%r1,0
   l.lwz\t%0,%1
   l.sw\t%0,%r1
   #"
  [(set_attr "type" "logic,load,store,unknown")])

(define_split
  [(set (match_operand:SF 0 "register_operand")
        (match_operand:SF 1 "const_double_operand"))]
  "reload_completed"
  [(const_int 0)]
{
  long img;
  REAL_VALUE_TO_TARGET_SINGLE (*CONST_DOUBLE_REAL_VALUE (operands[1]), img);
  emit_move_insn (gen_lowpart (SImode, operands[0]),
		  gen_int_mode (img, SImode));
  DONE;
})

;;
;; extendqisi2
;;

(define_expand "extendqisi2"
  [(set (match_operand:SI 0 "register_operand")
	(sign_extend:SI
	  (match_operand:QI 1 "nonimmediate_operand")))]
  ""
{
  if (!TARGET_SEXT && !MEM_P (operands[1]))
    {
      rtx shift = GEN_INT (24);
      rtx sub = can_create_pseudo_p () ? gen_reg_rtx (SImode) : operands[0];
      operands[1] = gen_lowpart (SImode, operands[1]);
      emit_insn (gen_ashlsi3 (sub, operands[1], shift));
      emit_insn (gen_ashrsi3 (operands[0], sub, shift));
      DONE;
    }
})

(define_insn "*extendqisi2_sext"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_SEXT"
  "@
   l.extbs\t%0,%1
   l.lbs\t%0,%1"
  [(set_attr "type" "extend,load")])

(define_insn "*extendqisi2_no_sext"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI (match_operand:QI 1 "memory_operand" "m")))]
  "!TARGET_SEXT"
  "l.lbs\t%0,%1"
  [(set_attr "type" "load")])

;;
;; extendhisi2
;;

(define_expand "extendhisi2"
  [(set (match_operand:SI 0 "register_operand")
	(sign_extend:SI
	  (match_operand:HI 1 "nonimmediate_operand")))]
  ""
{
  if (!TARGET_SEXT && !MEM_P (operands[1]))
    {
      rtx shift = GEN_INT (16);
      rtx sub = can_create_pseudo_p () ? gen_reg_rtx (SImode) : operands[0];
      operands[1] = gen_lowpart (SImode, operands[1]);
      emit_insn (gen_ashlsi3 (sub, operands[1], shift));
      emit_insn (gen_ashrsi3 (operands[0], sub, shift));
      DONE;
    }
})

(define_insn "*extendhisi2_sext"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_SEXT"
  "@
   l.exths\t%0,%1
   l.lhs\t%0,%1"
  [(set_attr "type" "extend,load")])

(define_insn "*extendhisi2_no_sext"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI (match_operand:HI 1 "memory_operand" "m")))]
  "!TARGET_SEXT"
  "l.lhs\t%0,%1"
  [(set_attr "type" "load")])

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
	(neg:SI (match_operand:SI 1 "register_operand" "r")))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "l.sub\t%0,r0,%1"
  [(set_attr "type" "add")])

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI (match_operand:SI 1 "register_operand" "%r,r")
		 (match_operand:SI 2 "reg_or_s16_operand" "r,I")))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "@
   l.add\t%0,%1,%2
   l.addi\t%0,%1,%2"
  [(set_attr "type" "add")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
		  (match_operand:SI 2 "reg_or_0_operand" "rO")))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "l.sub\t%0,%r1,%r2"
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
                 (match_operand:SI 2 "register_operand" "r")))
   (clobber (reg:BI SR_CY_REG))]
  "TARGET_HARD_DIV"
  "l.divu\t%0,%1,%2"
  [(set_attr "type" "mul")])

;;
;; Double-word arithmetic
;;

;; Because there is no non-carry-clobbering addition insn,
;; reload can clobber CY for address arithmetic.  Thus these
;; patterns must remain whole until after reload.

(define_expand "adddi3"
  [(set (match_operand:DI 0 "register_operand")
	(plus:DI (match_operand:DI 1 "register_operand")
		 (match_operand:DI 2 "reg_or_s16_operand")))]
  ""
{
  rtx h0, h1, h2, l0, l1, l2;

  h0 = gen_highpart (SImode, operands[0]);
  l0 = gen_lowpart (SImode, operands[0]);
  h1 = gen_highpart (SImode, operands[1]);
  l1 = gen_lowpart (SImode, operands[1]);
  if (CONST_INT_P (operands[2]))
    {
      l2 = operands[2];
      h2 = (INTVAL (l2) < 0 ? constm1_rtx : const0_rtx);
    }
  else
    {
      h2 = gen_highpart (SImode, operands[2]);
      l2 = gen_lowpart (SImode, operands[2]);
    }

  emit_insn (gen_adddi_int (l0, l1, l2, h0, h1, h2));
  DONE;
})

(define_insn_and_split "adddi_int"
  [(set (match_operand:SI 0 "register_operand"         "=r,&r")
	(plus:SI
	  (match_operand:SI 1 "register_operand"       "%0, r")
	  (match_operand:SI 2 "reg_or_s16_operand"     "rI,rI")))
   (set (match_operand:SI 3 "register_operand"         "=r, r")
	(plus:SI
	  (plus:SI
	    (ltu:SI
	      (plus:SI (match_dup 1) (match_dup 2))
              (match_dup 1))
	    (match_operand:SI 4 "register_operand"     " r, r"))
	  (match_operand:SI 5 "reg_or_s16_operand"     "rI,rI")))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  if (CONST_INT_P (operands[4]) && INTVAL (operands[4]) != 0)
    std::swap (operands[4], operands[5]);
  emit_insn (gen_addsi_co (operands[0], operands[1], operands[2]));
  emit_insn (gen_addsi_ci (operands[3], operands[4], operands[5]));
  DONE;
})

(define_insn_and_split "*adddi_1"
  [(set (match_operand:SI 0 "register_operand"     "=r, r,&r")
	(plus:SI
	  (match_operand:SI 1 "register_operand"   "%0, r, r")
	  (match_operand:SI 2 "reg_or_s16_operand" "rI,rI,rI")))
   (set (match_operand:SI 3 "register_operand"     "=r, r, r")
	(plus:SI
	  (ltu:SI
	    (plus:SI (match_dup 1) (match_dup 2))
	    (match_dup 1))
	  (match_operand:SI 4 "reg_or_s16_operand" "rI, I, r")))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_addsi_co (operands[0], operands[1], operands[2]));
  emit_insn (gen_addsi_ci (operands[3], const0_rtx, operands[4]));
  DONE;
})

(define_insn_and_split "*adddi_2"
  [(set (match_operand:SI 0 "register_operand"         "=r,&r")
	(plus:SI
	  (match_operand:SI 1 "register_operand"       "%0, r")
	  (match_operand:SI 2 "reg_or_s16_operand"     "rI,rI")))
   (set (match_operand:SI 3 "register_operand"         "=r, r")
	(plus:SI
	  (plus:SI
	    (ltu:SI
	      (plus:SI (match_dup 1) (match_dup 2))
	      (match_dup 1))
	    (match_operand:SI 4 "register_operand"     " r, r"))
	  (match_operand:SI 5 "reg_or_s16_operand"     "rI,rI")))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_addsi_co (operands[0], operands[1], operands[2]));
  emit_insn (gen_addsi_ci (operands[3], operands[4], operands[5]));
  DONE;
})

(define_insn_and_split "*adddi_3"
  [(set (match_operand:SI 0 "register_operand"         "=r, r,&r")
	(plus:SI
	  (match_operand:SI 1 "register_operand"       "%0, r, r")
	  (const_int -1)))
   (set (match_operand:SI 2 "register_operand"         "=r, r, r")
	(plus:SI
	  (ne:SI (match_dup 1) (const_int 0))
	  (match_operand:SI 3 "reg_or_s16_operand"     "rI, I, r")))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_addsi_co (operands[0], operands[1], constm1_rtx));
  emit_insn (gen_addsi_ci (operands[2], const0_rtx, operands[3]));
  DONE;
})

(define_insn_and_split "*adddi_4"
  [(set (match_operand:SI 0 "register_operand"         "=r,&r")
	(plus:SI
	  (match_operand:SI 1 "register_operand"       "%0, r")
	  (const_int -1)))
   (set (match_operand:SI 2 "register_operand"         "=r, r")
	(plus:SI
	  (plus:SI
	    (ltu:SI
	      (plus:SI (match_dup 1) (match_dup 2))
	      (match_dup 1))
	    (match_operand:SI 3 "register_operand"     " r, r"))
	  (match_operand:SI 4 "reg_or_s16_operand"     "rI,rI")))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_addsi_co (operands[0], operands[1], constm1_rtx));
  emit_insn (gen_addsi_ci (operands[2], operands[3], operands[4]));
  DONE;
})

(define_expand "subdi3"
  [(set (match_operand:DI 0 "register_operand")
	(plus:DI (match_operand:DI 1 "reg_or_0_operand")
		 (match_operand:DI 2 "register_operand")))]
  ""
{
  rtx h0, h1, h2, l0, l1, l2;

  h0 = gen_highpart (SImode, operands[0]);
  l0 = gen_lowpart (SImode, operands[0]);
  if (operands[1] == const0_rtx)
    h1 = l1 = const0_rtx;
  else
    {
      h1 = gen_highpart (SImode, operands[1]);
      l1 = gen_lowpart (SImode, operands[1]);
    }
  h2 = gen_highpart (SImode, operands[2]);
  l2 = gen_lowpart (SImode, operands[2]);

  emit_insn (gen_subdi_int (l0, l1, l2, h0, h1, h2));
  DONE;
})

(define_insn_and_split "subdi_int"
  [(set (match_operand:SI 0 "register_operand"           "=r,&r")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" " 0,rO")
		  (match_operand:SI 2 "reg_or_0_operand" "rO,rO")))
   (set (match_operand:SI 3 "register_operand"           "=r,r")
	(minus:SI
	  (minus:SI
	    (match_operand:SI 4 "reg_or_0_operand"       "rO,rO")
	    (match_operand:SI 5 "reg_or_0_operand"       "rO,rO"))
	  (ltu:SI (match_dup 1) (match_dup 2))))
   (clobber (match_scratch:SI 6 "=&r,&r"))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_subsi_co (operands[0], operands[1], operands[2]));
  emit_insn (gen_addsi_ci (operands[6], const0_rtx, const0_rtx));
  if (operands[5] == const0_rtx)
    emit_insn (gen_subsi3 (operands[3], operands[4], operands[6]));
  else
    {
      emit_insn (gen_subsi3 (operands[3], operands[4], operands[5]));
      emit_insn (gen_subsi3 (operands[3], operands[3], operands[6]));
    }
  DONE;
})

(define_insn_and_split "*subdi_1"
  [(set (match_operand:SI 0 "register_operand"           "=r,&r")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" " 0,rO")
		  (match_operand:SI 2 "reg_or_0_operand" "rO,rO")))
   (set (match_operand:SI 3 "register_operand"           "=r,r")
	(minus:SI
	  (match_operand:SI 4 "reg_or_0_operand"         "rO,rO")
	  (ltu:SI (match_dup 1) (match_dup 2))))
   (clobber (match_scratch:SI 5 "=&r,&r"))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_subsi_co (operands[0], operands[1], operands[2]));
  emit_insn (gen_addsi_ci (operands[5], const0_rtx, const0_rtx));
  emit_insn (gen_subsi3 (operands[3], operands[4], operands[5]));
  DONE;
})

(define_insn_and_split "*subdi_2"
  [(set (match_operand:SI 0 "register_operand"           "=r,&r")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" " 0,rO")
		  (match_operand:SI 2 "reg_or_0_operand" "rO,rO")))
   (set (match_operand:SI 3 "register_operand"           "=r,r")
	(minus:SI
	  (ltu:SI (match_dup 1) (match_dup 2))
	  (match_operand:SI 4 "reg_or_0_operand"         "rO,rO")))
   (clobber (match_scratch:SI 5 "=&r,&r"))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_subsi_co (operands[0], operands[1], operands[2]));
  emit_insn (gen_addsi_ci (operands[5], const0_rtx, const0_rtx));
  emit_insn (gen_subsi3 (operands[3], operands[5], operands[4]));
  DONE;
})

(define_insn_and_split "*subdi_3"
  [(set (match_operand:SI 0 "register_operand"     "=r")
	(minus:SI
	  (match_operand:SI 1 "reg_or_0_operand"   "rO")
	  (ltu:SI
	    (match_operand:SI 2 "reg_or_0_operand" "rO")
	    (match_operand:SI 3 "reg_or_0_operand" "rO"))))
   (clobber (match_scratch:SI 4 "=&r"))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_subsi_co (operands[4], operands[2], operands[3]));
  emit_insn (gen_addsi_ci (operands[4], const0_rtx, const0_rtx));
  emit_insn (gen_subsi3 (operands[0], operands[1], operands[4]));
  DONE;
})

(define_insn_and_split "*subdi_4"
  [(set (match_operand:SI 0 "register_operand"     "=r")
	(minus:SI
	  (ltu:SI
	    (match_operand:SI 1 "reg_or_0_operand" "rO")
	    (match_operand:SI 2 "reg_or_0_operand" "rO"))
	  (match_operand:SI 3 "reg_or_0_operand"   "rO")))
   (clobber (match_scratch:SI 4 "=&r"))
   (clobber (reg:BI SR_CY_REG))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
{
  emit_insn (gen_subsi_co (operands[4], operands[1], operands[2]));
  emit_insn (gen_addsi_ci (operands[4], const0_rtx, const0_rtx));
  emit_insn (gen_subsi3 (operands[0], operands[4], operands[3]));
  DONE;
})

(define_insn "addsi_co"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI (match_operand:SI 1 "reg_or_0_operand" "%rO,rO")
		 (match_operand:SI 2 "reg_or_s16_operand" "r,I")))
   (set (reg:BI SR_CY_REG)
	(ltu:BI
	  (plus:SI (match_dup 1) (match_dup 2))
	  (match_dup 2)))]
  "reload_completed"
  "@
   l.add\t%0,%r1,%2
   l.addi\t%0,%r1,%2"
  [(set_attr "type" "add")])

(define_insn "addsi_ci"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI
	  (plus:SI
	    (ne:SI (reg:BI SR_CY_REG) (const_int 0))
	    (match_operand:SI 1 "reg_or_0_operand" "%rO,rO"))
	  (match_operand:SI 2 "reg_or_s16_operand" "r,I")))
   (clobber (reg:BI SR_CY_REG))]
  "reload_completed"
  "@
   l.addc\t%0,%r1,%2
   l.addic\t%0,%r1,%2"
  [(set_attr "type" "add")])

(define_insn "subsi_co"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(minus:SI (match_operand:SI 1 "reg_or_0_operand" "rO")
		  (match_operand:SI 2 "reg_or_0_operand" "rO")))
   (set (reg:BI SR_CY_REG)
	(ltu:BI (match_dup 1) (match_dup 2)))]
  "reload_completed"
  "l.sub\t%0,%r1,%r2"
  [(set_attr "type" "add")])

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

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (minus:SF (match_operand:SF 1 "register_operand" "r")
                 (match_operand:SF 2 "register_operand" "r")))]
  "TARGET_HARD_FLOAT"
  "lf.sub.s\t%0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (mult:SF (match_operand:SF 1 "register_operand" "r")
                 (match_operand:SF 2 "register_operand" "r")))]
  "TARGET_HARD_FLOAT"
  "lf.mul.s\t%0,%1,%2"
  [(set_attr "type" "fp")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (div:SF (match_operand:SF 1 "register_operand" "r")
		(match_operand:SF 2 "register_operand" "r")))]
  "TARGET_HARD_FLOAT"
  "lf.div.s\t%0,%1,%2"
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

;; This is a placeholder, during RA, in order to create the PIC register.
;; We do this so that we don't unconditionally mark the LINK register as
;; clobbered.  It is replaced during prologue generation with the proper
;; set_got pattern below.  This works because the set_got_tmp insn is the
;; first insn in the stream and that it isn't moved during RA.
(define_insn "set_got_tmp"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI [(const_int 0)] UNSPEC_SET_GOT))]
  ""
  { abort (); })

;; The insn to set GOT.
(define_insn "set_got"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(const_int 0)] UNSPEC_SET_GOT))
   (clobber (reg:SI 9))
   (clobber (reg:BI SR_CY_REG))]
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
