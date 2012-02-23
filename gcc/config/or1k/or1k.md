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
  (CC_REG 34)

  ;; unspec values
  (UNSPEC_FRAME 0)
  (UNSPEC_GOT 1)
  (UNSPEC_GOTOFF 2)
  (UNSPEC_PCREL 3)
  (UNSPEC_PIC_LABEL 4)
  (UNSPEC_SYMBOL_OFFSET 5)
  (UNSPEC_SET_GOT       101)

  ;; unspec_volatile values
  (UNSPECV_SIBCALL_EPILOGUE 0)
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
(define_delay (eq_attr "type" "branch,jump")
               [(and (eq_attr "type" "!branch,jump")
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


;; Called after register allocation to add any instructions needed for the
;; prologue.  Using a prologue insn is favored compared to putting all of the
;; instructions in output_function_prologue(), since it allows the scheduler
;; to intermix instructions with the saves of the caller saved registers.  In
;; some cases, it might be necessary to emit a barrier instruction as the last
;; insn to prevent such scheduling.

(define_expand "prologue"
  [(use (const_int 1))]
  "TARGET_SCHED_LOGUE"
{
  or1k_expand_prologue ();
  DONE;
})

;; Called after register allocation to add any instructions needed for the
;; epilogue.  Using an epilogue insn is favored compared to putting all of the
;; instructions in output_function_epilogue(), since it allows the scheduler
;; to intermix instructions with the restores of the caller saved registers.
;; In some cases, it might be necessary to emit a barrier instruction as the
;; first insn to prevent such scheduling.
(define_expand "epilogue"
  [(use (const_int 2))]
  "TARGET_SCHED_LOGUE"
{
  or1k_expand_epilogue (NULL_RTX);
  DONE;
})

(define_insn_and_split "sibcall_epilogue"
  [(unspec_volatile [(const_int 2)] UNSPECV_SIBCALL_EPILOGUE)]
  "TARGET_SCHED_LOGUE"
  "#"
  ""
  [(pc)]
{
  or1k_expand_epilogue (curr_insn);
  DONE;
})

(define_insn "frame_alloc_fp"
  [(set (reg:SI SP_REG)
	(plus:SI (reg:SI SP_REG)
		 (match_operand:SI 0 "nonmemory_operand" "r,I")))
   (clobber (mem:QI (plus:SI (reg:SI FP_REG)
			     (unspec:SI [(const_int FP_REG)] UNSPEC_FRAME))))]
  ""
  "@
   l.add\tr1,r1,%0\t# allocate frame
   l.addi\tr1,r1,%0\t# allocate frame"
  [(set_attr "type" "add")
   (set_attr "length" "1")])

(define_insn "frame_dealloc_fp"
  [(set (reg:SI SP_REG) (reg:SI FP_REG))
   (clobber (mem:QI (plus:SI (reg:SI FP_REG)
			     (unspec:SI [(const_int FP_REG)] UNSPEC_FRAME))))]
  ""
  "l.ori\tr1,r2,0\t# deallocate frame"
  [(set_attr "type" "logic")
   (set_attr "length" "1")])

(define_insn "frame_dealloc_sp"
  [(set (reg:SI SP_REG)
	(plus:SI (reg:SI SP_REG)
		 (match_operand:SI 0 "nonmemory_operand" "r,I")))
   (clobber (mem:QI (plus:SI (reg:SI SP_REG)
			     (unspec:SI [(const_int SP_REG)] UNSPEC_FRAME))))]
  ""
  "@
   l.add\tr1,r1,%0
   l.addi\tr1,r1,%0"
  [(set_attr "type" "add")
   (set_attr "length" "1")])

(define_insn "return_internal"
  [(return)
   (use (match_operand 0 "pmode_register_operand" ""))]
  "TARGET_SCHED_LOGUE"
  "l.jr    \t%0%("
  [(set_attr "type" "jump")
   (set_attr "length" "1")])

;;
;; Sibcalls
;;

(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "" "")
                    (match_operand 1 "" ""))
              (use (match_operand 2 "" ""))     ;; next_arg_reg
              (use (match_operand 3 "" ""))])]  ;; struct_value_size_rtx
  ""
  "
{
  or1k_expand_sibcall (0, XEXP (operands[0], 0), operands[1]); 
  DONE;
}")

(define_expand "sibcall_value"
  [(set (match_operand 0 "" "")
                   (call (match_operand:SI 1 "" "")
                         (match_operand 2 "" "")))]
  ""
  "
{
  or1k_expand_sibcall (operands[0], XEXP (operands[1], 0), operands[2]); 
  DONE; 
}")

(define_insn "sibcall_internal"
  [(call (mem:SI (match_operand:SI 0 "sibcall_insn_operand" "s,Rsc,r"))
         (match_operand 1 "" ""))
   (use (reg:SI 9))]
  ""
  "@
   l.j\t%S0%(\t# sibcall s
   l.jr\t%0%(\t# sibcall Rsc
   l.jr\t%0\t\t# sibcall r%J0"
  [(set_attr "type" "jump,jump,jump_restore")])



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

	      emit_insn (gen_rtx_SET (SImode, reg,
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
   l.sb    \t%0,%1\t    # movqi
   l.ori   \t%0,%1,0\t  # movqi: move reg to reg
   l.addi  \t%0,r0,%1\t # movqi: move immediate
   l.ori   \t%0,r0,%1\t # movqi: move immediate
   l.lbz   \t%0,%1\t    # movqi"
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

	      emit_insn (gen_rtx_SET (SImode, reg,
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
   l.sh    \t%0,%1\t # movhi
   l.ori   \t%0,%1,0\t # movhi: move reg to reg
   l.addi  \t%0,r0,%1\t # movhi: move immediate
   l.ori   \t%0,r0,%1\t # movhi: move immediate
   l.lhz   \t%0,%1\t # movhi"
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
  l.addi  \t%0,r0,%1\t # move immediate I
  l.ori   \t%0,r0,%1\t # move immediate K
  l.movhi \t%0,hi(%1)\t # move immediate M
  l.ori   \t%0,%1,0\t # move reg to reg
  l.lwz   \t%0,%1\t # SI load
  l.sw    \t%0,%1\t # SI store"
  [(set_attr "type" "add,load,store,add,logic,move")
   (set_attr "length" "1,1,1,1,1,1")])

(define_insn "movsi_lo_sum"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_operand:SI 1 "register_operand" "r")
                   (match_operand:SI 2 "immediate_operand" "i")))]
  ""
  "l.ori   \t%0,%1,lo(%2) # movsi_lo_sum"
 [(set_attr "type" "logic")
   (set_attr "length" "1")])

(define_insn "movsi_high"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand:SI 1 "immediate_operand" "i")))]
  ""
  "l.movhi  \t%0,hi(%1)"
[(set_attr "type" "move")
   (set_attr "length" "1")])

(define_insn_and_split "movsi_insn_big"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(match_operand:SI 1 "immediate_operand" "i"))]
  "(GET_CODE (operands[1]) != CONST_INT
    || ! (CONST_OK_FOR_CONSTRAINT_P (INTVAL (operands[1]), 'I', \"I\")
	  || CONST_OK_FOR_CONSTRAINT_P (INTVAL (operands[1]), 'K', \"K\")
	  || CONST_OK_FOR_CONSTRAINT_P (INTVAL (operands[1]), 'M', \"M\")))"
  "l.movhi \t%0,hi(%1)\;l.ori   \t%0,%0,lo(%1)"
  ;; the switch of or1k bfd to Rela allows us to schedule insns separately.
  "&& reload_completed
   && GET_CODE (operands[1]) != HIGH && GET_CODE (operands[1]) != LO_SUM"
  [(pc)]
{
  emit_insn (gen_movsi_high (operands[0], operands[1]));
  emit_insn (gen_movsi_lo_sum (operands[0], operands[0], operands[1]));
  DONE;
}
  [(set_attr "type" "move")
   (set_attr "length" "2")])


;;
;; Conditional Branches & Moves
;; 

(define_expand "addsicc"
  [(match_operand:SI 0 "register_operand" "")
   (match_operand 1 "comparison_operator" "")
   (match_operand:SI 2 "register_operand" "")
   (match_operand:SI 3 "register_operand" "")]
  ""
  "FAIL;")

(define_expand "addhicc"
  [(match_operand:HI 0 "register_operand" "")
   (match_operand 1 "comparison_operator" "")
   (match_operand:HI 2 "register_operand" "")
   (match_operand:HI 3 "register_operand" "")]
  ""
  "FAIL;")

(define_expand "addqicc"
  [(match_operand:QI 0 "register_operand" "")
   (match_operand 1 "comparison_operator" "")
   (match_operand:QI 2 "register_operand" "")
   (match_operand:QI 3 "register_operand" "")]
  ""
  "FAIL;")


;;
;; conditional moves
;;

(define_expand "movsicc"
   [(set (match_operand:SI 0 "register_operand" "")
	 (if_then_else:SI (match_operand 1 "comparison_operator" "")
			  (match_operand:SI 2 "register_operand" "")
			  (match_operand:SI 3 "register_operand" "")))]
  "TARGET_MASK_CMOV"
  "
{
  if (or1k_emit_cmove (operands[0], operands[1], operands[2], operands[3]))
    DONE;
}")

(define_expand "movhicc"
   [(set (match_operand:HI 0 "register_operand" "")
	 (if_then_else:SI (match_operand 1 "comparison_operator" "")
			  (match_operand:HI 2 "register_operand" "")
			  (match_operand:HI 3 "register_operand" "")))]
  ""
  "
{
    FAIL;
}")

(define_expand "movqicc"
   [(set (match_operand:QI 0 "register_operand" "")
	 (if_then_else:SI (match_operand 1 "comparison_operator" "")
			  (match_operand:QI 2 "register_operand" "")
			  (match_operand:QI 3 "register_operand" "")))]
  ""
  "
{
    FAIL;
}")


;; We use the BASE_REGS for the cmov input operands because, if rA is
;; 0, the value of 0 is placed in rD upon truth.  Similarly for rB
;; because we may switch the operands and rB may end up being rA.

(define_insn "cmov"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(if_then_else:SI
	 (match_operator 1 "comparison_operator"
			 [(match_operand 4 "cc_reg_operand" "")
			  (const_int 0)])
	 (match_operand:SI 2 "register_operand" "r")
	 (match_operand:SI 3 "register_operand" "r")))]
  "TARGET_MASK_CMOV"
  "*
   return or1k_output_cmov(operands);
  ")

;;
;;  ....................
;;
;;	COMPARISONS
;;
;;  ....................

;; Flow here is rather complex:
;;
;;  1)	The cmp{si,di,sf,df} routine is called.  It deposits the
;;	arguments into the branch_cmp array, and the type into
;;	branch_type.  No RTL is generated.
;;
;;  2)	The appropriate branch define_expand is called, which then
;;	creates the appropriate RTL for the comparison and branch.
;;	Different CC modes are used, based on what type of branch is
;;	done, so that we can constrain things appropriately.  There
;;	are assumptions in the rest of GCC that break if we fold the
;;	operands into the branches for integer operations, and use cc0
;;	for floating point, so we use the fp status register instead.
;;	If needed, an appropriate temporary is created to hold the
;;	of the integer compare.

;; Compare insns are next.  Note that the RS/6000 has two types of compares,
;; signed & unsigned, and one type of branch.
;;
;; Start with the DEFINE_EXPANDs to generate the rtl for compares, scc
;; insns, and branches.  We store the operands of compares until we see
;; how it is used.

;; JPB 31-Aug-10: cmpxx appears to be obsolete in GCC 4.5. Needs more
;; investigation.

;;(define_expand "cmpsi"
;;  [(set (reg:CC CC_REG)
;;	(compare:CC (match_operand:SI 0 "register_operand" "")
;;		    (match_operand:SI 1 "nonmemory_operand" "")))]
;;  ""
;;  {
;;   if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
;;      operands[0] = force_reg (SImode, operands[0]);
;;      or1k_compare_op0 = operands[0];
;;     or1k_compare_op1 = operands[1];
;;      DONE;
;;      })

;; (define_expand "cmpsf"
;;   [(set (reg:CC CC_REG)
;; 	(compare:CC (match_operand:SF 0 "register_operand" "")
;; 		    (match_operand:SF 1 "register_operand" "")))]
;;   "TARGET_HARD_FLOAT"
;;   {
;;    if (GET_CODE (operands[0]) == MEM && GET_CODE (operands[1]) == MEM)
;;       operands[0] = force_reg (SFmode, operands[0]);
;;       or1k_compare_op0 = operands[0];
;;       or1k_compare_op1 = operands[1];
;;       DONE;
;;       })

(define_expand "cbranchsi4"
  [(match_operator 0 "comparison_operator"
    [(match_operand:SI 1 "register_operand")
     (match_operand:SI 2 "nonmemory_operand")])
   (match_operand 3 "")]
   ""
   {
   or1k_expand_conditional_branch (operands, SImode);
   DONE;
   })

(define_expand "cbranchsf4"
  [(match_operator 0 "comparison_operator"
    [(match_operand:SI 1 "register_operand")
     (match_operand:SI 2 "register_operand")])
   (match_operand 3 "")]
   "TARGET_HARD_FLOAT"
   {
   or1k_expand_conditional_branch (operands, SFmode);
   DONE;
   })

;;
;; Setting a CCxx registers from comparision
;;



;; Here are the actual compare insns.
(define_insn "*cmpsi_eq"
  [(set (reg:CCEQ CC_REG)
	(compare:CCEQ (match_operand:SI 0 "register_operand" "r,r")
		      (match_operand:SI 1 "nonmemory_operand" "I,r")))]
  ""
  "@
   l.sfeqi\t%0,%1
   l.sfeq \t%0,%1")

(define_insn "*cmpsi_ne"
  [(set (reg:CCNE CC_REG)
	(compare:CCNE (match_operand:SI 0 "register_operand" "r,r")
		      (match_operand:SI 1 "nonmemory_operand" "I,r")))]
  ""
  "@
   l.sfnei\t%0,%1
   l.sfne \t%0,%1")

(define_insn "*cmpsi_gt"
  [(set (reg:CCGT CC_REG)
	(compare:CCGT (match_operand:SI 0 "register_operand" "r,r")
		      (match_operand:SI 1 "nonmemory_operand" "I,r")))]
  ""
  "@
   l.sfgtsi\t%0,%1
   l.sfgts \t%0,%1")

(define_insn "*cmpsi_gtu"
  [(set (reg:CCGTU CC_REG)
	(compare:CCGTU (match_operand:SI 0 "register_operand" "r,r")
		       (match_operand:SI 1 "nonmemory_operand" "I,r")))]
  ""
  "@
   l.sfgtui\t%0,%1
   l.sfgtu \t%0,%1")

(define_insn "*cmpsi_lt"
  [(set (reg:CCLT CC_REG)
	(compare:CCLT (match_operand:SI 0 "register_operand" "r,r")
		      (match_operand:SI 1 "nonmemory_operand" "I,r")))]
  ""
  "@
   l.sfltsi\t%0,%1
   l.sflts \t%0,%1")

(define_insn "*cmpsi_ltu"
  [(set (reg:CCLTU CC_REG)
	(compare:CCLTU (match_operand:SI 0 "register_operand" "r,r")
		       (match_operand:SI 1 "nonmemory_operand" "I,r")))]
  ""
  "@
   l.sfltui\t%0,%1
   l.sfltu \t%0,%1")

(define_insn "*cmpsi_ge"
  [(set (reg:CCGE CC_REG)
	(compare:CCGE (match_operand:SI 0 "register_operand" "r,r")
		      (match_operand:SI 1 "nonmemory_operand" "I,r")))]
  ""
  "@
   l.sfgesi\t%0,%1
   l.sfges \t%0,%1")


(define_insn "*cmpsi_geu"
  [(set (reg:CCGEU CC_REG)
	(compare:CCGEU (match_operand:SI 0 "register_operand" "r,r")
		       (match_operand:SI 1 "nonmemory_operand" "I,r")))]
  ""
  "@
   l.sfgeui\t%0,%1
   l.sfgeu \t%0,%1")


(define_insn "*cmpsi_le"
  [(set (reg:CCLE CC_REG)
	(compare:CCLE (match_operand:SI 0 "register_operand" "r,r")
		      (match_operand:SI 1 "nonmemory_operand" "I,r")))]
  ""
  "@
   l.sflesi\t%0,%1
   l.sfles \t%0,%1")

(define_insn "*cmpsi_leu"
  [(set (reg:CCLEU CC_REG)
	(compare:CCLEU (match_operand:SI 0 "register_operand" "r,r")
		       (match_operand:SI 1 "nonmemory_operand" "I,r")))]
  ""
  "@
   l.sfleui\t%0,%1
   l.sfleu \t%0,%1")

;; Single precision floating point evaluation instructions
(define_insn "*cmpsf_eq"
  [(set (reg:CCEQ CC_REG)
	(compare:CCEQ (match_operand:SF 0 "register_operand" "r,r")
		      (match_operand:SF 1 "register_operand" "r,r")))]
  "TARGET_HARD_FLOAT"
  "lf.sfeq.s\t%0,%1")

(define_insn "*cmpsf_ne"
  [(set (reg:CCNE CC_REG)
	(compare:CCNE (match_operand:SF 0 "register_operand" "r,r")
		      (match_operand:SF 1 "register_operand" "r,r")))]
  "TARGET_HARD_FLOAT"
  "lf.sfne.s\t%0,%1")


(define_insn "*cmpsf_gt"
  [(set (reg:CCGT CC_REG)
	(compare:CCGT (match_operand:SF 0 "register_operand" "r,r")
		      (match_operand:SF 1 "register_operand" "r,r")))]
  "TARGET_HARD_FLOAT"
  "lf.sfgt.s\t%0,%1")

(define_insn "*cmpsf_ge"
  [(set (reg:CCGE CC_REG)
	(compare:CCGE (match_operand:SF 0 "register_operand" "r,r")
		      (match_operand:SF 1 "register_operand" "r,r")))]
  "TARGET_HARD_FLOAT"
  "lf.sfge.s\t%0,%1")


(define_insn "*cmpsf_lt"
  [(set (reg:CCLT CC_REG)
	(compare:CCLT (match_operand:SF 0 "register_operand" "r,r")
		      (match_operand:SF 1 "register_operand" "r,r")))]
  "TARGET_HARD_FLOAT"
  "lf.sflt.s\t%0,%1")

(define_insn "*cmpsf_le"
  [(set (reg:CCLE CC_REG)
	(compare:CCLE (match_operand:SF 0 "register_operand" "r,r")
		      (match_operand:SF 1 "register_operand" "r,r")))]
  "TARGET_HARD_FLOAT"
  "lf.sfle.s\t%0,%1")

(define_insn "*bf"
  [(set (pc)
	(if_then_else (match_operator 1 "comparison_operator"
				      [(match_operand 2
						      "cc_reg_operand" "")
				       (const_int 0)])
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "*
   return or1k_output_bf(operands);
  "
  [(set_attr "type" "branch")
   (set_attr "length" "1")])

;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;
(define_insn_and_split "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r, r, m, r")
	(match_operand:DI 1 "general_operand"      " r, m, r, n"))]
  ""
  "*
    return or1k_output_move_double (operands);
  "
  "&& reload_completed && CONSTANT_P (operands[1])"
  [(set (match_dup 2) (match_dup 3)) (set (match_dup 4) (match_dup 5))]
  "operands[2] = operand_subword (operands[0], 0, 0, DImode);
   operands[3] = operand_subword (operands[1], 0, 0, DImode);
   operands[4] = operand_subword (operands[0], 1, 0, DImode);
   operands[5] = operand_subword (operands[1], 1, 0, DImode);"
  [(set_attr "length" "2,2,2,3")])

;; Moving double and single precision floating point values


(define_insn "movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "=r, r, m, r")
	(match_operand:DF 1 "general_operand"      " r, m, r, i"))]
  ""
  "*
    return or1k_output_move_double (operands);
  "
  [(set_attr "length" "2,2,2,3")])


(define_insn "movsf"
  [(set (match_operand:SF 0 "general_operand" "=r,r,m")
        (match_operand:SF 1 "general_operand"  "r,m,r"))]
  ""
  "@
   l.ori   \t%0,%1,0\t # movsf
   l.lwz   \t%0,%1\t # movsf
   l.sw    \t%0,%1\t # movsf"
  [(set_attr "type" "move,load,store")
   (set_attr "length" "1,1,1")])


;;
;; extendqisi2
;;

(define_expand "extendqisi2"
  [(use (match_operand:SI 0 "register_operand" ""))
   (use (match_operand:QI 1 "nonimmediate_operand" ""))]
  ""
  "
{
  if (TARGET_MASK_SEXT)
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
}")

(define_insn "extendqisi2_sext"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(sign_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_MASK_SEXT"
  "@
   l.extbs \t%0,%1\t # extendqisi2_has_signed_extend
   l.lbs   \t%0,%1\t # extendqisi2_has_signed_extend"
  [(set_attr "length" "1,1")
   (set_attr "type" "extend,load")])

(define_insn "extendqisi2_no_sext_mem"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI (match_operand:QI 1 "memory_operand" "m")))]
  "!TARGET_MASK_SEXT"
  "l.lbs   \t%0,%1\t # extendqisi2_no_sext_mem"
  [(set_attr "length" "1")
   (set_attr "type" "load")])

(define_expand "extendqisi2_no_sext_reg"
  [(set (match_dup 2)
	(ashift:SI (match_operand:QI 1 "register_operand" "")
		   (const_int 24)))
   (set (match_operand:SI 0 "register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 24)))]
  "!TARGET_MASK_SEXT"
  "
{
  operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode); }")

;;
;; extendhisi2
;;

(define_expand "extendhisi2"
  [(use (match_operand:SI 0 "register_operand" ""))
   (use (match_operand:HI 1 "nonimmediate_operand" ""))]
  ""
  "
{
  if (TARGET_MASK_SEXT)
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
}")

(define_insn "extendhisi2_sext"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,m")))]
  "TARGET_MASK_SEXT"
  "@
   l.exths \t%0,%1\t # extendhisi2_has_signed_extend
   l.lhs   \t%0,%1\t # extendhisi2_has_signed_extend"
  [(set_attr "length" "1,1")
   (set_attr "type" "extend,load")])

(define_insn "extendhisi2_no_sext_mem"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI (match_operand:HI 1 "memory_operand" "m")))]
  "!TARGET_MASK_SEXT"
  "l.lhs   \t%0,%1\t # extendhisi2_no_sext_mem"
  [(set_attr "length" "1")
   (set_attr "type" "load")])

(define_expand "extendhisi2_no_sext_reg"
  [(set (match_dup 2)
	(ashift:SI (match_operand:HI 1 "register_operand" "")
		   (const_int 16)))
   (set (match_operand:SI 0 "register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 16)))]
  "!TARGET_MASK_SEXT"
  "
{
  operands[1] = gen_lowpart (SImode, operands[1]);
  operands[2] = gen_reg_rtx (SImode); }")


;;
;; zero_extend<m><n>2
;;

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   l.andi  \t%0,%1,0xff\t # zero_extendqisi2
   l.lbz   \t%0,%1\t # zero_extendqisi2"
  [(set_attr "type" "logic,load")
   (set_attr "length" "1,1")])


(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "r,m")))]
  ""
  "@
   l.andi  \t%0,%1,0xffff\t # zero_extendqisi2
   l.lhz   \t%0,%1\t # zero_extendqisi2"
  [(set_attr "type" "logic,load")
   (set_attr "length" "1,1")])

;;
;; Shift/rotate operations
;;

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (ashift:SI (match_operand:SI 1 "register_operand" "r,r")
                   (match_operand:SI 2 "nonmemory_operand" "r,L")))]
  ""
  "@
   l.sll   \t%0,%1,%2
   l.slli  \t%0,%1,%2"
  [(set_attr "type" "shift,shift")
   (set_attr "length" "1,1")])

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (ashiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
                   (match_operand:SI 2 "nonmemory_operand" "r,L")))]
  ""
  "@
   l.sra   \t%0,%1,%2
   l.srai  \t%0,%1,%2"
  [(set_attr "type" "shift,shift")
   (set_attr "length" "1,1")])

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (lshiftrt:SI (match_operand:SI 1 "register_operand" "r,r")
                   (match_operand:SI 2 "nonmemory_operand" "r,L")))]
  ""
  "@
   l.srl   \t%0,%1,%2
   l.srli  \t%0,%1,%2"
  [(set_attr "type" "shift,shift")
   (set_attr "length" "1,1")])

(define_insn "rotrsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
        (rotatert:SI (match_operand:SI 1 "register_operand" "r,r")
                   (match_operand:SI 2 "nonmemory_operand" "r,L")))]
  "TARGET_MASK_ROR"
  "@
   l.ror   \t%0,%1,%2
   l.rori  \t%0,%1,%2"
  [(set_attr "type" "shift,shift")
   (set_attr "length" "1,1")])

;;
;; Logical bitwise operations
;;

(define_insn "andsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(and:SI (match_operand:SI 1 "register_operand" "%r,r")
		(match_operand:SI 2 "nonmemory_operand" "r,K")))]
  ""
  "@
   l.and   \t%0,%1,%2
   l.andi  \t%0,%1,%2"
  [(set_attr "type" "logic,logic")
   (set_attr "length" "1,1")])

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(ior:SI (match_operand:SI 1 "register_operand" "%r,r")
		(match_operand:SI 2 "nonmemory_operand" "r,K")))]
  ""
  "@
   l.or    \t%0,%1,%2
   l.ori   \t%0,%1,%2"
  [(set_attr "type" "logic,logic")
   (set_attr "length" "1,1")])

(define_insn "xorsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(xor:SI (match_operand:SI 1 "register_operand" "%r,r")
		(match_operand:SI 2 "nonmemory_operand" "r,I")))]
  ""
  "@
   l.xor   \t%0,%1,%2
   l.xori  \t%0,%1,%2"
  [(set_attr "type" "logic,logic")
   (set_attr "length" "1,1")])

(define_insn "one_cmplqi2"
  [(set (match_operand:QI 0 "register_operand" "=r")
	(not:QI (match_operand:QI 1 "register_operand" "r")))]
  ""
  "l.xori  \t%0,%1,0x00ff"
  [(set_attr "type" "logic")
   (set_attr "length" "1")])

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(not:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "l.xori  \t%0,%1,0xffff"
  [(set_attr "type" "logic")
   (set_attr "length" "1")])

;;
;; Arithmetic operations 
;;

(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(neg:SI (match_operand:SI 1 "register_operand" "r")))]
  ""
  "l.sub   \t%0,r0,%1"
  [(set_attr "type" "add")
   (set_attr "length" "1")])

(define_insn "addsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(plus:SI (match_operand:SI 1 "register_operand" "%r,r")
		 (match_operand:SI 2 "nonmemory_operand" "r,I")))]
  ""
  "@
   l.add   \t%0,%1,%2 # addsi3
   l.addi  \t%0,%1,%2 # addsi3"
  [(set_attr "type" "add,add")
   (set_attr "length" "1,1")])

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(minus:SI (match_operand:SI 1 "register_operand" "r,r")
		  (match_operand:SI 2 "nonmemory_operand" "r,I")))]
  ""
  "@
   l.sub   \t%0,%1,%2
   l.addi  \t%0,%1,%n2"
  [(set_attr "type" "add,add")]
)

;;
;; mul and div
;;

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (mult:SI (match_operand:SI 1 "register_operand" "r")
                 (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_HARD_MUL"
  "l.mul   \t%0,%1,%2"
  [(set_attr "type" "mul")
   (set_attr "length" "1")])

(define_insn "divsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (div:SI (match_operand:SI 1 "register_operand" "r")
                 (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_HARD_DIV"
  "l.div   \t%0,%1,%2"
  [(set_attr "type" "mul")
   (set_attr "length" "1")])

(define_insn "udivsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (udiv:SI (match_operand:SI 1 "register_operand" "r")
                 (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_HARD_DIV"
  "l.divu  \t%0,%1,%2"
  [(set_attr "type" "mul")
   (set_attr "length" "1")])

;;
;; jumps 
;;

;; jump

(define_expand "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "
{
  emit_jump_insn (gen_jump_internal (operands[0]));
  DONE;
}")

(define_insn "jump_internal"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  "l.j     \t%l0%("
  [(set_attr "type" "jump")
   (set_attr "length" "1")])

;; indirect jump

(define_expand "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "
{
  emit_jump_insn (gen_indirect_jump_internal (operands[0]));
  DONE;

}")

(define_insn "indirect_jump_internal"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "l.jr    \t%0%("
  [(set_attr "type" "jump")
   (set_attr "length" "1")])

;;
;; calls
;;

;; call

(define_expand "call"
  [(parallel [(call (match_operand:SI 0 "sym_ref_mem_operand" "")
		    (match_operand 1 "" "i"))
	      (clobber (reg:SI 9))])]
  ""
  "
{
  emit_call_insn (gen_call_internal (operands[0], operands[1]));
  DONE;
}")

(define_insn "call_internal"
[(parallel [(call (match_operand:SI 0 "sym_ref_mem_operand" "")
                  (match_operand 1 "" "i"))
            (clobber (reg:SI 9))])]
  ""
  "l.jal   \t%S0%("
  [(set_attr "type" "jump")
   (set_attr "length" "1")])

;; call value

(define_expand "call_value"
  [(parallel [(set (match_operand 0 "register_operand" "=r")
		   (call (match_operand:SI 1 "sym_ref_mem_operand" "")
			 (match_operand 2 "" "i")))
	      (clobber (reg:SI 9))])]
  ""
  "
{
  emit_call_insn (gen_call_value_internal (operands[0], operands[1], operands[2]));
  DONE;
}")

(define_insn "call_value_internal"
[(parallel [(set (match_operand 0 "register_operand" "=r")
                  (call (match_operand:SI 1 "sym_ref_mem_operand" "")
                        (match_operand 2 "" "i")))
            (clobber (reg:SI 9))])]
  ""
  "l.jal   \t%S1%("
  [(set_attr "type" "jump")
   (set_attr "length" "1")])

;; indirect call value 

(define_expand "call_value_indirect"
  [(parallel [(set (match_operand 0 "register_operand" "=r")
                   (call (mem:SI (match_operand:SI 1 "register_operand" "r"))
                         (match_operand 2 "" "i")))
              (clobber (reg:SI 9))])]
  ""
  "
{
  emit_call_insn (gen_call_value_indirect_internal (operands[0], operands[1], operands[2]));
  DONE;
}")

(define_insn "call_value_indirect_internal"
  [(parallel [(set (match_operand 0 "register_operand" "=r")
                   (call (mem:SI (match_operand:SI 1 "register_operand" "r"))
                         (match_operand 2 "" "i")))
              (clobber (reg:SI 9))])]
  ""
  "l.jalr  \t%1%("
  [(set_attr "type" "jump")
   (set_attr "length" "1")])

;; indirect call

(define_expand "call_indirect"
  [(parallel [(call (mem:SI (match_operand:SI 0 "register_operand" "r"))
		    (match_operand 1 "" "i"))
              (clobber (reg:SI 9))])]
  ""
  "
{
  emit_call_insn (gen_call_indirect_internal (operands[0], operands[1]));
  DONE;
}")

(define_insn "call_indirect_internal"
[(parallel [(call (mem:SI (match_operand:SI 0 "register_operand" "r"))
                  (match_operand 1 "" "i"))
              (clobber (reg:SI 9))])]
  ""
  "l.jalr  \t%0%("
  [(set_attr "type" "jump")
   (set_attr "length" "1")])

;; table jump

(define_expand "tablejump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
   ""
  "
{
  if (CASE_VECTOR_PC_RELATIVE || flag_pic)
    operands[0]
      = force_reg (Pmode,
		   gen_rtx_PLUS (Pmode, operands[0],
				 gen_rtx_LABEL_REF (Pmode, operands[1])));
  emit_jump_insn (gen_tablejump_internal (operands[0], operands[1]));
  DONE;
}")

(define_insn "tablejump_internal"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))
   (use (label_ref (match_operand 1 "" "")))]
  ""
  "l.jr    \t%0%("
  [(set_attr "type" "jump")
   (set_attr "length" "1")])


;; no-op

(define_insn "nop"
  [(const_int 0)]
  ""
  "l.nop"
  [(set_attr "type" "logic")
   (set_attr "length" "1")])

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
  [(set_attr "type"     "fp")
   (set_attr "length"   "1")])
   
(define_insn "adddf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (plus:DF (match_operand:DF 1 "register_operand" "r")
                 (match_operand:DF 2 "register_operand" "r")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "lf.add.d\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "length"   "1")])

(define_insn "subsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (minus:SF (match_operand:SF 1 "register_operand" "r")
                 (match_operand:SF 2 "register_operand" "r")))]
  "TARGET_HARD_FLOAT"
  "lf.sub.s\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "length"   "1")])

(define_insn "subdf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (minus:DF (match_operand:DF 1 "register_operand" "r")
		  (match_operand:DF 2 "register_operand" "r")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "lf.sub.d\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "length"   "1")])

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (mult:SF (match_operand:SF 1 "register_operand" "r")
                 (match_operand:SF 2 "register_operand" "r")))]
  "TARGET_HARD_FLOAT"
  "lf.mul.s\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "length"   "1")])

(define_insn "muldf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (mult:DF (match_operand:DF 1 "register_operand" "r")
                 (match_operand:DF 2 "register_operand" "r")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "lf.mul.d\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "length"   "1")])

(define_insn "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (div:SF (match_operand:SF 1 "register_operand" "r")
		(match_operand:SF 2 "register_operand" "r")))]
  "TARGET_HARD_FLOAT"
  "lf.div.s\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "length"   "1")])

(define_insn "divdf3"
  [(set (match_operand:DF 0 "register_operand" "=r")
        (div:DF (match_operand:DF 1 "register_operand" "r")
		(match_operand:DF 2 "register_operand" "r")))]
  "TARGET_HARD_FLOAT && TARGET_DOUBLE_FLOAT"
  "lf.div.d\t%0,%1,%2"
  [(set_attr "type"     "fp")
   (set_attr "length"   "1")])

;; Conversion between fixed point and floating point.


(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(float:SF (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_HARD_FLOAT"
  "lf.itof.s\t%0, %1"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

;; not working 
(define_insn "fixunssfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(fix:SI (match_operand:SF 1 "register_operand" "r")))]
  "TARGET_HARD_FLOAT"
  "lf.ftoi.s\t%0, %1"
  [(set_attr "type" "fp")
   (set_attr "length" "1")])

;; The insn to set GOT.
(define_insn "set_got"
  [(set (match_operand:SI 0 "register_operand" "=r")
    (unspec:SI [(const_int 0)] UNSPEC_SET_GOT))]
  ""
  "l.jal 2
\tl.movhi r10,hi(_GLOBAL_OFFSET_TABLE_@GOTPC+1)
\tl.addi   r10,r10,lo(_GLOBAL_OFFSET_TABLE_@GOTPC)
\tl.add   r10,r10,r9"
  [(set_attr "length" "16")])


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
