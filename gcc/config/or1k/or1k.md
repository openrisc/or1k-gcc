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
;; Moxie specific constraints, predicates and attributes
;; -------------------------------------------------------------------------

(include "constraints.md")
(include "predicates.md")

;; Register numbers
(define_constants
  [(LR_REGNUM       9)
   (CC_REGNUM      33)]
)

; Most instructions are 4 bytes long.
(define_attr "length" "" (const_int 4))

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
	   (match_operand:SI 1 "register_operand" "r,r")
	   (match_operand:SI 2 "general_operand" "r,J")))]
  ""
  "@
  l.add\t%0, %1, %2
  l.addi\t%0, %1, %2")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (minus:SI
	   (match_operand:SI 1 "register_operand" "r")
	   (match_operand:SI 2 "register_operand" "r")))]
  ""
  "l.sub\t%0, %1, %2")

;; -------------------------------------------------------------------------
;; Unary arithmetic instructions
;; -------------------------------------------------------------------------

;; -------------------------------------------------------------------------
;; Logical operators
;; -------------------------------------------------------------------------

;; -------------------------------------------------------------------------
;; Move instructions
;; -------------------------------------------------------------------------

;; 8-bit moves

(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  "
{
  if (MEM_P (operands[0]))
    operands[1] = force_reg (QImode, operands[1]);
}")

(define_insn "*movqi_internal"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,W,r")
	(match_operand:QI 1 "general_operand"       "r,J,r,W"))]
  "register_operand (operands[0], QImode) || register_operand (operands[1], QImode)"
  "@
   l.or\t%0, r0, %1
   l.ori\t%0, r0, %1
   l.sb\t%0, %1
   l.lbz\t%0, %1")

;; 16-bit moves

(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  "
{
  if (MEM_P (operands[0]))
    operands[1] = force_reg (HImode, operands[1]);
}")

(define_insn "*movhi_internal"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,W,r")
	(match_operand:HI 1 "general_operand"       "r,J,r,W"))]
  "register_operand (operands[0], HImode) || register_operand (operands[1], HImode)"
  "@
   l.or\t%0, r0, %1
   l.ori\t%0, r0, %1
   l.sh\t%0, %1
   l.lhz\t%0, %1")

;; 32-bit moves

(define_expand "movsi"
  [(set (match_operand:SI 0 "" "")
	(match_operand:SI 1 "" ""))]
  ""
  "
{
  if (MEM_P (operands[0]))
    operands[1] = force_reg (SImode, operands[1]);
}")

(define_insn "*movsi_internal"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,r,W,r,r")
	(match_operand:SI 1 "general_operand"       "r,I,J,K,r,W,i"))]
  "register_operand (operands[0], SImode) || register_operand (operands[1], SImode)"
  "@
   l.or\t%0, r0, %1
   l.movhi\t%0, %1
   l.ori\t%0, r0, %1
   l.movhi\t%0, %1
   l.sw\t%0, %1
   l.lwz\t%0, %1
   l.movhi\t%0, hi(%1)\n\tl.ori\t%0, %0, lo(%1)")


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

(define_code_iterator cond [ne eq lt ltu gt gtu ge le geu leu])
(define_code_attr insn [(ne "ne") (eq "eq") (lt "lts") (ltu "ltu")
			(gt "gts") (gtu "gtu") (ge "ges") (le "les")
			(geu "geu") (leu "leu") ])

(define_insn "sf<code>_insn"
  [(set (reg:CC CC_REGNUM)
	(cond (match_operand:SI 0 "register_operand" "r,r")
	      (match_operand:SI 1 "general_operand" "r,J")))]
  ""
  "@
   l.sf<insn>\t%0, %1
   l.sf<insn>i\t%0, %1")

;; -------------------------------------------------------------------------
;; Branch instructions
;; -------------------------------------------------------------------------

(define_insn "*cbranchsi4_internal"
  [(set (pc)
	(if_then_else (ne (reg:CC CC_REGNUM) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "l.bf\t%0")


(define_expand "cbranchsi4"
  [(set (reg:CC CC_REGNUM)
	(match_operator 0 "comparison_operator"
	  [(match_operand:SI 1 "register_operand" "")
	   (match_operand:SI 2 "general_operand" "")]))
   (set (pc)
	(if_then_else (ne (reg:CC CC_REGNUM) (const_int 0))
		      (label_ref (match_operand 3 "" ""))
		      (pc)))]
  ""
  "")

;; -------------------------------------------------------------------------
;; Call and Jump instructions
;; -------------------------------------------------------------------------
(define_insn "jump"
  [(set (pc) (label_ref (match_operand 0 "" "")))]
  ""
  "l.j\t%0")

(define_insn "indirect_jump"
  [(set (pc) (match_operand:SI 0 "register_operand" "r"))]
  ""
  "l.jr\t%0")

(define_expand "call"
  [(parallel [(call (match_operand 0 "" "")
		    (match_operand 1 "" ""))
	      (clobber (reg:SI LR_REGNUM))
	     ])]
  ""
  "
{
  rtx addr = XEXP (operands[0], 0);
  if (!CONSTANT_ADDRESS_P (addr))
    XEXP (operands[0], 0) = force_reg (Pmode, addr);
}")

(define_insn "*call"
  [(call (mem:SI (match_operand:SI 0 "general_operand" "r,i"))
	 (match_operand 1 "" ""))
   (clobber (reg:SI LR_REGNUM))]
  ""
  "@
   l.jalr\t%0
   l.jal\t%0")

;; Call with a retun value
(define_expand "call_value"
  [(parallel [(set (match_operand 0 "" "")
		   (call (match_operand 1 "" "")
		   (match_operand 2 "" "")))
	      (clobber (reg:SI LR_REGNUM))
             ])]
  ""
  "
{
  rtx addr = XEXP (operands[1], 0);
  if (!CONSTANT_ADDRESS_P (addr))
    XEXP (operands[1], 0) = force_reg (Pmode, addr);
}")

(define_insn "*call_value"
  [(set (match_operand 0 "register_operand" "=r,r")
	(call (mem:SI (match_operand:SI 1 "general_operand" "r,i"))
	      (match_operand 2 "" "")))
   (clobber (reg:SI LR_REGNUM))]
  ""
  "@
   l.jalr\t%1
   l.jal\t%1")

;; -------------------------------------------------------------------------
;; Prologue & Epilogue
;; -------------------------------------------------------------------------
(define_expand "prologue"
  [(const_int 1)]
  ""
  "
{
  or1k_expand_prologue ();
  DONE;
}")

;; Expand epilogue as RTL
(define_expand "epilogue"
  [(return)]
  ""
  "
{
  or1k_expand_epilogue ();
  DONE;
}")

(define_insn "return_internal"
  [(use (match_operand:SI 0 "register_operand" "r"))
   (return)]
  ""
  "l.jr\t%0")

