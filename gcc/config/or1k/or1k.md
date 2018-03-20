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
	   (match_operand:SI 1 "register_operand" "0,0")
	   (match_operand:SI 2 "general_operand" "0,J")))]
  ""
  "@
  l.add\t%0, %1, %2
  l.addi\t%0, %1, %2")

(define_insn "subsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	  (minus:SI
	   (match_operand:SI 1 "register_operand" "0")
	   (match_operand:SI 2 "register_operand" "0")))]
  ""
  "@
  l.sub\t%0, %1 %2")

;; -------------------------------------------------------------------------
;; Unary arithmetic instructions
;; -------------------------------------------------------------------------

;; -------------------------------------------------------------------------
;; Logical operators
;; -------------------------------------------------------------------------

;; -------------------------------------------------------------------------
;; Move instructions
;; -------------------------------------------------------------------------

(define_expand "movsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
	(match_operand:SI 1 "general_operand" ""))]
  ""
{
  if (MEM_P (operands[0]))
    operands[1] = force_reg (SImode, operands[1]);
})

(define_insn "*movsi_internal"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r,r,W,r")
	(match_operand:SI 1 "general_operand" "r,I,J,K,r,W"))]
  "register_operand (operands[0], SImode) || register_operand (operands[1], SImode)"
  "@
   l.ori\t%0, r0, %1
   l.movhi\t%0, %1
   l.ori\t%0, r0, %1
   l.movhi\t%0, %1
   l.sw\t%0, %1
   l.lwz\t%0, %1
")


;; -------------------------------------------------------------------------
;; Compare instructions
;; -------------------------------------------------------------------------

;; -------------------------------------------------------------------------
;; Branch instructions
;; -------------------------------------------------------------------------

;; -------------------------------------------------------------------------
;; Call and Jump instructions
;; -------------------------------------------------------------------------

;; -------------------------------------------------------------------------
;; Prologue & Epilogue
;; -------------------------------------------------------------------------

