;; Predicate definitions for OpenRISC
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
;; Predicates
;; -------------------------------------------------------------------------

(define_predicate "input_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "memory_operand")
       (and (match_code "const_int")
	    (match_test "satisfies_constraint_I (op)
			 || satisfies_constraint_K (op)
			 || satisfies_constraint_M (op)"))))

(define_predicate "const0_operand"
  (and (match_code "const_int,const_wide_int,const_double,const_vector")
       (match_test "op == CONST0_RTX (mode)")))

(define_predicate "reg_or_0_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const0_operand")))

(define_predicate "reg_or_u6_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) >= 0 && INTVAL (op) <= 0x3f")
    (match_operand 0 "register_operand")))

(define_predicate "reg_or_u16_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) >= 0 && INTVAL (op) <= 0xffff")
    (match_operand 0 "register_operand")))

(define_predicate "reg_or_s16_operand"
  (if_then_else (match_code "const_int")
    (match_test "INTVAL (op) >= -32768 && INTVAL (op) <= 32767")
    (match_operand 0 "register_operand")))

(define_predicate "call_insn_operand"
  (ior (match_code "symbol_ref")
       (match_operand 0 "register_operand")))
