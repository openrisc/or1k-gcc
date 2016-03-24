;; Atomic operations for GNU compiler, OpenRISC 1000 family, OR32 ISA
;; Copyright (C) 2016 Free Software Foundation, Inc.
;;
;; This file is part of GNU CC.
;;
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

;; Note that MULT stands in for the non-existant NAND rtx_code.
(define_code_iterator FETCHOP [plus minus ior xor and mult])

(define_code_attr fetchop_name
  [(plus "add")
   (minus "sub")
   (ior "or")
   (xor "xor")
   (and "and")
   (mult "nand")])

(define_code_attr fetchop_pred
  [(plus "reg_or_s16_operand")
   (minus "register_operand")
   (ior "reg_or_u16_operand")
   (xor "reg_or_s16_operand")
   (and "reg_or_u16_operand")
   (mult "reg_or_u16_operand")])

(define_expand "mem_thread_fence"
  [(match_operand:SI 0 "const_int_operand" "")]		;; model
  ""
{
  memmodel model = memmodel_base (INTVAL (operands[0]));
  if (model != MEMMODEL_RELAXED)
    emit_insn (gen_msync ());
  DONE;
})

(define_expand "msync"
  [(set (match_dup 0) (unspec:BLK [(match_dup 0)] UNSPEC_MSYNC))]
  ""
{
  operands[0] = gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode));
  MEM_VOLATILE_P (operands[0]) = 1;
})

(define_insn "*msync"
  [(set (match_operand:BLK 0 "" "")
	(unspec:BLK [(match_dup 0)] UNSPEC_MSYNC))]
  ""
  "l.msync"
  [(set_attr "type" "logic")])

(define_insn "load_locked_si"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec_volatile:SI
	  [(match_operand:SI 1 "memory_operand" "m")] UNSPECV_LL))]
  ""
  "l.lwa\t%0,%1"
  [(set_attr "type" "load")])

(define_insn "store_conditional_si"
  [(set (reg:BI SR_F_REG)
	(unspec_volatile:BI [(const_int 0)] UNSPECV_SC))
   (set (match_operand:SI 0 "memory_operand" "=m")
	(match_operand:SI 1 "reg_or_0_operand" "rO"))]
  ""
  "l.swa\t%0,%r1"
  [(set_attr "type" "store")])

(define_expand "atomic_compare_and_swapsi"
  [(match_operand:SI 0 "register_operand")   ;; bool output
   (match_operand:SI 1 "register_operand")   ;; val output
   (match_operand:SI 2 "memory_operand")     ;; memory
   (match_operand:SI 3 "reg_or_s16_operand") ;; expected
   (match_operand:SI 4 "reg_or_0_operand")   ;; desired
   (match_operand:SI 5 "const_int_operand")  ;; is_weak
   (match_operand:SI 6 "const_int_operand")  ;; mod_s
   (match_operand:SI 7 "const_int_operand")] ;; mod_f
  ""
{
  or1k_expand_atomic_compare_and_swap (operands);
  DONE;
})

(define_expand "atomic_compare_and_swap<mode>"
  [(match_operand:SI 0 "register_operand")   ;; bool output
   (match_operand:I12 1 "register_operand")  ;; val output
   (match_operand:I12 2 "memory_operand")    ;; memory
   (match_operand:I12 3 "register_operand")  ;; expected
   (match_operand:I12 4 "reg_or_0_operand")  ;; desired
   (match_operand:SI 5 "const_int_operand")  ;; is_weak
   (match_operand:SI 6 "const_int_operand")  ;; mod_s
   (match_operand:SI 7 "const_int_operand")] ;; mod_f
  ""
{
  or1k_expand_atomic_compare_and_swap_qihi (operands);
  DONE;
})

(define_expand "atomic_exchangesi"
  [(match_operand:SI 0 "register_operand")	;; output
   (match_operand:SI 1 "memory_operand")	;; memory
   (match_operand:SI 2 "reg_or_0_operand")	;; input
   (match_operand:SI 3 "const_int_operand")]	;; model
  ""
{
  or1k_expand_atomic_exchange (operands);
  DONE;
})

(define_expand "atomic_exchange<mode>"
  [(match_operand:I12 0 "register_operand")	;; output
   (match_operand:I12 1 "memory_operand")	;; memory
   (match_operand:I12 2 "reg_or_0_operand")	;; input
   (match_operand:SI 3 "const_int_operand")]	;; model
  ""
{
  or1k_expand_atomic_exchange_qihi (operands);
  DONE;
})

(define_expand "atomic_<fetchop_name>si"
  [(match_operand:SI 0 "memory_operand")	;; memory
   (FETCHOP:SI (match_dup 0)
     (match_operand:SI 1 "<fetchop_pred>"))	;; operand
   (match_operand:SI 2 "const_int_operand")]	;; model
  ""
{
  or1k_expand_atomic_op (<CODE>, operands[0], operands[1], NULL, NULL);
  DONE;
})

(define_expand "atomic_<fetchop_name><mode>"
  [(match_operand:I12 0 "memory_operand")	;; memory
   (FETCHOP:I12 (match_dup 0)
     (match_operand:I12 1 "register_operand"))	;; operand
   (match_operand:SI 2 "const_int_operand")]	;; model
  ""
{
  or1k_expand_atomic_op_qihi (<CODE>, operands[0], operands[1], NULL, NULL);
  DONE;
})

(define_expand "atomic_fetch_<fetchop_name>si"
  [(match_operand:SI 0 "register_operand" "")		;; output
   (match_operand:SI 1 "memory_operand" "")		;; memory
   (FETCHOP:SI (match_dup 1)
     (match_operand:SI 2 "<fetchop_pred>" ""))		;; operand
   (match_operand:SI 3 "const_int_operand" "")]		;; model
  ""
{
  or1k_expand_atomic_op (<CODE>, operands[1], operands[2], operands[0], NULL);
  DONE;
})

(define_expand "atomic_fetch_<fetchop_name><mode>"
  [(match_operand:I12 0 "register_operand" "")		;; output
   (match_operand:I12 1 "memory_operand" "")		;; memory
   (FETCHOP:I12 (match_dup 1)
     (match_operand:I12 2 "<fetchop_pred>" ""))		;; operand
   (match_operand:SI 3 "const_int_operand" "")]		;; model
  ""
{
  or1k_expand_atomic_op_qihi (<CODE>, operands[1], operands[2],
			      operands[0], NULL);
  DONE;
})

(define_expand "atomic_<fetchop_name>_fetchsi"
  [(match_operand:SI 0 "register_operand" "")		;; output
   (match_operand:SI 1 "memory_operand" "")		;; memory
   (FETCHOP:SI (match_dup 1)
     (match_operand:SI 2 "<fetchop_pred>" ""))		;; operand
   (match_operand:SI 3 "const_int_operand" "")]		;; model
  ""
{
  or1k_expand_atomic_op (<CODE>, operands[1], operands[2], NULL, operands[0]);
  DONE;
})

(define_expand "atomic_<fetchop_name>_fetch<mode>"
  [(match_operand:I12 0 "register_operand" "")		;; output
   (match_operand:I12 1 "memory_operand" "")		;; memory
   (FETCHOP:I12 (match_dup 1)
     (match_operand:I12 2 "<fetchop_pred>" ""))	;; operand
   (match_operand:SI 3 "const_int_operand" "")]		;; model
  ""
{
  or1k_expand_atomic_op_qihi (<CODE>, operands[1], operands[2],
			      NULL, operands[0]);
  DONE;
})
