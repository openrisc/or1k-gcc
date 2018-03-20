;; Constraint definitions for OpenRISC
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
;; Constraints
;; -------------------------------------------------------------------------

;; Memory
(define_constraint "W"
  "A register indirect memory operand."
  (and (match_code "mem")
       (match_test "or1k_legitimate_address_p (mode, op, false)")))

;; Immediates
(define_constraint "I"
  "The constant zerK"
  (and (match_code "const_int")
       (match_test "ival == 0")))

(define_constraint "J"
  "A signed 16-bit immediate in the range -32768 to 32767."
  (and (match_code "const_int")
       (match_test "IN_RANGE (ival, -32768, 32767)")))

(define_constraint "K"
  "A shifted signed 16-bit constant suitable for l.movhi."
  (and (match_code "const_int")
       (match_test "(ival & 0xffff) == 0
                    && (ival >> 31 == -1 || ival >> 31 == 0)")))
