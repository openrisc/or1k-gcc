/* Target Code for OpenRISC
   Copyright (C) 2018 Free Software Foundation, Inc.
   Contributed by Stafford Horne based on other ports.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "stringpool.h"
#include "attribs.h"
#include "df.h"
#include "regs.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "diagnostic-core.h"
#include "output.h"
#include "stor-layout.h"
#include "varasm.h"
#include "calls.h"
#include "expr.h"
#include "builtins.h"

/* These 4 are needed to allow using satisfies_constraint_J.  */
#include "insn-config.h"
#include "recog.h"
#include "tm_p.h"
#include "tm-constrs.h"

/* This file should be included last.  */
#include "target-def.h"

int
or1k_initial_elimination_offset (int from ATTRIBUTE_UNUSED, int to ATTRIBUTE_UNUSED)
{
  return 0;
}


/* Worker function for TARGET_LEGITIMATE_ADDRESS_P.  */

bool
or1k_legitimate_address_p (machine_mode mode ATTRIBUTE_UNUSED,
			   rtx x, bool strict_p ATTRIBUTE_UNUSED)
{
  if (GET_CODE(x) == PLUS
      && REG_P (XEXP (x, 0))
      && satisfies_constraint_J (XEXP (x, 1)))
    return true;

  if (REG_P (x))
    return true;

  return false;
}

/* Worker function for TARGET_FUNCTION_VALUE.  */

static rtx
or1k_function_value (const_tree valtype,
		     const_tree fn_decl_or_type ATTRIBUTE_UNUSED,
		     bool outgoing ATTRIBUTE_UNUSED)
{
  /* TODO support 2 reg return values and return on stack?  */
  return gen_rtx_REG (TYPE_MODE (valtype), RV_REGNUM);
}


#undef TARGET_LEGITIMATE_ADDRESS_P
#define TARGET_LEGITIMATE_ADDRESS_P or1k_legitimate_address_p

#undef TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE or1k_function_value

struct gcc_target targetm = TARGET_INITIALIZER;

/* Enable when we need option_override TARGET_OPTION_OVERRIDE.  */
//#include "gt-or1k.h"
