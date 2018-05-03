/* Target Definitions for OpenRISC.
   Copyright (C) 2018 Free Software Foundation, Inc.
   Contributed by Stafford Horne.

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

#ifndef GCC_OR1K_H
#define GCC_OR1K_H

/* Names to predefine in the preprocessor for this target machine.  */
/* __M32R__ is defined by the existing compiler so we use that.  */
#define TARGET_CPU_CPP_BUILTINS()		\
  do						\
    {						\
      builtin_define ("__OR1K__");		\
      builtin_define ("__or1k__");		\
      builtin_assert ("cpu=or1k");		\
      builtin_assert ("machine=or1k");		\
      builtin_define ("__BIG_ENDIAN__");	\
    }						\
  while (0)

/* Storage layout.  */

#define DEFAULT_SIGNED_CHAR 1
#define BITS_BIG_ENDIAN 0
#define BYTES_BIG_ENDIAN 1
#define WORDS_BIG_ENDIAN 1
#define BITS_PER_WORD 32
#define UNITS_PER_WORD 4
#define POINTER_SIZE 32
#define BIGGEST_ALIGNMENT 32
#define STRICT_ALIGNMENT 1
#define FUNCTION_BOUNDARY 32
#define PARM_BOUNDARY 32
#define STACK_BOUNDARY 32
#define PREFERRED_STACK_BOUNDARY 32
#define MAX_FIXED_MODE_SIZE 64

#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT

/* Layout of source language data types.  */

#define INT_TYPE_SIZE 32
#define SHORT_TYPE_SIZE 16
#define LONG_TYPE_SIZE 32
#define LONG_LONG_TYPE_SIZE 64
#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 64

#undef SIZE_TYPE
#define SIZE_TYPE "unsigned int"

#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE "int"

/* Describing Relative Costs of Operations.  */
#define MOVE_MAX 4
#define SLOW_BYTE_ACCESS 1

/* Register usage, class and contents.  */

/* In OpenRISC there are 32 general purpose registers with the following
   designations:

   r0    always 0
   r1    stack pointer
   r2    frame pointer (optional)
   r3    arg 0
   r4    arg 1
   r5    arg 2
   r6    arg 3
   r7    arg 4
   r8    arg 5
   r9    function call return link address
   r10   thread local storage
   r11   function return value
   r12   function return value high (upper 64-bit)
   r13   temporary
   r14   callee saved
   r15   temporary
   r16   callee saved
   r17   position independent code base pointer
   r18   callee saved
   r19   temporary
   r20   callee saved
   r21   temporary
   r22   callee saved
   r23   temporary
   r24   callee saved
   r25   temporary
   r26   callee saved
   r27   temporary
   r28   callee saved
   r29   temporary
   r30   callee saved
   r31   temporary  */

#define ZERO_REGNUM   0
#define SP_REGNUM   1
#define FP_REGNUM   2
#define LR_REGNUM   9
#define TLS_REGNUM  10
#define RV_REGNUM   11
#define RVH_REGNUM  12
#define AP_REGNUM   32
#define CC_REGNUM   33
#define FIRST_PSEUDO_REGISTER  34

#define FIXED_REGISTERS   \
{ 1, 1, 0, 0, 0, 0, 0, 0, \
  0, 0, 1, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  1, 1}

#define CALL_USED_REGISTERS \
{ 1, 1, 0, 0, 0, 0, 0, 0, \
  0, 1, 1, 1, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  1, 1}

enum reg_class
{
  NO_REGS,
  GENERAL_REGS,
  SPECIAL_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES { \
  "NO_REGS", 			\
  "GENERAL_REGS",		\
  "SPECIAL_REGS",		\
  "ALL_REGS" }

#define REG_CLASS_CONTENTS      \
{ {0x00000000, 0x00000000},	\
  {0xffffffff, 0x00000000},	\
  {0x00000000, 0x00000003},	\
  {0xffffffff, 0x00000003}	\
}

/* A C expression whose value is a register class containing hard
   register REGNO.  In general there is more that one such class;
   choose a class which is "minimal", meaning that no smaller class
   also contains the register.  */
#define REGNO_REG_CLASS(REGNO) \
  ((REGNO >= FIRST_PSEUDO_REGISTER || REGNO < 0) ? NO_REGS :	\
   (REGNO == AP_REGNUM || 					\
    REGNO == CC_REGNUM ? SPECIAL_REGS : GENERAL_REGS))

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE)               \
do {                                                    \
  if (GET_MODE_CLASS (MODE) == MODE_INT                 \
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)         \
    (MODE) = word_mode;                                 \
} while (0)

/* A macro whose definition is the name of the class to which a valid
   base register must belong.  A base register is one used in an
   address which is the register value plus a displacement.  */
#define BASE_REG_CLASS GENERAL_REGS

#define INDEX_REG_CLASS NO_REGS

/* Assembly definitions.  */

#define ASM_APP_ON ""
#define ASM_APP_OFF ""

#define ASM_COMMENT_START "# "

#define GLOBAL_ASM_OP "\t.global\t"
#define TEXT_SECTION_ASM_OP "\t.section\t.text"
#define DATA_SECTION_ASM_OP "\t.section\t.data"
#define BSS_SECTION_ASM_OP "\t.section\t.bss"
#define SBSS_SECTION_ASM_OP "\t.section\t.sbss"

#define REGISTER_NAMES {						\
  "r0",   "r1",   "r2",   "r3",   "r4",   "r5",   "r6",   "r7",		\
  "r8",   "r9",   "r10",  "r11",  "r12",  "r13",  "r14",  "r15",	\
  "r16",  "r17",  "r18",  "r19",  "r20",  "r21",  "r22",  "r23",	\
  "r24",  "r25",  "r26",  "r27",  "r28",  "r29",  "r30",  "r31",	\
  "?ap",  "?cc" }

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */
#define ASM_OUTPUT_ALIGN(FILE,LOG)			\
  do							\
    {							\
      if ((LOG) != 0)					\
	fprintf (FILE, "\t.align %d\n", 1 << (LOG));	\
    }							\
  while (0)

#define PRINT_OPERAND_PUNCT_VALID_P(CODE) (code == '#')

/* Calling convention definitions.  */
#define CUMULATIVE_ARGS int
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  do { (CUM) = 0; } while (0)


/* Trampolines, for nested functions */
#define TRAMPOLINE_SIZE 12
#define TRAMPOLINE_ALIGNMENT (abort (), 0)

/* Pointer mode */
#define Pmode	SImode
#define FUNCTION_MODE	SImode
#define STACK_POINTER_REGNUM SP_REGNUM
#define FRAME_POINTER_REGNUM FP_REGNUM

/* The register number of the arg pointer register, which is used to
   access the function's argument list.  */
#define ARG_POINTER_REGNUM FRAME_POINTER_REGNUM

/* A C expression that is nonzero if REGNO is the number of a hard
   register in which function arguments are sometimes passed.  */
#define FUNCTION_ARG_REGNO_P(r) (r >= 3 && r <= 8)

#define MAX_REGS_PER_ADDRESS 1

/* The ELIMINABLE_REGS macro specifies a table of register pairs used to
   eliminate unneeded registers that point into the stack frame. Note,
   the only elimination attempted by the compiler is to replace references
   to the frame pointer with references to the stack pointer.  */

#define ELIMINABLE_REGS					\
{{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM },	\
 { ARG_POINTER_REGNUM,	 STACK_POINTER_REGNUM }}

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) \
  do {							\
    (OFFSET) = or1k_initial_elimination_offset ((FROM), (TO)); \
  } while (0)

#define REGNO_OK_FOR_INDEX_P(REGNO) 0
#define REGNO_OK_FOR_BASE_P(REGNO) \
  ((REGNO) < FIRST_PSEUDO_REGISTER			\
   || ((unsigned int) reg_renumber[REGNO]) < FIRST_PSEUDO_REGISTER)

/* If defined, the maximum amount of space required for outgoing
   arguments will be computed and placed into the variable
   'crtl->outgoing_args_size'.  No space will be pushed
   onto the stack for each call; instead, the function prologue
   should increase the stack frame size by this amount.  */
#define ACCUMULATE_OUTGOING_ARGS 0

/* Offset from the argument pointer register to the first argument's
   address.  On some machines it may depend on the data type of the
   function.  If 'ARGS_GROW_DOWNWARD', this is the offset to the
   location above the first argument's address.  */
#define FIRST_PARM_OFFSET(FNDECL) (UNITS_PER_WORD)

/* Stack layout and stack pointer usage.  */

/* Define this macro if pushing a word onto the stack moves the stack
   pointer to a smaller address.  */
#define STACK_GROWS_DOWNWARD 1

/* Offset from the stack pointer register to the first location at which
   outgoing arguments are placed.  */
#define STACK_POINTER_OFFSET (UNITS_PER_WORD)

/* An alias for a machine mode name.  This is the machine mode that
   elements of a jump-table should have.  */
#define CASE_VECTOR_MODE SImode

#define STORE_FLAG_VALUE 1

/* Indicates how loads of narrow mode values are loaded into words.  */
#define LOAD_EXTEND_OP(MODE) (ZERO_EXTEND)

/* Profiling */
#define FUNCTION_PROFILER(FILE,LABELNO) (abort (), 0)

/* Dwarf 2 Support */
#define DWARF2_DEBUGGING_INFO 1
#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (Pmode, LR_REGNUM)
#define DWARF_FRAME_RETURN_COLUMN LR_REGNUM

#endif /* GCC_OR1K_H */
