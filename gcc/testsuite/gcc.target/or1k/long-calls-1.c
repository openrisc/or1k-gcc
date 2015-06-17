/* Check that long value calls are not optimized to "l.jal" */
/* { dg-do compile { target or1k-*-* } } */
/* { dg-options "-O2" } */
/* This test expects that short calls are the default.  */
/* { dg-skip-if "-mlong-calls in use" { "*-*-*" } { "-mlong-calls" } { "" } } */

#define attr_weak __attribute__((weak))
#define attr_noinline __attribute__((noinline))
#define attr_long_call __attribute__((long_call))
#define attr_far __attribute__((far))
#define attr_near __attribute__((near))

#define REMOTE_CALL(ID, TARGET_ATTRS)				\
  const char *TARGET_ATTRS ID (void);						\
  const char *call_##ID (void) { return ID () + 1; }

#define EXTERN_CALL(ID, TARGET_ATTRS)				\
  const char *TARGET_ATTRS attr_noinline ID (void) { return #ID; }		\
  const char *call_##ID (void) { return ID () + 1; }

#define STATIC_CALL(ID, TARGET_ATTRS)				\
  static const char *TARGET_ATTRS attr_noinline ID (void) { return #ID; }	\
  const char *call_##ID (void) { return ID () + 1; }

#define DO_TESTS_CALL_ATTR(ID, TEST, TARGET_ATTRS)				\
  TEST (ID##none, TARGET_ATTRS)				\
  TEST (ID##long, TARGET_ATTRS attr_long_call)		\
  TEST (ID##far, TARGET_ATTRS attr_far)			\
  TEST (ID##near, TARGET_ATTRS attr_near)

DO_TESTS_CALL_ATTR (remote_, REMOTE_CALL,)
DO_TESTS_CALL_ATTR (strong_, EXTERN_CALL,)
DO_TESTS_CALL_ATTR (weak_, EXTERN_CALL, attr_weak)
DO_TESTS_CALL_ATTR (static_, STATIC_CALL,)


/* Calls to functions should honor the call type attribute,
 * with "short" being the default.
 * 
 * TODO:
 * TCL doesn't allow back references in look ahead patterns needed for
 * register checks.
 * "\[\\t \]+l\\.movhi\[\\t \]+(r\\d+),hi\\(strong_long\\)\[^\\n\]*((?!\\1).)*\[\\t \]+l.ori\[\\t \]+\\1,\\1,lo\\(strong_long\\)\[^\\n\]*((?!\\1).)*\[\\t \]+l.jalr\[\\t \]+\\1"
 * 
 * Instead the test currently only checks for the absence of corresponding l.jal
 */

/* { dg-final { scan-assembler "\[\\t \]+l\\.jal\[\\t \]+remote_none" } } */
/* { dg-final { scan-assembler-not "\[\\t \]+l\\.jal\[\\t \]+remote_long" } } */
/* { dg-final { scan-assembler-not "\[\\t \]+l\\.jal\[\\t \]+remote_far" } } */
/* { dg-final { scan-assembler "\[\\t \]+l\\.jal\[\\t \]+remote_near" } } */

/* { dg-final { scan-assembler "\[\\t \]+l\\.jal\[\\t \]+strong_none" } } */
/* { dg-final { scan-assembler-not "\[\\t \]+l\\.jal\[\\t \]+strong_long" } } */
/* { dg-final { scan-assembler-not "\[\\t \]+l\\.jal\[\\t \]+strong_far" } } */
/* { dg-final { scan-assembler "\[\\t \]+l\\.jal\[\\t \]+strong_near" } } */

/* { dg-final { scan-assembler "\[\\t \]+l\\.jal\[\\t \]+weak_none" } } */
/* { dg-final { scan-assembler-not "\[\\t \]+l\\.jal\[\\t \]+weak_long" } } */
/* { dg-final { scan-assembler-not "\[\\t \]+l\\.jal\[\\t \]+weak_far" } } */
/* { dg-final { scan-assembler "\[\\t \]+l\\.jal\[\\t \]+weak_near" } } */

/* { dg-final { scan-assembler "\[\\t \]+l\\.jal\[\\t \]+static_none" } } */
/* { dg-final { scan-assembler-not "\[\\t \]+l\\.jal\[\\t \]+static_long" } } */
/* { dg-final { scan-assembler-not "\[\\t \]+l\\.jal\[\\t \]+static_far" } } */
/* { dg-final { scan-assembler "\[\\t \]+l\\.jal\[\\t \]+static_near" } } */
