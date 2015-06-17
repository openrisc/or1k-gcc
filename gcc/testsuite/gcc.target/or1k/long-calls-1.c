/* Check that long calls are not optimized to "l.jal" */
/* { dg-do compile { target or1k-*-* } } */
/* { dg-options "-O2" } */
/* This test expects that short calls are the default.  */
/* { dg-skip-if "-mlong-calls in use" { "*-*-*" } { "-mlong-calls" } { "" } } */

#define section(S) __attribute__((section(S)))
#define attr_weak __attribute__((weak))
#define attr_noinline __attribute__((noinline))
#define attr_long_call __attribute__((long_call))
#define attr_far __attribute__((far))
#define attr_near __attribute__((near))

#define REMOTE_CALL(ID, TARGET_ATTRS, CALL_ATTRS)				\
  const char *TARGET_ATTRS ID (void);						\
  const char *CALL_ATTRS call_##ID (void) { return ID () + 1; }

#define EXTERN_CALL(ID, TARGET_ATTRS, CALL_ATTRS)				\
  const char *TARGET_ATTRS attr_noinline ID (void) { return #ID; }		\
  const char *CALL_ATTRS call_##ID (void) { return ID () + 1; }

#define STATIC_CALL(ID, TARGET_ATTRS, CALL_ATTRS)				\
  static const char *TARGET_ATTRS attr_noinline ID (void) { return #ID; }	\
  const char *CALL_ATTRS call_##ID (void) { return ID () + 1; }

#define DO_TESTS_SECTION(ID, TEST, TARGET_ATTRS)				\
  TEST (ID##1, TARGET_ATTRS, )	/* ignore section tests
  TEST (ID##2, TARGET_ATTRS section (".test.a"), section (".test.b"))		\
  TEST (ID##3, TARGET_ATTRS section (".test.c"), section (".test.c"))*/

#define DO_TESTS_CALL_ATTR(ID, TEST, TARGET_ATTRS)				\
  DO_TESTS_SECTION (ID##none, TEST, TARGET_ATTRS)				\
  DO_TESTS_SECTION (ID##long, TEST, TARGET_ATTRS attr_long_call)		\
  DO_TESTS_SECTION (ID##far, TEST, TARGET_ATTRS attr_far)			\
  DO_TESTS_SECTION (ID##near, TEST, TARGET_ATTRS attr_near)

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
 * We could hardcode the jump register like this
 * "\[\\t \]+l\\.movhi\[\\t \]+r11,hi\\(strong_long1\\)\[^\\n\]*((?!r11).)*\[\\t \]+l.ori\[\\t \]+r11,r11,lo\\(strong_long1\\)\[^\\n\]*((?!r11).)*\[\\t \]+l.jalr\[\\t \]+r11"
 * but instead the test currently only checks for the absence of l.jal
 */

/* { dg-final { scan-assembler "\[\\t \]+l\\.jal\[\\t \]+remote_none1" } } */
/* { dg-final { scan-assembler-not "\[\\t \]+l\\.jal\[\\t \]+remote_long1" } } */
/* { dg-final { scan-assembler-not "\[\\t \]+l\\.jal\[\\t \]+remote_far1" } } */
/* { dg-final { scan-assembler "\[\\t \]+l\\.jal\[\\t \]+remote_near1" } } */

/* { dg-final { scan-assembler "\[\\t \]+l\\.jal\[\\t \]+strong_none1" } } */
/* { dg-final { scan-assembler-not "\[\\t \]+l\\.jal\[\\t \]+strong_long1" } } */
/* { dg-final { scan-assembler-not "\[\\t \]+l\\.jal\[\\t \]+strong_far1" } } */
/* { dg-final { scan-assembler "\[\\t \]+l\\.jal\[\\t \]+strong_near1" } } */

/* { dg-final { scan-assembler "\[\\t \]+l\\.jal\[\\t \]+weak_none1" } } */
/* { dg-final { scan-assembler-not "\[\\t \]+l\\.jal\[\\t \]+weak_long1" } } */
/* { dg-final { scan-assembler-not "\[\\t \]+l\\.jal\[\\t \]+weak_far1" } } */
/* { dg-final { scan-assembler "\[\\t \]+l\\.jal\[\\t \]+weak_near1" } } */

/* { dg-final { scan-assembler "\[\\t \]+l\\.jal\[\\t \]+static_none1" } } */
/* { dg-final { scan-assembler-not "\[\\t \]+l\\.jal\[\\t \]+static_long1" } } */
/* { dg-final { scan-assembler-not "\[\\t \]+l\\.jal\[\\t \]+static_far1" } } */
/* { dg-final { scan-assembler "\[\\t \]+l\\.jal\[\\t \]+static_near1" } } */
