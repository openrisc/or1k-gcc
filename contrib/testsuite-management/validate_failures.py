#!/usr/bin/python

# Script to compare testsuite failures against a list of known-to-fail
# tests.

# Contributed by Diego Novillo <dnovillo@google.com>
#
# Copyright (C) 2011, 2012 Free Software Foundation, Inc.
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING.  If not, write to
# the Free Software Foundation, 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

"""This script provides a coarser XFAILing mechanism that requires no
detailed DejaGNU markings.  This is useful in a variety of scenarios:

- Development branches with many known failures waiting to be fixed.
- Release branches with known failures that are not considered
  important for the particular release criteria used in that branch.

The script must be executed from the toplevel build directory.  When
executed it will:

1- Determine the target built: TARGET
2- Determine the source directory: SRCDIR
3- Look for a failure manifest file in
   <SRCDIR>/contrib/testsuite-management/<TARGET>.xfail
4- Collect all the <tool>.sum files from the build tree.
5- Produce a report stating:
   a- Failures expected in the manifest but not present in the build.
   b- Failures in the build not expected in the manifest.
6- If all the build failures are expected in the manifest, it exits
   with exit code 0.  Otherwise, it exits with error code 1.
"""

import datetime
import optparse
import os
import re
import sys

# Handled test results.
_VALID_TEST_RESULTS = [ 'FAIL', 'UNRESOLVED', 'XPASS', 'ERROR' ]

# Pattern for naming manifest files.  The first argument should be
# the toplevel GCC source directory.  The second argument is the
# target triple used during the build.
_MANIFEST_PATH_PATTERN = '%s/contrib/testsuite-management/%s.xfail'

def Error(msg):
  print >>sys.stderr, '\nerror: %s' % msg
  sys.exit(1)


class TestResult(object):
  """Describes a single DejaGNU test result as emitted in .sum files.

  We are only interested in representing unsuccessful tests.  So, only
  a subset of all the tests are loaded.

  The summary line used to build the test result should have this format:

  attrlist | XPASS: gcc.dg/unroll_1.c (test for excess errors)
  ^^^^^^^^   ^^^^^  ^^^^^^^^^^^^^^^^^ ^^^^^^^^^^^^^^^^^^^^^^^^
  optional   state  name              description
  attributes

  Attributes:
    attrlist: A comma separated list of attributes.
      Valid values:
        flaky            Indicates that this test may not always fail.  These
                         tests are reported, but their presence does not affect
                         the results.

        expire=YYYYMMDD  After this date, this test will produce an error
                         whether it is in the manifest or not.

    state: One of UNRESOLVED, XPASS or FAIL.
    name: File name for the test.
    description: String describing the test (flags used, dejagnu message, etc)
    ordinal: Monotonically increasing integer.
             It is used to keep results for one .exp file sorted
             by the order the tests were run.
  """

  def __init__(self, summary_line, ordinal=-1):
    try:
      self.attrs = ''
      if '|' in summary_line:
        (self.attrs, summary_line) = summary_line.split('|', 1)
      try:
        (self.state,
         self.name,
         self.description) = re.match(r' *([A-Z]+):\s*(\S+)\s+(.*)',
                                      summary_line).groups()
      except:
        print 'Failed to parse summary line: "%s"' % summary_line
        raise
      self.attrs = self.attrs.strip()
      self.state = self.state.strip()
      self.description = self.description.strip()
      self.ordinal = ordinal
    except ValueError:
      Error('Cannot parse summary line "%s"' % summary_line)

    if self.state not in _VALID_TEST_RESULTS:
      Error('Invalid test result %s in "%s" (parsed as "%s")' % (
            self.state, summary_line, self))

  def __lt__(self, other):
    return (self.name < other.name or
            (self.name == other.name and self.ordinal < other.ordinal))

  def __hash__(self):
    return hash(self.state) ^ hash(self.name) ^ hash(self.description)

  def __eq__(self, other):
    return (self.state == other.state and
            self.name == other.name and
            self.description == other.description)

  def __ne__(self, other):
    return not (self == other)

  def __str__(self):
    attrs = ''
    if self.attrs:
      attrs = '%s | ' % self.attrs
    return '%s%s: %s %s' % (attrs, self.state, self.name, self.description)

  def ExpirationDate(self):
    # Return a datetime.date object with the expiration date for this
    # test result.  Return None, if no expiration has been set.
    if re.search(r'expire=', self.attrs):
      expiration = re.search(r'expire=(\d\d\d\d)(\d\d)(\d\d)', self.attrs)
      if not expiration:
        Error('Invalid expire= format in "%s".  Must be of the form '
              '"expire=YYYYMMDD"' % self)
      return datetime.date(int(expiration.group(1)),
                           int(expiration.group(2)),
                           int(expiration.group(3)))
    return None

  def HasExpired(self):
    # Return True if the expiration date of this result has passed.
    expiration_date = self.ExpirationDate()
    if expiration_date:
      now = datetime.date.today()
      return now > expiration_date


def GetMakefileValue(makefile_name, value_name):
  if os.path.exists(makefile_name):
    makefile = open(makefile_name)
    for line in makefile:
      if line.startswith(value_name):
        (_, value) = line.split('=', 1)
        value = value.strip()
        makefile.close()
        return value
    makefile.close()
  return None


def ValidBuildDirectory(builddir, target):
  if (not os.path.exists(builddir) or
      not os.path.exists('%s/Makefile' % builddir) or
      (not os.path.exists('%s/build-%s' % (builddir, target)) and
       not os.path.exists('%s/%s' % (builddir, target)))):
    return False
  return True


def IsInterestingResult(line):
  """Return True if the given line is one of the summary lines we care about."""
  line = line.strip()
  if line.startswith('#'):
    return False
  if '|' in line:
    (_, line) = line.split('|', 1)
  line = line.strip()
  for result in _VALID_TEST_RESULTS:
    if line.startswith(result):
      return True
  return False


def ParseSummary(sum_fname):
  """Create a set of TestResult instances from the given summary file."""
  result_set = set()
  # ordinal is used when sorting the results so that tests within each
  # .exp file are kept sorted.
  ordinal=0
  sum_file = open(sum_fname)
  for line in sum_file:
    if IsInterestingResult(line):
      result = TestResult(line, ordinal)
      ordinal += 1
      if result.HasExpired():
        # Tests that have expired are not added to the set of expected
        # results. If they are still present in the set of actual results,
        # they will cause an error to be reported.
        print 'WARNING: Expected failure "%s" has expired.' % line.strip()
        continue
      result_set.add(result)
  sum_file.close()
  return result_set


def GetManifest(manifest_name):
  """Build a set of expected failures from the manifest file.

  Each entry in the manifest file should have the format understood
  by the TestResult constructor.

  If no manifest file exists for this target, it returns an empty set.
  """
  if os.path.exists(manifest_name):
    return ParseSummary(manifest_name)
  else:
    return set()


def CollectSumFiles(builddir):
  sum_files = []
  for root, dirs, files in os.walk(builddir):
    if '.svn' in dirs:
      dirs.remove('.svn')
    for fname in files:
      if fname.endswith('.sum'):
        sum_files.append(os.path.join(root, fname))
  return sum_files


def GetResults(sum_files):
  """Collect all the test results from the given .sum files."""
  build_results = set()
  for sum_fname in sum_files:
    print '\t%s' % sum_fname
    build_results |= ParseSummary(sum_fname)
  return build_results


def CompareResults(manifest, actual):
  """Compare sets of results and return two lists:
     - List of results present in ACTUAL but missing from MANIFEST.
     - List of results present in MANIFEST but missing from ACTUAL.
  """
  # Collect all the actual results not present in the manifest.
  # Results in this set will be reported as errors.
  actual_vs_manifest = set()
  for actual_result in actual:
    if actual_result not in manifest:
      actual_vs_manifest.add(actual_result)

  # Collect all the tests in the manifest that were not found
  # in the actual results.
  # Results in this set will be reported as warnings (since
  # they are expected failures that are not failing anymore).
  manifest_vs_actual = set()
  for expected_result in manifest:
    # Ignore tests marked flaky.
    if 'flaky' in expected_result.attrs:
      continue
    if expected_result not in actual:
      manifest_vs_actual.add(expected_result)

  return actual_vs_manifest, manifest_vs_actual


def GetBuildData(options):
  target = GetMakefileValue('%s/Makefile' % options.build_dir, 'target_alias=')
  srcdir = GetMakefileValue('%s/Makefile' % options.build_dir, 'srcdir =')
  if not ValidBuildDirectory(options.build_dir, target):
    Error('%s is not a valid GCC top level build directory.' %
          options.build_dir)
  print 'Source directory: %s' % srcdir
  print 'Build target:     %s' % target
  return srcdir, target, True


def PrintSummary(msg, summary):
  print '\n\n%s' % msg
  for result in sorted(summary):
    print result


def GetSumFiles(results, build_dir):
  if not results:
    print 'Getting actual results from build directory %s' % build_dir
    sum_files = CollectSumFiles(build_dir)
  else:
    print 'Getting actual results from user-provided results'
    sum_files = results.split()
  return sum_files


def PerformComparison(expected, actual, ignore_missing_failures):
  actual_vs_expected, expected_vs_actual = CompareResults(expected, actual)

  tests_ok = True
  if len(actual_vs_expected) > 0:
    PrintSummary('Unexpected results in this build (new failures)',
                 actual_vs_expected)
    tests_ok = False

  if not ignore_missing_failures and len(expected_vs_actual) > 0:
    PrintSummary('Expected results not present in this build (fixed tests)'
                 '\n\nNOTE: This is not a failure.  It just means that these '
                 'tests were expected\nto fail, but they worked in this '
                 'configuration.\n', expected_vs_actual)

  if tests_ok:
    print '\nSUCCESS: No unexpected failures.'

  return tests_ok


def CheckExpectedResults(options):
  if not options.manifest:
    (srcdir, target, valid_build) = GetBuildData(options)
    if not valid_build:
      return False
    manifest_name = _MANIFEST_PATH_PATTERN % (srcdir, target)
  else:
    manifest_name = options.manifest
    if not os.path.exists(manifest_name):
      Error('Manifest file %s does not exist.' % manifest_name)

  print 'Manifest:         %s' % manifest_name
  manifest = GetManifest(manifest_name)
  sum_files = GetSumFiles(options.results, options.build_dir)
  actual = GetResults(sum_files)

  if options.verbosity >= 1:
    PrintSummary('Tests expected to fail', manifest)
    PrintSummary('\nActual test results', actual)

  return PerformComparison(manifest, actual, options.ignore_missing_failures)


def ProduceManifest(options):
  (srcdir, target, valid_build) = GetBuildData(options)
  if not valid_build:
    return False

  manifest_name = _MANIFEST_PATH_PATTERN % (srcdir, target)
  if os.path.exists(manifest_name) and not options.force:
    Error('Manifest file %s already exists.\nUse --force to overwrite.' %
          manifest_name)

  sum_files = GetSumFiles(options.results, options.build_dir)
  actual = GetResults(sum_files)
  manifest_file = open(manifest_name, 'w')
  for result in sorted(actual):
    print result
    manifest_file.write('%s\n' % result)
  manifest_file.close()

  return True


def CompareBuilds(options):
  (srcdir, target, valid_build) = GetBuildData(options)
  if not valid_build:
    return False

  sum_files = GetSumFiles(options.results, options.build_dir)
  actual = GetResults(sum_files)

  clean_sum_files = GetSumFiles(None, options.clean_build)
  clean = GetResults(clean_sum_files)

  return PerformComparison(clean, actual, options.ignore_missing_failures)


def Main(argv):
  parser = optparse.OptionParser(usage=__doc__)

  # Keep the following list sorted by option name.
  parser.add_option('--build_dir', action='store', type='string',
                    dest='build_dir', default='.',
                    help='Build directory to check (default = .)')
  parser.add_option('--clean_build', action='store', type='string',
                    dest='clean_build', default=None,
                    help='Compare test results from this build against '
                    'those of another (clean) build.  Use this option '
                    'when comparing the test results of your patch versus '
                    'the test results of a clean build without your patch. '
                    'You must provide the path to the top directory of your '
                    'clean build.')
  parser.add_option('--force', action='store_true', dest='force',
                    default=False, help='When used with --produce_manifest, '
                    'it will overwrite an existing manifest file '
                    '(default = False)')
  parser.add_option('--ignore_missing_failures', action='store_true',
                    dest='ignore_missing_failures', default=False,
                    help='When a failure is expected in the manifest but '
                    'it is not found in the actual results, the script '
                    'produces a note alerting to this fact. This means '
                    'that the expected failure has been fixed, or '
                    'it did not run, or it may simply be flaky '
                    '(default = False)')
  parser.add_option('--manifest', action='store', type='string',
                    dest='manifest', default=None,
                    help='Name of the manifest file to use (default = '
                    'taken from contrib/testsuite-managment/<target_alias>.xfail)')
  parser.add_option('--produce_manifest', action='store_true',
                    dest='produce_manifest', default=False,
                    help='Produce the manifest for the current '
                    'build (default = False)')
  parser.add_option('--results', action='store', type='string',
                    dest='results', default=None, help='Space-separated list '
                    'of .sum files with the testing results to check. The '
                    'only content needed from these files are the lines '
                    'starting with FAIL, XPASS or UNRESOLVED (default = '
                    '.sum files collected from the build directory).')
  parser.add_option('--verbosity', action='store', dest='verbosity',
                    type='int', default=0, help='Verbosity level (default = 0)')
  (options, _) = parser.parse_args(argv[1:])

  if options.produce_manifest:
    retval = ProduceManifest(options)
  elif options.clean_build:
    retval = CompareBuilds(options)
  else:
    retval = CheckExpectedResults(options)

  if retval:
    return 0
  else:
    return 1


if __name__ == '__main__':
  retval = Main(sys.argv)
  sys.exit(retval)
