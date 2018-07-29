#!/bin/bash

# Runs the test suite for the travis build
#
# If CHECK_FORMAT is true, then we run the mix formatter to ensure that
# all files are correctly formatted.
# If COVERALLS is true, then we report test coverage to coveralls.
#
# This script could be used for local testing as long as COVERALLS is not set.

export MIX_ENV=test

if [ "$CHECK_FORMAT" = true ]
then
  echo "Checking that files are formatted"
  mix format --check-formatted
fi

if [ "$COVERALLS" = true ]
then
  echo "Coveralls will be reported"
  TEST_COMMAND=coveralls.travis
else
  TEST_COMMAND=test
fi

mix "$TEST_COMMAND"
