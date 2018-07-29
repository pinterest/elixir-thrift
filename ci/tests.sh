#!/bin/bash

# Runs the test suite for the travis build
#
# If EXTRA_CI_CHECKS is true, then we report test coverage to coveralls, and run
# the mix formatter to ensure all files are formatted.
#
# This script could be used for local testing as long as EXTRA_CI_CHECKS is not set.

export MIX_ENV=test

if [ "$EXTRA_CI_CHECKS" = true  ]
then
  echo "Checking that all code is formatted"
  mix format --check-formatted
  echo "Coveralls will be reported"
  mix coveralls.travis
else
  mix test
fi
