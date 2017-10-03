#!/bin/bash

# Runs the test suite for the travis build
#
# If COVERALLS is true, then we report test coverage to coveralls.
#
# This script could be used for local testing as long as COVERALLS is not set.

export MIX_ENV=test
export THRIFT=${TRAVIS_BUILD_DIR}/ci/thrift-docker

if [ "$COVERALLS" = true  ]
then
  echo "Coveralls will be reported"
  TEST_COMMAND=coveralls.travis
else
  TEST_COMMAND=test
fi

mix "$TEST_COMMAND"
