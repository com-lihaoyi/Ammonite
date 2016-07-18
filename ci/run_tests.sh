#!/usr/bin/env bash
set -euxv

sbt ++$TRAVIS_SCALA_VERSION published/compile
sbt ++$TRAVIS_SCALA_VERSION $SBT_COMMAND
