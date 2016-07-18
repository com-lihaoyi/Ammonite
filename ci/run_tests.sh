#!/usr/bin/env bash

sbt ++$TRAVIS_SCALA_VERSION published/compile && sbt ++$TRAVIS_SCALA_VERSION $SBT_COMMAND
