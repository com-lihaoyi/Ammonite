#!/usr/bin/env sh

# This is a wrapper script, that automatically download ammonite from GitHub release pages
# You can give the required ammonite version with AMM_VERSION env variable
# If no version is given, it falls back to the value of DEFAULT_AMM_VERSION
DEFAULT_AMM_VERSION=
DEFAULT_SCALA_VERSION=
set -e

if [ -z "$AMM_VERSION" ] ; then
  AMM_VERSION=$DEFAULT_AMM_VERSION
fi
if [ -z "$SCALA_VERSION" ] ; then
  SCALA_VERSION=$DEFAULT_SCALA_VERSION
fi

AMM_DOWNLOAD_PATH="$HOME/.ammonite/download"
AMM_EXEC_PATH="${AMM_DOWNLOAD_PATH}/${AMM_VERSION}_$SCALA_VERSION"

if [ ! -x "$AMM_EXEC_PATH" ] ; then
  mkdir -p $AMM_DOWNLOAD_PATH
  DOWNLOAD_FILE=$AMM_EXEC_PATH-tmp-download
  AMM_VERSION_TAG=$(echo $AMM_VERSION | sed -E 's/([^-]+)(-M[0-9]+)?(-.*)?/\1\2/')
  AMM_DOWNLOAD_URL="https://github.com/lihaoyi/ammonite/releases/download/${AMM_VERSION_TAG}/$SCALA_VERSION-$AMM_VERSION"
  curl --fail -L -o "$DOWNLOAD_FILE" "$AMM_DOWNLOAD_URL"
  chmod +x "$DOWNLOAD_FILE"
  mv "$DOWNLOAD_FILE" "$AMM_EXEC_PATH"
  unset DOWNLOAD_FILE
  unset AMM_DOWNLOAD_URL
fi

unset AMM_DOWNLOAD_PATH
unset AMM_VERSION

exec $AMM_EXEC_PATH "$@"
