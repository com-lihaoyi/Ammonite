#!/usr/bin/env bash

echo "import \$ivy.\`com.lihaoyi::mill-contrib-bloop:0.4.1\`" > /tmp/setup.sc
mill --predef /tmp/setup.sc mill.contrib.Bloop/install

find .bloop -type f -name '*.json' | grep -v 2.12.8 | xargs rm


