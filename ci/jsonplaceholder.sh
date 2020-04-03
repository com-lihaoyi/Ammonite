#!/usr/bin/env bash

PORT="4545"
export JSONPLACEHOLDER="http://localhost:$PORT"

npx jsonplaceholder -v
npx jsonplaceholder -p "$PORT" ci/jsonplaceholder-db.json &

unset PORT
