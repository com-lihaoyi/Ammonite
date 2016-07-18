#!/usr/bin/env python
import os
import subprocess
import json

if os.environ['TRAVIS_PULL_REQUEST'] == "false" and os.environ['TRAVIS_BRANCH'] == "master":
    deploy_key = json.loads('"' + os.environ['DEPLOY_KEY'] + '"')
    with open("deploy_key", "w") as f:
        f.write(deploy_key)
    if os.environ.get("TRAVIS_TAG"):
        new_env = dict(os.environ, DOC_FOLDER=".")
    else:
        new_env = dict(os.environ, DOC_FOLDER="master")
    subprocess.check_call("ci/deploy_master_docs.sh", env=new_env)
else:
    print "Building docs"
    subprocess.check_call("ci/build_docs.sh")